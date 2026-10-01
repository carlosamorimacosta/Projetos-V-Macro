# =============================================================================
# PROJETO V – ITAÚ / SFN
# SVAR RECURSIVO: BETS, ATIVIDADE, IPCA, SELIC E INADIMPLÊNCIA PF ATÉ 10 SM
# =============================================================================
#
# MODELO PRINCIPAL:
#
#   Y_t = [ BETS_t, IBC_t, IPCA_t, SELIC_t, INAD_10SM_t ]'
#
# em que, por padrão:
#   BETS  = 100 * Delta log do GGR real mensal das bets
#   IBC   = 100 * Delta log(IBC-Br com ajuste sazonal)
#   IPCA  = inflação mensal (%)
#   SELIC = Delta da Meta Selic vigente no fim de cada mês (p.p.)
#   INAD  = Delta da inadimplência PF até 10 salários mínimos (p.p.)
#
# IDENTIFICAÇÃO PRINCIPAL:
#   BETS -> IBC -> IPCA -> SELIC -> INAD
#
# ROBUSTEZ:
#   ordem alternativa: IBC -> IPCA -> SELIC -> BETS -> INAD
#
# IMPORTANTE:
# - O GGR nacional NÃO é uma medida direta da exposição das famílias <= 10 SM.
# - A base de Bets 2021-2025 contém valores estimados/construídos; isso deve ser
#   explicitado na interpretação dos resultados.
# - Como a amostra com Bets é curta, o código limita automaticamente o número
#   admissível de defasagens e usa SC/BIC como critério-base.
# - A ordenação de Cholesky é uma hipótese de identificação. Por isso há uma
#   ordem alternativa de robustez.
# - O código testa estacionariedade. Se log(Bets), Selic ou inadimplência forem
#   claramente não estacionários, considere as opções "dlog"/"diff" abaixo.
# =============================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260930)

# =============================================================================
# 0. CONFIGURAÇÕES
# =============================================================================

DATA_INICIO_DESEJADO <- as.Date("2020-01-01")
DATA_FIM_DESEJADO    <- as.Date("2026-07-31")

# Horizonte das IRFs/FEVD
H_IRF <- 24

# Intervalo de confiança das IRFs
CI_IRF <- 0.90

# Bootstrap das IRFs
BOOT_RUNS <- 2000

# Máximo solicitado. O código reduz este valor se a amostra for pequena.
LAG_MAX_USUARIO <- 4

# Critério principal de lag:
# "AIC(n)", "HQ(n)", "SC(n)" ou "FPE(n)"
CRITERIO_P <- "SC(n)"

# Opcional: force uma defasagem específica (1, 2, 3, ...).
# Deixe NULL para usar automaticamente o critério acima.
P_FORCADO <- 2


# Regra de parcimônia aproximada por equação:
MIN_OBS_POR_PARAM <- 3

# Dummies mensais custam 11 parâmetros por equação.
# Como a amostra com Bets é curta, o padrão é FALSE.
USAR_DUMMIES_SAZONAIS <- FALSE

# -----------------------------------------------------------------------------
# Transformações
# -----------------------------------------------------------------------------

# Especificação final adotada após os testes de estacionariedade:
#   Bets  = 100 * Delta log(GGR real)
#   Selic = Delta da Meta Selic de fim de mês, em p.p.
#   Inad  = Delta da inadimplência, em p.p.
# IBC-Br entra sempre como 100 * Delta log(IBC).
# IPCA entra como inflação mensal.

TRANSFORM_BETS  <- "dlog"   # "log" ou "dlog"
TRANSFORM_SELIC <- "diff"   # "nivel" ou "diff"
TRANSFORM_INAD  <- "diff"   # "nivel" ou "diff"

# Robustez de identificação
RODAR_ORDEM_ALTERNATIVA <- TRUE

# -----------------------------------------------------------------------------
# Caminhos
# -----------------------------------------------------------------------------

arquivo_inad_10sm <- paste0(
  "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/",
  "Inadimplência 10sm/Base_Final_Inadimplencia_PF.xlsx"
)

# NOVO: Selic Meta utilizada diretamente no SVAR.
arquivo_selic <- paste0(
  "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/",
  "Taxa de juros/Selic Meta.csv"
)

arquivo_ipca <- paste0(
  "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/",
  "ipca_202606SerieHist.xls"
)

arquivo_bets <- paste0(
  "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/",
  "Bets/Bets_GGR_Mensal_2021_2025_estimado.xlsx"
)

aba_inad_10sm <- "Base Mensal"
aba_ipca      <- 1
aba_bets      <- "Base_mensal"

# IBC-Br com ajuste sazonal
SGS_IBC <- 24364

DIR_OUT <- "C:/Users/carlo/Downloads/output_SVAR_Itau_Selic"

if (!dir.exists(DIR_OUT)) {
  dir.create(DIR_OUT, recursive = TRUE)
}
# =============================================================================
# 1. PACOTES
# =============================================================================

pacotes <- c(
  "tidyverse",
  "lubridate",
  "zoo",
  "readxl",
  "readr",
  "rbcb",
  "vars",
  "tseries",
  "urca",
  "lmtest",
  "ggplot2",
  "openxlsx"
)

instalar_ausentes <- function(pkgs) {
  ausentes <- pkgs[
    !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(ausentes) > 0) {
    install.packages(ausentes, dependencies = TRUE)
  }
}

instalar_ausentes(pacotes)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(readxl)
  library(readr)
  library(rbcb)
  library(vars)
  library(tseries)
  library(urca)
  library(lmtest)
  library(ggplot2)
  library(openxlsx)
})

# =============================================================================
# 2. FUNÇÕES DE IMPORTAÇÃO – ADAPTADAS DO CÓDIGO ARDL
# =============================================================================

normalizar_nome <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

parse_numero <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "NaN", "-", "--", "...", "null", "NULL")] <- NA_character_

  usa_virgula <- mean(grepl(",", s), na.rm = TRUE) > 0.20
  if (is.na(usa_virgula)) usa_virgula <- FALSE

  if (usa_virgula) {
    out <- readr::parse_number(
      s,
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    )
  } else {
    out <- readr::parse_number(
      s,
      locale = readr::locale(
        decimal_mark = ".",
        grouping_mark = ","
      )
    )
  }

  as.numeric(out)
}

parse_data_mensal <- function(x) {
  if (inherits(x, "Date")) {
    return(lubridate::floor_date(x, "month"))
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(lubridate::floor_date(as.Date(x), "month"))
  }

  if (is.numeric(x)) {
    xx <- as.numeric(x)
    med <- suppressWarnings(median(xx, na.rm = TRUE))

    # Data Excel
    if (is.finite(med) && med > 20000 && med < 80000) {
      return(
        lubridate::floor_date(
          as.Date(xx, origin = "1899-12-30"),
          "month"
        )
      )
    }

    # YYYYMM
    if (all(is.na(xx) | (xx >= 190001 & xx <= 210012))) {
      s <- sprintf("%06d", as.integer(xx))
      return(
        as.Date(
          paste0(
            substr(s, 1, 4), "-",
            substr(s, 5, 6), "-01"
          )
        )
      )
    }
  }

  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "NaN")] <- NA_character_

  out <- rep(as.Date(NA), length(s))

  # YYYY-MM ou YYYY/MM
  idx1 <- grepl("^\\d{4}[-/]\\d{1,2}$", s)

  if (any(idx1, na.rm = TRUE)) {
    ss <- gsub("/", "-", s[idx1])
    out[idx1] <- as.Date(paste0(ss, "-01"))
  }

  # MM/YYYY ou MM-YYYY
  idx2 <- is.na(out) & grepl("^\\d{1,2}[-/]\\d{4}$", s)

  if (any(idx2, na.rm = TRUE)) {
    ss <- gsub("-", "/", s[idx2])
    p <- strsplit(ss, "/", fixed = TRUE)

    out[idx2] <- as.Date(
      vapply(
        p,
        function(z) {
          sprintf(
            "%04d-%02d-01",
            as.integer(z[2]),
            as.integer(z[1])
          )
        },
        character(1)
      )
    )
  }

  # YYYYMM texto
  idx3 <- is.na(out) & grepl("^\\d{6}$", s)

  if (any(idx3, na.rm = TRUE)) {
    ss <- s[idx3]

    out[idx3] <- as.Date(
      paste0(
        substr(ss, 1, 4), "-",
        substr(ss, 5, 6), "-01"
      )
    )
  }

  # Demais formatos de data
  idx4 <- is.na(out) & !is.na(s)

  if (any(idx4)) {
    d <- suppressWarnings(
      lubridate::parse_date_time(
        s[idx4],
        orders = c(
          "Ymd", "Y-m-d", "Y/m/d",
          "dmy", "d/m/Y", "d-m-Y",
          "mdy", "m/d/Y", "m-d-Y"
        ),
        quiet = TRUE
      )
    )

    out[idx4] <- as.Date(d)
  }

  lubridate::floor_date(out, "month")
}

mes_pt_numero <- function(x) {
  s <- iconv(
    tolower(trimws(as.character(x))),
    from = "",
    to = "ASCII//TRANSLIT"
  )

  num <- suppressWarnings(as.integer(s))

  out <- ifelse(
    !is.na(num) & num >= 1 & num <= 12,
    num,
    NA_integer_
  )

  chave <- substr(s, 1, 3)

  mapa <- c(
    jan = 1, fev = 2, mar = 3, abr = 4,
    mai = 5, jun = 6, jul = 7, ago = 8,
    set = 9, out = 10, nov = 11, dez = 12
  )

  idx <- is.na(out) & chave %in% names(mapa)
  out[idx] <- unname(mapa[chave[idx]])

  as.integer(out)
}

validar_serie_mensal <- function(df, nome) {
  if (!all(c("data", nome) %in% names(df))) {
    stop("Estrutura inválida para a série ", nome, ".")
  }

  df <- df %>% dplyr::arrange(data)

  if (anyDuplicated(df$data)) {
    print(df %>% dplyr::count(data) %>% dplyr::filter(n > 1))

    stop(
      "A série ",
      nome,
      " possui mais de uma observação no mesmo mês."
    )
  }

  if (all(is.na(df[[nome]]))) {
    stop(
      "A série ",
      nome,
      " foi importada, mas todos os valores são NA."
    )
  }

  df
}

detectar_encoding_csv <- function(caminho) {
  enc <- tryCatch(
    readr::guess_encoding(caminho, n_max = 1000),
    error = function(e) NULL
  )

  if (
    is.null(enc) ||
    nrow(enc) == 0 ||
    is.na(enc$encoding[1])
  ) {
    return("UTF-8")
  }

  enc$encoding[1]
}

ler_arquivo_generico <- function(caminho, aba = 1) {
  if (!file.exists(caminho)) {
    stop(
      "\nArquivo não encontrado:\n",
      caminho,
      "\n\nCorrija o caminho no bloco CONFIGURAÇÕES."
    )
  }

  ext <- tolower(tools::file_ext(caminho))

  if (ext %in% c("xlsx", "xlsm", "xls")) {
    df <- readxl::read_excel(
      caminho,
      sheet = aba,
      .name_repair = "unique"
    )
  } else {
    encoding_usar <- detectar_encoding_csv(caminho)

    primeira <- readr::read_lines(
      caminho,
      n_max = 1,
      locale = readr::locale(encoding = encoding_usar),
      progress = FALSE
    )

    contar <- function(txt) {
      if (
        length(primeira) == 0 ||
        is.na(primeira[1])
      ) {
        return(0L)
      }

      stringr::str_count(
        primeira[1],
        stringr::fixed(txt)
      )
    }

    n_pv  <- contar(";")
    n_vg  <- contar(",")
    n_tab <- contar("\t")

    delim <- if (
      n_tab >= max(n_pv, n_vg) &&
      n_tab > 0
    ) {
      "\t"
    } else if (n_pv > n_vg) {
      ";"
    } else {
      ","
    }

    df <- readr::read_delim(
      caminho,
      delim = delim,
      locale = readr::locale(
        encoding = encoding_usar
      ),
      col_types = readr::cols(.default = "c"),
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "unique"
    )
  }

  names(df) <- normalizar_nome(names(df))
  as.data.frame(df)
}

encontrar_coluna <- function(
    df,
    alternativas,
    nome_logico
) {
  alternativas <- normalizar_nome(alternativas)

  achou <- intersect(
    alternativas,
    names(df)
  )

  if (length(achou) > 0) {
    return(achou[1])
  }

  nomes_sem_codigo <- sub(
    "^[0-9]+_",
    "",
    names(df)
  )

  for (alt in alternativas) {
    idx <- which(
      nomes_sem_codigo == alt
    )

    if (length(idx) == 1) {
      return(names(df)[idx])
    }
  }

  for (alt in alternativas) {
    idx <- which(
      grepl(
        alt,
        names(df),
        fixed = TRUE
      )
    )

    if (length(idx) == 1) {
      return(names(df)[idx])
    }
  }

  stop(
    "\nNão encontrei a coluna de ",
    nome_logico,
    ".\nNomes aceitos: ",
    paste(alternativas, collapse = ", "),
    "\nColunas encontradas: ",
    paste(names(df), collapse = ", ")
  )
}

ler_serie_unica <- function(
    caminho,
    aba,
    nome_final,
    alternativas_valor
) {
  df <- ler_arquivo_generico(
    caminho,
    aba
  )

  col_data <- encontrar_coluna(
    df,
    c(
      "data", "date", "mes",
      "mes_ano", "competencia", "periodo"
    ),
    "data"
  )

  col_valor <- encontrar_coluna(
    df,
    alternativas_valor,
    nome_final
  )

  out <- df %>%
    transmute(
      data = parse_data_mensal(
        .data[[col_data]]
      ),
      valor = parse_numero(
        .data[[col_valor]]
      )
    ) %>%
    filter(
      !is.na(data),
      data >= floor_date(
        DATA_INICIO_DESEJADO,
        "month"
      ),
      data <= floor_date(
        DATA_FIM_DESEJADO,
        "month"
      )
    ) %>%
    arrange(data)

  names(out)[2] <- nome_final

  validar_serie_mensal(
    out,
    nome_final
  )
}


# -----------------------------------------------------------------------------
# Parser de data diária
# -----------------------------------------------------------------------------

parse_data_diaria <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    xx <- as.numeric(x)
    med <- suppressWarnings(median(xx, na.rm = TRUE))

    # Data serial do Excel
    if (is.finite(med) && med > 20000 && med < 80000) {
      return(as.Date(xx, origin = "1899-12-30"))
    }
  }

  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "NaN", "-", "--", "...")] <- NA_character_

  d <- suppressWarnings(
    lubridate::parse_date_time(
      s,
      orders = c(
        "dmy", "d/m/Y", "d-m-Y",
        "Ymd", "Y-m-d", "Y/m/d",
        "mdy", "m/d/Y", "m-d-Y"
      ),
      quiet = TRUE
    )
  )

  as.Date(d)
}

# -----------------------------------------------------------------------------
# SELIC META — FIM DE MÊS
#
# A série 432 pode conter observações diárias/repetidas da Meta Selic.
# Para representar a postura de política monetária no SVAR, usamos a Meta Selic
# vigente na ÚLTIMA observação válida de cada mês, e não a média mensal.
#
# Depois, na base mestre:
#   d_selic_t = selic_eom_t - selic_eom_{t-1}
#
# Isso evita espalhar uma única decisão do Copom artificialmente por dois meses.
# -----------------------------------------------------------------------------

ler_selic_meta <- function(caminho) {

  raw <- ler_arquivo_generico(
    caminho = caminho,
    aba = 1
  )

  col_data <- encontrar_coluna(
    raw,
    c(
      "data", "date", "mes", "mes_ano",
      "competencia", "periodo"
    ),
    "data da Selic"
  )

  col_selic <- encontrar_coluna(
    raw,
    c(
      "432_taxa_de_juros_meta_selic_definida_pelo_copom_a_a",
      "selic_meta",
      "meta_selic",
      "selic",
      "taxa_selic",
      "selic_media_mensal_pct_aa",
      "selic_media_mensal",
      "valor"
    ),
    "Meta Selic"
  )

  out <- raw %>%
    dplyr::transmute(
      data_original = parse_data_diaria(
        .data[[col_data]]
      ),
      selic = suppressWarnings(
        parse_numero(
          .data[[col_selic]]
        )
      )
    ) %>%
    dplyr::filter(
      !is.na(data_original),
      !is.na(selic)
    ) %>%
    dplyr::mutate(
      data = lubridate::floor_date(
        data_original,
        "month"
      )
    ) %>%
    dplyr::filter(
      data >= lubridate::floor_date(
        DATA_INICIO_DESEJADO,
        "month"
      ),
      data <= lubridate::floor_date(
        DATA_FIM_DESEJADO,
        "month"
      )
    ) %>%
    dplyr::group_by(data) %>%
    dplyr::arrange(
      data_original,
      .by_group = TRUE
    ) %>%
    dplyr::slice_tail(
      n = 1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      data,
      selic
    ) %>%
    dplyr::arrange(data)

  validar_serie_mensal(
    out,
    "selic"
  )
}

ler_ipca_ibge <- function(
    caminho,
    aba = 1
) {
  raw <- readxl::read_excel(
    caminho,
    sheet = aba,
    col_names = FALSE,
    .name_repair = "minimal"
  ) %>%
    as.data.frame(
      check.names = FALSE
    )

  if (ncol(raw) < 4) {
    stop(
      "Arquivo histórico do IPCA possui menos de 4 colunas."
    )
  }

  ano_raw <- suppressWarnings(
    parse_numero(raw[[1]])
  )

  ano <- ifelse(
    is.finite(ano_raw) &
      ano_raw >= 1900 &
      ano_raw <= 2100,
    as.integer(ano_raw),
    NA_integer_
  )

  ano <- zoo::na.locf(
    ano,
    na.rm = FALSE
  )

  mes <- mes_pt_numero(
    raw[[2]]
  )

  ipca <- suppressWarnings(
    parse_numero(
      raw[[4]]
    )
  )

  valido <-
    !is.na(ano) &
    !is.na(mes) &
    mes >= 1 &
    mes <= 12 &
    !is.na(ipca)

  out <- tibble(
    data = as.Date(
      sprintf(
        "%04d-%02d-01",
        ano[valido],
        mes[valido]
      )
    ),
    ipca = as.numeric(
      ipca[valido]
    )
  ) %>%
    filter(
      data >= floor_date(
        DATA_INICIO_DESEJADO,
        "month"
      ),
      data <= floor_date(
        DATA_FIM_DESEJADO,
        "month"
      )
    ) %>%
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    arrange(data)

  validar_serie_mensal(
    out,
    "ipca"
  )
}

ler_bets_ggr_mensal <- function(
    caminho,
    aba = "Base_mensal"
) {
  raw <- readxl::read_excel(
    caminho,
    sheet = aba,
    skip = 2,
    .name_repair = "unique"
  ) %>%
    as.data.frame(
      check.names = FALSE
    )

  names(raw) <- normalizar_nome(
    names(raw)
  )

  col_data <- encontrar_coluna(
    raw,
    c(
      "data", "date",
      "mes_ano", "competencia", "periodo"
    ),
    "data da série mensal de Bets"
  )

  col_bets <- encontrar_coluna(
    raw,
    c(
      "ggr_bets_r_bi",
      "ggr_bets_rbi",
      "ggr_bets",
      "ggr",
      "bets"
    ),
    "GGR mensal das Bets"
  )

  out <- raw %>%
    transmute(
      data = parse_data_mensal(
        .data[[col_data]]
      ),
      bets = parse_numero(
        .data[[col_bets]]
      )
    ) %>%
    filter(
      !is.na(data),
      !is.na(bets),
      data >= floor_date(
        DATA_INICIO_DESEJADO,
        "month"
      ),
      data <= floor_date(
        DATA_FIM_DESEJADO,
        "month"
      )
    ) %>%
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    arrange(data)

  validar_serie_mensal(
    out,
    "bets"
  )
}

# =============================================================================
# 3. DOWNLOAD ROBUSTO DO IBC-BR NO SGS/BCB
# =============================================================================

baixar_bcb_blocos <- function(
    codigo,
    nome,
    inicio,
    fim,
    anos_por_bloco = 9
) {
  inicio <- as.Date(inicio)
  fim    <- as.Date(fim)

  resultados <- list()

  inicio_bloco <- inicio
  contador <- 1

  while (inicio_bloco <= fim) {
    fim_bloco <- min(
      inicio_bloco %m+%
        lubridate::years(
          anos_por_bloco
        ) -
        lubridate::days(1),
      fim
    )

    cat(
      "\nBaixando SGS",
      codigo,
      ":",
      as.character(inicio_bloco),
      "até",
      as.character(fim_bloco),
      "\n"
    )

    temp <- rbcb::get_series(
      code = codigo,
      start_date = as.character(
        inicio_bloco
      ),
      end_date = as.character(
        fim_bloco
      )
    )

    temp <- as.data.frame(
      temp
    )

    if (!"date" %in% names(temp)) {
      stop(
        paste0(
          "A série SGS ",
          codigo,
          " não retornou a coluna 'date'. ",
          "Colunas encontradas: ",
          paste(
            names(temp),
            collapse = ", "
          )
        )
      )
    }

    if ("value" %in% names(temp)) {
      valor <- temp$value
    } else {
      candidatos <- setdiff(
        names(temp),
        c(
          "date",
          "series",
          "serie",
          "variable"
        )
      )

      candidatos_numericos <- candidatos[
        sapply(
          temp[candidatos],
          is.numeric
        )
      ]

      if (length(candidatos_numericos) == 0) {
        stop(
          paste0(
            "Não encontrei coluna numérica para SGS ",
            codigo,
            ". Colunas retornadas: ",
            paste(
              names(temp),
              collapse = ", "
            )
          )
        )
      }

      valor <- temp[[candidatos_numericos[1]]]
    }

    resultados[[contador]] <- tibble(
      data = as.Date(
        temp$date
      ),
      valor = as.numeric(
        valor
      )
    )

    inicio_bloco <-
      fim_bloco +
      lubridate::days(1)

    contador <-
      contador + 1
  }

  resultado <- dplyr::bind_rows(
    resultados
  ) %>%
    mutate(
      data = floor_date(
        data,
        "month"
      )
    ) %>%
    group_by(data) %>%
    summarise(
      valor = mean(
        valor,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    arrange(data) %>%
    distinct(
      data,
      .keep_all = TRUE
    )

  names(resultado)[2] <- nome

  resultado
}

# =============================================================================
# 4. IMPORTAÇÃO DAS SÉRIES
# =============================================================================

cat("\n============================================================")
cat("\n1. IMPORTANDO AS SÉRIES")
cat("\n============================================================\n")

inad_10sm <- ler_serie_unica(
  arquivo_inad_10sm,
  aba_inad_10sm,
  "inad_pf_10sm",
  c(
    "inadimplencia_pf_ate10sm_pct",
    "inadimplencia_pf_ate_10sm_pct",
    "inad_pf_10sm",
    "inadimplencia_pf_10sm",
    "inadimplencia_pf_ate10sm"
  )
)

selic_df <- ler_selic_meta(
  arquivo_selic
)

write.csv(
  selic_df,
  file.path(
    DIR_OUT,
    "selic_meta_fim_mes.csv"
  ),
  row.names = FALSE
)


ipca_df <- ler_ipca_ibge(
  arquivo_ipca,
  aba_ipca
)

bets_df <- ler_bets_ggr_mensal(
  arquivo_bets,
  aba_bets
)

# Baixamos alguns meses antes para construir Delta log(IBC)
ibc_df <- baixar_bcb_blocos(
  codigo = SGS_IBC,
  nome = "ibc",
  inicio = DATA_INICIO_DESEJADO %m-%
    months(3),
  fim = DATA_FIM_DESEJADO
)

validar_serie_mensal(
  ibc_df,
  "ibc"
)
# =============================================================================
# 5. BASE MESTRA E BETS REAIS
# =============================================================================

cat("\n============================================================")
cat("\n2. CONSTRUINDO BASE MESTRA")
cat("\n============================================================\n")

base_master <- list(
  inad_10sm,
  selic_df,
  ipca_df,
  bets_df,
  ibc_df
) %>%
  purrr::reduce(
    dplyr::full_join,
    by = "data"
  ) %>%
  arrange(data)

# Índice de preços construído a partir do IPCA mensal.
base_master$indice_precos <- NA_real_

idx_ipca <- which(
  !is.na(
    base_master$ipca
  )
)

if (length(idx_ipca) == 0) {
  stop(
    "Não há observações válidas de IPCA para construir o deflator."
  )
}

fator_ipca <-
  1 +
  base_master$ipca[
    idx_ipca
  ] / 100

indice_tmp <-
  100 *
  cumprod(
    fator_ipca
  ) /
  cumprod(
    fator_ipca
  )[1]

base_master$indice_precos[
  idx_ipca
] <- indice_tmp

# Referência: dezembro/2025, quando disponível.
ref_data <- as.Date(
  "2025-12-01"
)

ref_idx <- which(
  base_master$data == ref_data &
    !is.na(
      base_master$indice_precos
    )
)

if (length(ref_idx) == 0) {
  ref_idx <- tail(
    which(
      !is.na(
        base_master$indice_precos
      )
    ),
    1
  )
}

indice_ref <-
  base_master$indice_precos[
    ref_idx[1]
  ]

base_master <- base_master %>%
  mutate(
    bets_real = ifelse(
      !is.na(bets) &
        !is.na(indice_precos),
      bets *
        indice_ref /
        indice_precos,
      NA_real_
    ),

    log_bets_real = ifelse(
      bets_real > 0,
      log(
        bets_real
      ),
      NA_real_
    ),

    dlog_bets_real =
      100 *
      (
        log_bets_real -
          dplyr::lag(
            log_bets_real
          )
      ),

    log_ibc = ifelse(
      ibc > 0,
      log(
        ibc
      ),
      NA_real_
    ),

    dlog_ibc =
      100 *
      (
        log_ibc -
          dplyr::lag(
            log_ibc
          )
      ),

    d_selic =
      selic -
        dplyr::lag(
          selic
        ),

    d_inad =
      inad_pf_10sm -
        dplyr::lag(
          inad_pf_10sm
        )
  )
# =============================================================================
# 6. TRANSFORMAÇÕES DO SVAR
# =============================================================================

TRANSFORM_BETS  <- "dlog"
TRANSFORM_SELIC <- "diff"
TRANSFORM_INAD  <- "diff"

# Mesma amostra para o modelo principal e para a ordem alternativa.
base_model <- base_master %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(
    
    bets = if (
      TRANSFORM_BETS == "log"
    ) {
      log_bets_real
    } else {
      dlog_bets_real
    },
    
    # IBC-Br entra como crescimento mensal
    ibc = dlog_ibc,
    
    # IPCA mensal em %
    ipca = ipca,
    
    # Selic
    selic = if (
      TRANSFORM_SELIC == "nivel"
    ) {
      selic
    } else {
      d_selic
    },
    
    # Inadimplência PF até 10 SM
    inad = if (
      TRANSFORM_INAD == "nivel"
    ) {
      inad_pf_10sm
    } else {
      d_inad
    }
    
  ) %>%
  dplyr::filter(
    data >= DATA_INICIO_DESEJADO,
    data <= DATA_FIM_DESEJADO
  )


# Mesma amostra para modelo principal e ordem alternativa
base_svar <- base_model %>%
  dplyr::select(
    data,
    bets,
    ibc,
    ipca,
    selic,
    inad
  ) %>%
  tidyr::drop_na() %>%
  dplyr::arrange(data)

if (nrow(base_svar) < 36) {
  stop(
    "A amostra efetiva do SVAR ficou com apenas ",
    nrow(base_svar),
    " observações. Reveja as séries/cobertura."
  )
}

# Verificar se, depois do drop_na(), os meses permanecem consecutivos.
meses_esperados <- seq.Date(
  min(base_svar$data),
  max(base_svar$data),
  by = "month"
)

meses_faltantes <- setdiff(
  meses_esperados,
  base_svar$data
)

if (length(meses_faltantes) > 0) {
  stop(
    "A base do VAR possui meses faltantes após o merge: ",
    paste(
      format(
        meses_faltantes,
        "%Y-%m"
      ),
      collapse = ", "
    ),
    ". Um VAR mensal exige observações igualmente espaçadas."
  )
}

cat(
  "\nAmostra efetiva do SVAR:",
  format(
    min(base_svar$data),
    "%Y-%m"
  ),
  "a",
  format(
    max(base_svar$data),
    "%Y-%m"
  ),
  "\nObservações:",
  nrow(base_svar),
  "\n"
)

write.csv(
  base_svar,
  file.path(
    DIR_OUT,
    "base_SVAR_Itau_Selic.csv"
  ),
  row.names = FALSE
)

# Auditoria da Selic usada no modelo
selic_auditoria <- base_master %>%
  dplyr::select(
    data,
    selic,
    d_selic
  ) %>%
  dplyr::filter(
    data >= DATA_INICIO_DESEJADO,
    data <= DATA_FIM_DESEJADO
  )

write.csv(
  selic_auditoria,
  file.path(
    DIR_OUT,
    "auditoria_selic_eom_e_delta.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 7. GRÁFICOS DAS SÉRIES
# =============================================================================

base_long <- base_svar %>%
  pivot_longer(
    cols = -data,
    names_to = "variavel",
    values_to = "valor"
  )

g_series <- ggplot(
  base_long,
  aes(
    x = data,
    y = valor
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ variavel,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Séries utilizadas nos SVARs do projeto Itaú",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  file.path(
    DIR_OUT,
    "series_SVAR_Itau.png"
  ),
  g_series,
  width = 10,
  height = 9,
  dpi = 300
)

# =============================================================================
# 8. TESTES DE ESTACIONARIEDADE
# =============================================================================
#
# ADF e PP:
#   H0 = raiz unitária.
#
# KPSS:
#   H0 = estacionariedade.
#
# A coluna "estacionaria_maioria" considera estacionária quando pelo menos
# 2 dos 3 sinais apontam para estacionariedade a 5%.
# =============================================================================
teste_estacionariedade <- function(
    x,
    nome
) {
  x <- as.numeric(
    x
  )

  x <- x[
    is.finite(
      x
    )
  ]

  if (length(x) < 20) {
    return(
      tibble(
        variavel = nome,
        n = length(x),
        adf_p = NA_real_,
        pp_p = NA_real_,
        kpss_p = NA_real_,
        estacionaria_maioria = NA,
        classificacao = "Amostra insuficiente"
      )
    )
  }

  adf <- tryCatch(
    suppressWarnings(
      tseries::adf.test(
        x,
        alternative = "stationary"
      )
    ),
    error = function(e) NULL
  )

  pp <- tryCatch(
    suppressWarnings(
      tseries::pp.test(
        x,
        alternative = "stationary"
      )
    ),
    error = function(e) NULL
  )

  kpss <- tryCatch(
    suppressWarnings(
      tseries::kpss.test(
        x,
        null = "Level"
      )
    ),
    error = function(e) NULL
  )

  adf_p <- if (is.null(adf)) {
    NA_real_
  } else {
    as.numeric(
      adf$p.value
    )
  }

  pp_p <- if (is.null(pp)) {
    NA_real_
  } else {
    as.numeric(
      pp$p.value
    )
  }

  kpss_p <- if (is.null(kpss)) {
    NA_real_
  } else {
    as.numeric(
      kpss$p.value
    )
  }

  sinais <- c(
    is.finite(adf_p) && adf_p < 0.05,
    is.finite(pp_p) && pp_p < 0.05,
    is.finite(kpss_p) && kpss_p > 0.05
  )

  estacionaria <-
    sum(
      sinais %in% TRUE
    ) >= 2

  tibble(
    variavel = nome,
    n = length(x),
    adf_p = adf_p,
    pp_p = pp_p,
    kpss_p = kpss_p,
    estacionaria_maioria = estacionaria,
    classificacao = ifelse(
      estacionaria,
      "Compatível com I(0)",
      "Rever transformação / possível não estacionariedade"
    )
  )
}

testes_estacionariedade <- purrr::imap_dfr(
  base_svar %>%
    dplyr::select(
      -data
    ),
  ~ teste_estacionariedade(
    .x,
    .y
  )
)

cat("\n============================================================")
cat("\n3. TESTES DE ESTACIONARIEDADE – VARIÁVEIS DO SVAR")
cat("\n============================================================\n")

print(
  testes_estacionariedade,
  n = Inf
)

write.csv(
  testes_estacionariedade,
  file.path(
    DIR_OUT,
    "testes_estacionariedade_SVAR.csv"
  ),
  row.names = FALSE
)

if (
  any(
    testes_estacionariedade$estacionaria_maioria %in% FALSE
  )
) {
  warning(
    paste0(
      "Ao menos uma variável selecionada não parece I(0) pela regra conjunta. ",
      "Antes de interpretar o SVAR como estacionário, confira a tabela ",
      "'testes_estacionariedade_SVAR.csv'. Para Bets, considere TRANSFORM_BETS='dlog'; ",
      "para Selic/inadimplência, considere 'diff' se economicamente e estatisticamente adequado. ",
      "Se houver múltiplas variáveis I(1) cointegradas, considere VECM/SVEC em vez de ",
      "diferenciar mecanicamente."
    )
  )
}

# ============================================================
# TESTES ADICIONAIS PARA DELTA SELIC (META DE FIM DE MÊS)
# ============================================================

selic_diff <- base_svar$selic

# ADF com constante
adf_selic_drift <- urca::ur.df(
  selic_diff,
  type = "drift",
  selectlags = "AIC"
)

summary(adf_selic_drift)


# ADF sem constante
adf_selic_none <- urca::ur.df(
  selic_diff,
  type = "none",
  selectlags = "AIC"
)

summary(adf_selic_none)


# ADF com tendência
adf_selic_trend <- urca::ur.df(
  selic_diff,
  type = "trend",
  selectlags = "AIC"
)

summary(adf_selic_trend)


# Phillips-Perron
pp_selic <- urca::ur.pp(
  selic_diff,
  type = "Z-tau",
  model = "constant",
  lags = "short"
)

summary(pp_selic)


# KPSS
kpss_selic <- urca::ur.kpss(
  selic_diff,
  type = "mu"
)

summary(kpss_selic)


# Zivot-Andrews: permite uma quebra estrutural
za_selic <- urca::ur.za(
  selic_diff,
  model = "intercept",
  lag = 1
)

summary(za_selic)

plot(
  base_svar$data,
  base_svar$selic,
  type = "l",
  main = "Primeira diferença da Selic",
  xlab = "",
  ylab = "Δ Selic (p.p.)"
)

acf(
  base_svar$selic,
  main = "ACF – Δ Selic"
)

pacf(
  base_svar$selic,
  main = "PACF – Δ Selic"
)

# =============================================================================
# 9. FUNÇÕES DO VAR / SVAR
# =============================================================================

irf_para_df <- function(
    objeto_irf,
    choque
) {
  irf_mat   <- objeto_irf$irf[[choque]]
  lower_mat <- objeto_irf$Lower[[choque]]
  upper_mat <- objeto_irf$Upper[[choque]]

  respostas <- colnames(
    irf_mat
  )

  purrr::map_dfr(
    respostas,
    function(resp) {
      tibble(
        horizonte =
          0:(
            nrow(
              irf_mat
            ) - 1
          ),
        resposta = resp,
        irf = irf_mat[, resp],
        inferior = lower_mat[, resp],
        superior = upper_mat[, resp]
      )
    }
  )
}

# Decide quais respostas devem ser acumuladas para interpretação em nível.
respostas_cumulativas <- function(
    ordem
) {
  out <- character(0)

  # IBC entra em Delta log.
  if ("ibc" %in% ordem) {
    out <- c(
      out,
      "ibc"
    )
  }

  # Bets em dlog -> acumulada recupera aproximadamente o log-nível.
  if (
    "bets" %in% ordem &&
    TRANSFORM_BETS == "dlog"
  ) {
    out <- c(
      out,
      "bets"
    )
  }

  if (
    "selic" %in% ordem &&
    TRANSFORM_SELIC == "diff"
  ) {
    out <- c(
      out,
      "selic"
    )
  }

  if (
    "inad" %in% ordem &&
    TRANSFORM_INAD == "diff"
  ) {
    out <- c(
      out,
      "inad"
    )
  }

  unique(
    out
  )
}
selecionar_irf_exibicao <- function(
    raw_df,
    cum_df,
    ordem
) {
  cum_vars <- respostas_cumulativas(
    ordem
  )

  dplyr::bind_rows(
    raw_df %>%
      filter(
        !resposta %in% cum_vars
      ),
    cum_df %>%
      filter(
        resposta %in% cum_vars
      )
  ) %>%
    arrange(
      match(
        resposta,
        ordem
      ),
      horizonte
    )
}

rotulos_variaveis <- c(
  bets = ifelse(
    TRANSFORM_BETS == "log",
    "Bets – log GGR real",
    "Bets – variação mensal\n(efeito acumulado, %)"
  ),
  ibc = "IBC-Br\n(efeito acumulado, %)",
  ipca = "IPCA mensal\n(p.p.)",
  selic = ifelse(
    TRANSFORM_SELIC == "nivel",
    "Selic Meta\n(p.p.)",
    "Selic Meta\n(efeito acumulado, p.p.)"
  ),
  inad = ifelse(
    TRANSFORM_INAD == "nivel",
    "Inadimplência PF ≤ 10 SM\n(p.p.)",
    "Inadimplência PF ≤ 10 SM\n(efeito acumulado, p.p.)"
  )
)
extrair_p_seguro <- function(
    objeto,
    caminho
) {
  tryCatch(
    {
      z <- objeto

      for (nm in caminho) {
        z <- z[[nm]]
      }

      as.numeric(
        z$p.value
      )
    },
    error = function(e) NA_real_
  )
}

calcular_lag_max_admissivel <- function(
    n,
    K,
    n_exogenas = 0
) {
  # Por equação:
  # aproximadamente K*p + constante + exógenas.
  max_por_regra <- floor(
    (
      n /
        MIN_OBS_POR_PARAM -
        1 -
        n_exogenas
    ) /
      K
  )

  max_por_regra <- max(
    1,
    max_por_regra
  )

  min(
    LAG_MAX_USUARIO,
    max_por_regra
  )
}

resumir_irf_inad <- function(
    irf_df,
    nome_modelo
) {
  z <- irf_df %>%
    filter(
      resposta == "inad"
    ) %>%
    mutate(
      significativo_90 =
        inferior > 0 |
        superior < 0
    )

  if (nrow(z) == 0) {
    return(
      tibble(
        modelo = nome_modelo,
        pico_h = NA_integer_,
        pico_resposta = NA_real_,
        horizontes_significativos_90 = NA_character_
      )
    )
  }

  pico <- z %>%
    slice_max(
      order_by = abs(
        irf
      ),
      n = 1,
      with_ties = FALSE
    )

  hs <- z %>%
    filter(
      significativo_90
    ) %>%
    pull(
      horizonte
    )

  tibble(
    modelo = nome_modelo,
    pico_h = pico$horizonte[1],
    pico_resposta = pico$irf[1],
    horizontes_significativos_90 = if (
      length(hs) == 0
    ) {
      "nenhum"
    } else {
      paste(
        hs,
        collapse = ", "
      )
    }
  )
}

# =============================================================================
# 10. FUNÇÃO PRINCIPAL PARA ESTIMAR UM SVAR RECURSIVO
# =============================================================================

rodar_svar <- function(
    nome_modelo,
    ordem,
    choque = "bets"
) {
  cat("\n\n============================================================")
  cat("\nMODELO:", nome_modelo)
  cat("\nORDEM:", paste(ordem, collapse = " -> "))
  cat("\n============================================================\n")

  if (!choque %in% ordem) {
    stop(
      "O choque '",
      choque,
      "' não pertence à ordem do modelo ",
      nome_modelo,
      "."
    )
  }

  dir_modelo <- file.path(
    DIR_OUT,
    nome_modelo
  )

  if (!dir.exists(dir_modelo)) {
    dir.create(
      dir_modelo,
      recursive = TRUE
    )
  }

  dados <- base_svar %>%
    dplyr::select(
      data,
      dplyr::all_of(
        ordem
      )
    )

  Y <- as.matrix(
    dados[
      ,
      ordem,
      drop = FALSE
    ]
  )

  colnames(Y) <- ordem

  # ---------------------------------------------------------------------------
  # Exógenas determinísticas
  # ---------------------------------------------------------------------------

  D_SAZ <- NULL
  n_exogenas <- 0

  if (USAR_DUMMIES_SAZONAIS) {
    mes_factor <- factor(
      month(
        dados$data
      ),
      levels = 1:12
    )

    D_SAZ <- model.matrix(
      ~ mes_factor
    )[
      ,
      -1,
      drop = FALSE
    ]

    colnames(D_SAZ) <- paste0(
      "mes_",
      2:12
    )

    n_exogenas <- ncol(
      D_SAZ
    )
  }

  # ---------------------------------------------------------------------------
  # Seleção do número de defasagens
  # ---------------------------------------------------------------------------

  K <- ncol(Y)
  n <- nrow(Y)

  lag_max <- calcular_lag_max_admissivel(
    n = n,
    K = K,
    n_exogenas = n_exogenas
  )

  cat(
    "\nNúmero de variáveis:",
    K,
    "\nObservações:",
    n,
    "\nlag.max solicitado:",
    LAG_MAX_USUARIO,
    "\nlag.max efetivamente usado:",
    lag_max,
    "\n"
  )

  lag_selection <- vars::VARselect(
    Y,
    lag.max = lag_max,
    type = "const",
    exogen = D_SAZ
  )

  print(
    lag_selection$selection
  )

  print(
    round(
      lag_selection$criteria,
      4
    )
  )

  if (!CRITERIO_P %in% names(lag_selection$selection)) {
    stop(
      "Critério ",
      CRITERIO_P,
      " não encontrado em VARselect()."
    )
  }

  if (is.null(P_FORCADO)) {
    p <- as.integer(
      lag_selection$selection[
        CRITERIO_P
      ]
    )
    criterio_usado <- CRITERIO_P
  } else {
    p <- as.integer(P_FORCADO)
    criterio_usado <- paste0("FORÇADO (p=", p, ")")
  }

  if (
    !is.finite(p) ||
    p < 1 ||
    p > lag_max
  ) {
    stop(
      "Número de defasagens inválido selecionado para ",
      nome_modelo,
      ". p = ",
      p,
      "; lag.max admissível = ",
      lag_max,
      "."
    )
  }

  cat(
    "\nCritério:",
    criterio_usado,
    "\nVAR selecionado: p =",
    p,
    "\n"
  )

  write.csv(
    lag_selection$criteria,
    file.path(
      dir_modelo,
      "criterios_defasagens.csv"
    )
  )

  # ---------------------------------------------------------------------------
  # Diagnóstico comparativo de p = 1,...,lag_max
  # ---------------------------------------------------------------------------

  diagnostico_lags <- purrr::map_dfr(
    seq_len(lag_max),
    function(pp) {

      mod_pp <- if (is.null(D_SAZ)) {
        vars::VAR(
          y = Y,
          p = pp,
          type = "const"
        )
      } else {
        vars::VAR(
          y = Y,
          p = pp,
          type = "const",
          exogen = D_SAZ
        )
      }

      roots_pp <- vars::roots(
        mod_pp,
        modulus = TRUE
      )

      serial_pp <- tryCatch(
        vars::serial.test(
          mod_pp,
          lags.pt = max(12, pp + 1),
          type = "ES"
        ),
        error = function(e) NULL
      )

      serial_p_pp <- if (is.null(serial_pp)) {
        NA_real_
      } else {
        tryCatch(
          as.numeric(
            serial_pp$serial$p.value
          ),
          error = function(e) NA_real_
        )
      }

      tibble::tibble(
        p = pp,
        maior_raiz = max(Mod(roots_pp)),
        estavel = max(Mod(roots_pp)) < 1,
        serial_ES_p = serial_p_pp,
        sem_autocorrelacao_5pct =
          is.finite(serial_p_pp) &&
          serial_p_pp > 0.05
      )
    }
  )

  print(
    diagnostico_lags,
    n = Inf
  )

  write.csv(
    diagnostico_lags,
    file.path(
      dir_modelo,
      "comparacao_diagnosticos_lags.csv"
    ),
    row.names = FALSE
  )

  # ---------------------------------------------------------------------------
  # VAR reduzido
  # ---------------------------------------------------------------------------
  if (is.null(D_SAZ)) {
    
    var_red <- vars::VAR(
      y = Y,
      p = p,
      type = "const"
    )
    
  } else {
    
    var_red <- vars::VAR(
      y = Y,
      p = p,
      type = "const",
      exogen = D_SAZ
    )
    
  }
  capture.output(
    summary(
      var_red
    ),
    file = file.path(
      dir_modelo,
      "VAR_reduzido.txt"
    )
  )

  # ---------------------------------------------------------------------------
  # Diagnósticos
  # ---------------------------------------------------------------------------

  roots_var <- vars::roots(
    var_red,
    modulus = TRUE
  )

  maior_raiz <- max(
    Mod(
      roots_var
    )
  )

  serial_ES <- tryCatch(
    vars::serial.test(
      var_red,
      lags.pt = max(
        12,
        p + 1
      ),
      type = "ES"
    ),
    error = function(e) NULL
  )

  normalidade <- tryCatch(
    vars::normality.test(
      var_red,
      multivariate.only = TRUE
    ),
    error = function(e) NULL
  )

  arch_multi <- tryCatch(
    vars::arch.test(
      var_red,
      lags.multi = 5,
      multivariate.only = TRUE
    ),
    error = function(e) NULL
  )

  serial_p <- if (
    is.null(serial_ES)
  ) {
    NA_real_
  } else {
    tryCatch(
      as.numeric(
        serial_ES$serial$p.value
      ),
      error = function(e) NA_real_
    )
  }

  normalidade_p <- if (
    is.null(normalidade)
  ) {
    NA_real_
  } else {
    tryCatch(
      as.numeric(
        normalidade$jb.mul$JB$p.value
      ),
      error = function(e) NA_real_
    )
  }

  arch_p <- if (
    is.null(arch_multi)
  ) {
    NA_real_
  } else {
    tryCatch(
      as.numeric(
        arch_multi$arch.mul$p.value
      ),
      error = function(e) NA_real_
    )
  }

  diagnosticos <- tibble(
    modelo = nome_modelo,
    n = n,
    K = K,
    p = p,
    lag_max_usado = lag_max,
    maior_raiz = maior_raiz,
    estavel = maior_raiz < 1,
    serial_ES_p = serial_p,
    normalidade_JB_p = normalidade_p,
    ARCH_p = arch_p
  )

  print(
    diagnosticos
  )

  write.csv(
    diagnosticos,
    file.path(
      dir_modelo,
      "diagnosticos.csv"
    ),
    row.names = FALSE
  )

  capture.output(
    serial_ES,
    normalidade,
    arch_multi,
    file = file.path(
      dir_modelo,
      "diagnosticos_completos.txt"
    )
  )

  # CUSUM
  estabilidade_cusum <- tryCatch(
    vars::stability(
      var_red,
      type = "OLS-CUSUM"
    ),
    error = function(e) NULL
  )

  if (!is.null(estabilidade_cusum)) {
    pdf(
      file.path(
        dir_modelo,
        "CUSUM_VAR.pdf"
      ),
      width = 9,
      height = 10
    )

    plot(
      estabilidade_cusum
    )

    dev.off()
  }

  if (maior_raiz >= 1) {
    warning(
      "O modelo ",
      nome_modelo,
      " não é estável: maior raiz >= 1. ",
      "Não interprete as IRFs como respostas transitórias de um VAR estacionário ",
      "sem rever transformações/especificação."
    )
  }

  # ---------------------------------------------------------------------------
  # Identificação recursiva – Cholesky
  # ---------------------------------------------------------------------------

  Sigma_hat <- summary(
    var_red
  )$covres

  L <- t(
    chol(
      Sigma_hat
    )
  )

  rownames(L) <- ordem
  colnames(L) <- ordem

  write.csv(
    L,
    file.path(
      dir_modelo,
      "matriz_impacto_Cholesky.csv"
    )
  )

  cat("\nMatriz de impacto Cholesky:\n")

  print(
    round(
      L,
      4
    )
  )

  # ---------------------------------------------------------------------------
  # IRFs – choque de Bets
  # ---------------------------------------------------------------------------

  set.seed(20260930)

  irf_raw <- vars::irf(
    var_red,
    impulse = choque,
    response = ordem,
    n.ahead = H_IRF,
    ortho = TRUE,
    cumulative = FALSE,
    boot = TRUE,
    runs = BOOT_RUNS,
    ci = CI_IRF
  )

  set.seed(20260930)

  irf_cum <- vars::irf(
    var_red,
    impulse = choque,
    response = ordem,
    n.ahead = H_IRF,
    ortho = TRUE,
    cumulative = TRUE,
    boot = TRUE,
    runs = BOOT_RUNS,
    ci = CI_IRF
  )

  irf_raw_df <- irf_para_df(
    irf_raw,
    choque
  )

  irf_cum_df <- irf_para_df(
    irf_cum,
    choque
  )

  irf_display <- selecionar_irf_exibicao(
    raw_df = irf_raw_df,
    cum_df = irf_cum_df,
    ordem = ordem
  ) %>%
    mutate(
      nome = unname(
        rotulos_variaveis[
          resposta
        ]
      )
    )

  write.csv(
    irf_raw_df,
    file.path(
      dir_modelo,
      "IRF_bets_raw.csv"
    ),
    row.names = FALSE
  )

  write.csv(
    irf_cum_df,
    file.path(
      dir_modelo,
      "IRF_bets_cumulativa.csv"
    ),
    row.names = FALSE
  )

  write.csv(
    irf_display,
    file.path(
      dir_modelo,
      "IRF_bets_exibicao.csv"
    ),
    row.names = FALSE
  )

  # Todas as respostas
  g_irf <- ggplot(
    irf_display,
    aes(
      x = horizonte,
      y = irf
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = 2
    ) +
    geom_ribbon(
      aes(
        ymin = inferior,
        ymax = superior
      ),
      alpha = 0.20
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    facet_wrap(
      ~ nome,
      scales = "free_y",
      ncol = 2
    ) +
    labs(
      title = paste0(
        "Resposta a um choque positivo de Bets – ",
        nome_modelo
      ),
      subtitle = paste0(
        "SVAR recursivo | ordem: ",
        paste(
          ordem,
          collapse = " → "
        ),
        " | IC ",
        round(
          100 * CI_IRF
        ),
        "%"
      ),
      x = "Meses após o choque",
      y = "Resposta"
    ) +
    theme_minimal(
      base_size = 12
    )

  ggsave(
    file.path(
      dir_modelo,
      "IRF_bets_todas_respostas.png"
    ),
    g_irf,
    width = 10,
    height = 8,
    dpi = 300
  )

  # IRF central do projeto: Bets -> Inadimplência
  irf_inad <- irf_display %>%
    filter(
      resposta == "inad"
    )

  g_inad <- ggplot(
    irf_inad,
    aes(
      x = horizonte,
      y = irf
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = 2
    ) +
    geom_ribbon(
      aes(
        ymin = inferior,
        ymax = superior
      ),
      alpha = 0.20
    ) +
    geom_line(
      linewidth = 0.9
    ) +
    labs(
      title = "Choque de Bets → Inadimplência PF até 10 SM",
      subtitle = paste0(
        nome_modelo,
        " | IC ",
        round(
          100 * CI_IRF
        ),
        "%"
      ),
      x = "Meses após o choque",
      y = ifelse(
        TRANSFORM_INAD == "nivel",
        "Resposta da inadimplência (p.p.)",
        "Efeito acumulado sobre a inadimplência (p.p.)"
      )
    ) +
    theme_minimal(
      base_size = 12
    )

  ggsave(
    file.path(
      dir_modelo,
      "IRF_Bets_para_Inadimplencia.png"
    ),
    g_inad,
    width = 8,
    height = 5,
    dpi = 300
  )

  # ---------------------------------------------------------------------------
  # FEVD
  # ---------------------------------------------------------------------------

  fevd_obj <- vars::fevd(
    var_red,
    n.ahead = H_IRF
  )

  H_FEVD <- c(
    3,
    6,
    12,
    24
  )

  H_FEVD <- H_FEVD[
    H_FEVD <= H_IRF
  ]

  fevd_inad_mat <- fevd_obj$inad

  H_FEVD <- H_FEVD[
    H_FEVD <= nrow(
      fevd_inad_mat
    )
  ]

  fevd_inad <- purrr::map_dfr(
    H_FEVD,
    function(h) {
      tibble(
        horizonte = h,
        choque = colnames(
          fevd_inad_mat
        ),
        participacao = as.numeric(
          fevd_inad_mat[
            h,
            ,
            drop = TRUE
          ]
        ),
        participacao_pct =
          100 *
          as.numeric(
            fevd_inad_mat[
              h,
              ,
              drop = TRUE
            ]
          )
      )
    }
  )

  fevd_bets_inad <- fevd_inad %>%
    filter(
      choque == "bets"
    )

  write.csv(
    fevd_inad,
    file.path(
      dir_modelo,
      "FEVD_inadimplencia_completa.csv"
    ),
    row.names = FALSE
  )

  write.csv(
    fevd_bets_inad,
    file.path(
      dir_modelo,
      "FEVD_Bets_na_inadimplencia.csv"
    ),
    row.names = FALSE
  )

  # ---------------------------------------------------------------------------
  # Causalidade de Granger – evidência preditiva, NÃO causalidade estrutural
  # ---------------------------------------------------------------------------

  granger_bets <- tryCatch(
    vars::causality(
      var_red,
      cause = choque
    ),
    error = function(e) NULL
  )

  capture.output(
    granger_bets,
    file = file.path(
      dir_modelo,
      "causalidade_Granger_Bets.txt"
    )
  )

  # ---------------------------------------------------------------------------
  # Resumo da IRF Bets -> inadimplência
  # ---------------------------------------------------------------------------

  resumo_irf <- resumir_irf_inad(
    irf_display,
    nome_modelo
  )

  write.csv(
    resumo_irf,
    file.path(
      dir_modelo,
      "resumo_IRF_Bets_Inad.csv"
    ),
    row.names = FALSE
  )

  # ---------------------------------------------------------------------------
  # Salvar objetos
  # ---------------------------------------------------------------------------

  saveRDS(
    var_red,
    file.path(
      dir_modelo,
      "var_reduzido.rds"
    )
  )

  saveRDS(
    irf_raw,
    file.path(
      dir_modelo,
      "irf_raw.rds"
    )
  )

  saveRDS(
    irf_cum,
    file.path(
      dir_modelo,
      "irf_cumulativa.rds"
    )
  )

  # ---------------------------------------------------------------------------
  # Retorno
  # ---------------------------------------------------------------------------

  list(
    nome = nome_modelo,
    ordem = ordem,
    p = p,
    lag_selection = lag_selection,
    modelo = var_red,
    diagnosticos = diagnosticos,
    matriz_impacto = L,
    irf_raw = irf_raw_df,
    irf_cum = irf_cum_df,
    irf_display = irf_display,
    fevd_inad = fevd_inad,
    fevd_bets_inad = fevd_bets_inad,
    granger = granger_bets,
    resumo_irf = resumo_irf
  )
}
# =============================================================================
# 11. MODELO PRINCIPAL
# =============================================================================
#
# BETS -> IBC -> IPCA -> SELIC -> INAD
#
# Leitura contemporânea:
# - Bets não reage dentro do mês às demais variáveis;
# - IBC pode reagir contemporaneamente a Bets;
# - IPCA pode reagir contemporaneamente a Bets/IBC;
# - Selic pode reagir contemporaneamente a Bets/IBC/IPCA;
# - inadimplência pode reagir contemporaneamente a todas as anteriores.
#
# Como a ordenação é uma hipótese forte, estimamos também uma ordem alternativa.
# =============================================================================

ORDEM_PRINCIPAL <- c(
  "bets",
  "ibc",
  "ipca",
  "selic",
  "inad"
)

res_principal <- rodar_svar(
  nome_modelo = "01_principal_selic",
  ordem = ORDEM_PRINCIPAL,
  choque = "bets"
)

# =============================================================================
# 12. ROBUSTEZ – ORDEM ALTERNATIVA
# =============================================================================
#
# IBC -> IPCA -> SELIC -> BETS -> INAD
#
# Nessa ordenação, Bets pode responder contemporaneamente à atividade, inflação
# e política monetária. Assim, o choque estrutural de Bets é a inovação em Bets
# ortogonal às condições macroeconômicas contemporâneas anteriores na ordem.
# =============================================================================

res_ordem_alt <- NULL

if (RODAR_ORDEM_ALTERNATIVA) {
  ORDEM_ALTERNATIVA <- c(
    "ibc",
    "ipca",
    "selic",
    "bets",
    "inad"
  )

  res_ordem_alt <- rodar_svar(
    nome_modelo = "02_ordem_alternativa_selic",
    ordem = ORDEM_ALTERNATIVA,
    choque = "bets"
  )
}

# =============================================================================
# 13. COMPARAÇÃO DA IDENTIFICAÇÃO – ORDEM PRINCIPAL x ALTERNATIVA
# =============================================================================

if (
  !is.null(
    res_ordem_alt
  )
) {
  comparar_ordem <- dplyr::bind_rows(
    res_principal$irf_display %>%
      filter(
        resposta == "inad"
      ) %>%
      mutate(
        identificacao = "Bets → IBC → IPCA → Selic → Inad"
      ),

    res_ordem_alt$irf_display %>%
      filter(
        resposta == "inad"
      ) %>%
      mutate(
        identificacao = "IBC → IPCA → Selic → Bets → Inad"
      )
  )

  write.csv(
    comparar_ordem,
    file.path(
      DIR_OUT,
      "comparacao_ordens_IRF_inad.csv"
    ),
    row.names = FALSE
  )

  g_compare_ordem <- ggplot(
    comparar_ordem,
    aes(
      x = horizonte,
      y = irf,
      linetype = identificacao
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = 3
    ) +
    geom_line(
      linewidth = 0.9
    ) +
    labs(
      title = "Robustez da identificação: choque de Bets → inadimplência",
      subtitle = "Mesmas variáveis; apenas a ordenação de Cholesky é alterada",
      x = "Meses após o choque",
      y = ifelse(
        TRANSFORM_INAD == "nivel",
        "Resposta da inadimplência (p.p.)",
        "Efeito acumulado sobre a inadimplência (p.p.)"
      ),
      linetype = NULL
    ) +
    theme_minimal(
      base_size = 12
    )

  ggsave(
    file.path(
      DIR_OUT,
      "comparacao_ordens_IRF_inad.png"
    ),
    g_compare_ordem,
    width = 9,
    height = 5.5,
    dpi = 300
  )
}
# =============================================================================
# 14. TABELAS CONSOLIDADAS
# =============================================================================

lista_resultados <- list(
  res_principal,
  res_ordem_alt
)

lista_resultados <- lista_resultados[
  !vapply(
    lista_resultados,
    is.null,
    logical(1)
  )
]

diagnosticos_todos <- dplyr::bind_rows(
  lapply(
    lista_resultados,
    function(x) {
      x$diagnosticos %>%
        mutate(
          ordem = paste(
            x$ordem,
            collapse = " -> "
          )
        )
    }
  )
)

resumos_irf_todos <- dplyr::bind_rows(
  lapply(
    lista_resultados,
    function(x) {
      x$resumo_irf
    }
  )
)

fevd_bets_todos <- dplyr::bind_rows(
  lapply(
    lista_resultados,
    function(x) {
      x$fevd_bets_inad %>%
        mutate(
          modelo = x$nome,
          .before = 1
        )
    }
  )
)

write.csv(
  diagnosticos_todos,
  file.path(
    DIR_OUT,
    "diagnosticos_todos_modelos.csv"
  ),
  row.names = FALSE
)

write.csv(
  resumos_irf_todos,
  file.path(
    DIR_OUT,
    "resumo_IRF_Bets_Inad_todos_modelos.csv"
  ),
  row.names = FALSE
)

write.csv(
  fevd_bets_todos,
  file.path(
    DIR_OUT,
    "FEVD_Bets_Inad_todos_modelos.csv"
  ),
  row.names = FALSE
)
# =============================================================================
# 15. EXCEL CONSOLIDADO
# =============================================================================

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb,
  "Base_SVAR"
)

openxlsx::writeData(
  wb,
  "Base_SVAR",
  base_svar
)

openxlsx::addWorksheet(
  wb,
  "Estacionariedade"
)

openxlsx::writeData(
  wb,
  "Estacionariedade",
  testes_estacionariedade
)

openxlsx::addWorksheet(
  wb,
  "Diagnosticos"
)

openxlsx::writeData(
  wb,
  "Diagnosticos",
  diagnosticos_todos
)

openxlsx::addWorksheet(
  wb,
  "Resumo_IRF"
)

openxlsx::writeData(
  wb,
  "Resumo_IRF",
  resumos_irf_todos
)

openxlsx::addWorksheet(
  wb,
  "FEVD_Bets_Inad"
)

openxlsx::writeData(
  wb,
  "FEVD_Bets_Inad",
  fevd_bets_todos
)

# IRF principal
openxlsx::addWorksheet(
  wb,
  "IRF_Principal"
)

openxlsx::writeData(
  wb,
  "IRF_Principal",
  res_principal$irf_display
)

# FEVD completa principal
openxlsx::addWorksheet(
  wb,
  "FEVD_Principal"
)

openxlsx::writeData(
  wb,
  "FEVD_Principal",
  res_principal$fevd_inad
)

# Matriz de impacto principal
impacto_principal <- as.data.frame(
  res_principal$matriz_impacto
)

impacto_principal <- tibble::rownames_to_column(
  impacto_principal,
  var = "resposta"
)

openxlsx::addWorksheet(
  wb,
  "Impacto_Cholesky"
)

openxlsx::writeData(
  wb,
  "Impacto_Cholesky",
  impacto_principal
)

# Robustez de ordem
if (
  !is.null(
    res_ordem_alt
  )
) {
  openxlsx::addWorksheet(
    wb,
    "IRF_Ordem_Alt"
  )

  openxlsx::writeData(
    wb,
    "IRF_Ordem_Alt",
    res_ordem_alt$irf_display
  )
}

openxlsx::saveWorkbook(
  wb,
  file.path(
    DIR_OUT,
    "Resultados_SVAR_Itau_Selic.xlsx"
  ),
  overwrite = TRUE
)
# =============================================================================
# 16. RESUMO FINAL
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("SVAR ITAÚ – SELIC – FINALIZADO\n")
cat("============================================================\n")

cat(
  "\nAmostra:",
  as.character(
    min(
      base_svar$data
    )
  ),
  "a",
  as.character(
    max(
      base_svar$data
    )
  )
)

cat(
  "\nObservações:",
  nrow(
    base_svar
  )
)

cat(
  "\nTransformação Bets:",
  TRANSFORM_BETS
)

cat(
  "\nTransformação Selic:",
  TRANSFORM_SELIC
)

cat(
  "\nTransformação inadimplência:",
  TRANSFORM_INAD
)

cat(
  "\nModelo principal:",
  paste(
    ORDEM_PRINCIPAL,
    collapse = " -> "
  )
)

cat(
  "\nLag principal:",
  res_principal$p
)

cat(
  "\nMaior raiz principal:",
  round(
    res_principal$diagnosticos$maior_raiz,
    4
  )
)

cat(
  "\nResultados salvos em:",
  normalizePath(
    DIR_OUT,
    winslash = "/",
    mustWork = FALSE
  )
)

cat("\n============================================================\n")

print(
  diagnosticos_todos,
  n = Inf
)

cat("\nResumo da IRF Bets -> Inadimplência:\n")

print(
  resumos_irf_todos,
  n = Inf
)

cat("\nFEVD: parcela da variância do erro de previsão da inadimplência")
cat("\nexplicada pelo choque de Bets:\n")

print(
  fevd_bets_todos,
  n = Inf
)

# =============================================================================
# FIM
# =============================================================================

print(
  diagnosticos_todos %>%
    dplyr::select(
      modelo,
      p,
      maior_raiz,
      serial_ES_p,
      normalidade_JB_p,
      ARCH_p
    ),
  n = Inf
)