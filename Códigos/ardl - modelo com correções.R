# =============================================================================
# PROJETO V – ITAÚ / SFN
# ARDL COM A MESMA ESPECIFICAÇÃO E AS MESMAS VARIÁVEIS DO SVAR
# =============================================================================
#
# OBJETIVO
# --------
# Estimar, de forma univariada, a equação da inadimplência correspondente ao
# VAR reduzido usado no SVAR.
#
# Mesmas variáveis e mesmas transformações do SVAR:
#
#   BETS  = 100 * Delta log(GGR real mensal)
#   IBC   = 100 * Delta log(IBC-Br com ajuste sazonal)
#   IPCA  = inflação mensal (%)
#   SELIC = Delta da Meta Selic de fim de mês (p.p.)
#   INAD  = Delta da inadimplência PF até 10 SM (p.p.)
#
# O SVAR utiliza p = 2. Para reproduzir a equação reduzida da inadimplência:
#
#   ΔInad_t =
#       a
#     + phi1 ΔInad_(t-1) + phi2 ΔInad_(t-2)
#     + b1 Bets_(t-1)    + b2 Bets_(t-2)
#     + c1 IBC_(t-1)     + c2 IBC_(t-2)
#     + d1 IPCA_(t-1)    + d2 IPCA_(t-2)
#     + e1 ΔSelic_(t-1)  + e2 ΔSelic_(t-2)
#     + erro_t
#
# IMPORTANTE
# ----------
# 1) NÃO há Bounds/ECM neste script. A especificação é propositalmente de curto
#    prazo e usa as transformações estacionárias do SVAR.
# 2) Não interpretar a soma dos coeficientes de Bets como efeito de longo prazo.
# 3) O GGR nacional não mede diretamente a exposição das famílias <= 10 SM.
# 4) A série de Bets 2021-2025 contém valores estimados/construídos.
# 5) Não há regressoras contemporâneas porque o VAR reduzido contém apenas lags.
#    A contemporaneidade do SVAR vem da identificação estrutural/Cholesky.
# =============================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260930)

# =============================================================================
# 0. CONFIGURAÇÕES
# =============================================================================

DATA_INICIO_DESEJADO <- as.Date("2020-01-01")
DATA_FIM_DESEJADO    <- as.Date("2026-07-31")

# Mesma escolha do SVAR
P_FIXO <- 2

# Holdout opcional para previsão one-step-ahead
N_HOLDOUT <- 12

# Caminhos – iguais aos usados no SVAR
arquivo_inad_10sm <- paste0(
  "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/",
  "Inadimplência 10sm/Base_Final_Inadimplencia_PF.xlsx"
)

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

DIR_OUT <- "C:/Users/carlo/Downloads/output_ARDL_mesma_especificacao_SVAR"

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
  "ARDL",
  "tseries",
  "lmtest",
  "sandwich",
  "strucchange",
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
  library(ARDL)
  library(tseries)
  library(lmtest)
  library(sandwich)
  library(strucchange)
  library(ggplot2)
  library(openxlsx)
})

# =============================================================================
# 2. FUNÇÕES DE IMPORTAÇÃO
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
    
    if (is.finite(med) && med > 20000 && med < 80000) {
      return(
        lubridate::floor_date(
          as.Date(xx, origin = "1899-12-30"),
          "month"
        )
      )
    }
    
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
  
  idx1 <- grepl("^\\d{4}[-/]\\d{1,2}$", s)
  if (any(idx1, na.rm = TRUE)) {
    ss <- gsub("/", "-", s[idx1])
    out[idx1] <- as.Date(paste0(ss, "-01"))
  }
  
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

parse_data_diaria <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  
  if (is.numeric(x)) {
    xx <- as.numeric(x)
    med <- suppressWarnings(median(xx, na.rm = TRUE))
    
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
  
  df <- df %>%
    arrange(data)
  
  if (anyDuplicated(df$data)) {
    print(
      df %>%
        count(data) %>%
        filter(n > 1)
    )
    
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
      if (length(primeira) == 0 || is.na(primeira[1])) {
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

encontrar_coluna <- function(df, alternativas, nome_logico) {
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

# =============================================================================
# 3. LEITORES ESPECÍFICOS
# =============================================================================

# -----------------------------------------------------------------------------
# SELIC META – última observação válida de cada mês
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
    transmute(
      data_original = parse_data_diaria(
        .data[[col_data]]
      ),
      selic = suppressWarnings(
        parse_numero(
          .data[[col_selic]]
        )
      )
    ) %>%
    filter(
      !is.na(data_original),
      !is.na(selic)
    ) %>%
    mutate(
      data = floor_date(
        data_original,
        "month"
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
    group_by(data) %>%
    arrange(
      data_original,
      .by_group = TRUE
    ) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(
      data,
      selic
    ) %>%
    arrange(data)
  
  validar_serie_mensal(
    out,
    "selic"
  )
}

# -----------------------------------------------------------------------------
# IPCA histórico IBGE
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# BETS – GGR mensal
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# IBC-Br – download SGS em blocos
# -----------------------------------------------------------------------------

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
          " não retornou a coluna 'date'."
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
            "."
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
  
  resultado <- bind_rows(
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

ipca_df <- ler_ipca_ibge(
  arquivo_ipca,
  aba_ipca
)

bets_df <- ler_bets_ggr_mensal(
  arquivo_bets,
  aba_bets
)

# Alguns meses extras são necessários para construir Delta log(IBC)
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
# 5. BASE MESTRA E MESMAS TRANSFORMAÇÕES DO SVAR
# =============================================================================

cat("\n============================================================")
cat("\n2. CONSTRUINDO BASE E TRANSFORMAÇÕES")
cat("\n============================================================\n")

base_master <- list(
  inad_10sm,
  selic_df,
  ipca_df,
  bets_df,
  ibc_df
) %>%
  purrr::reduce(
    full_join,
    by = "data"
  ) %>%
  arrange(data)

# Deflator de Bets com IPCA; referência dezembro/2025
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
    # Bets reais
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
    
    # Mesma Bets do SVAR
    dlog_bets_real =
      100 *
      (
        log_bets_real -
          lag(
            log_bets_real
          )
      ),
    
    # Mesmo IBC do SVAR
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
          lag(
            log_ibc
          )
      ),
    
    # Mesma Selic do SVAR
    d_selic =
      selic -
      lag(
        selic
      ),
    
    # Mesma inadimplência do SVAR
    d_inad =
      inad_pf_10sm -
      lag(
        inad_pf_10sm
      )
  )

# Renomeamos exatamente como no vetor do SVAR
base_model <- base_master %>%
  arrange(data) %>%
  transmute(
    data,
    bets = dlog_bets_real,
    ibc  = dlog_ibc,
    ipca = ipca,
    selic = d_selic,
    inad = d_inad
  ) %>%
  filter(
    data >= DATA_INICIO_DESEJADO,
    data <= DATA_FIM_DESEJADO
  )

# Mesma amostra comum do SVAR
base_svar_ardl <- base_model %>%
  drop_na() %>%
  arrange(data)

if (nrow(base_svar_ardl) < 36) {
  stop(
    "A amostra efetiva ficou com apenas ",
    nrow(base_svar_ardl),
    " observações."
  )
}

# Verificação de meses consecutivos
meses_esperados <- seq.Date(
  min(base_svar_ardl$data),
  max(base_svar_ardl$data),
  by = "month"
)

meses_faltantes <- setdiff(
  meses_esperados,
  base_svar_ardl$data
)

if (length(meses_faltantes) > 0) {
  stop(
    "Há meses faltantes após o merge: ",
    paste(
      format(
        meses_faltantes,
        "%Y-%m"
      ),
      collapse = ", "
    )
  )
}

cat(
  "\nAmostra efetiva:",
  format(
    min(base_svar_ardl$data),
    "%Y-%m"
  ),
  "a",
  format(
    max(base_svar_ardl$data),
    "%Y-%m"
  ),
  "\nObservações antes das defasagens do modelo:",
  nrow(base_svar_ardl),
  "\n"
)

# =============================================================================
# 6. TESTES DE ESTACIONARIEDADE NAS VARIÁVEIS TRANSFORMADAS
# =============================================================================
#
# ADF e PP: H0 = raiz unitária
# KPSS:     H0 = estacionariedade
#
# Regra operacional igual à usada no SVAR:
# estacionária se pelo menos 2 de 3 testes apontarem para I(0).
# =============================================================================

teste_estacionariedade <- function(x, nome) {
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
    as.numeric(adf$p.value)
  }
  
  pp_p <- if (is.null(pp)) {
    NA_real_
  } else {
    as.numeric(pp$p.value)
  }
  
  kpss_p <- if (is.null(kpss)) {
    NA_real_
  } else {
    as.numeric(kpss$p.value)
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

tab_estacionariedade <- purrr::imap_dfr(
  base_svar_ardl %>%
    select(
      -data
    ),
  ~ teste_estacionariedade(
    .x,
    .y
  )
)

cat("\n============================================================")
cat("\n3. ESTACIONARIEDADE – VARIÁVEIS TRANSFORMADAS")
cat("\n============================================================\n")

print(
  tab_estacionariedade,
  n = Inf
)

if (
  any(
    tab_estacionariedade$estacionaria_maioria %in% FALSE
  )
) {
  warning(
    paste0(
      "Ao menos uma variável transformada não parece I(0) pela regra conjunta. ",
      "Não interprete o ARDL antes de revisar a tabela de estacionariedade."
    )
  )
}

# =============================================================================
# 7. GRÁFICOS DAS SÉRIES TRANSFORMADAS
# =============================================================================

base_long <- base_svar_ardl %>%
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
    title = "ARDL – mesmas séries transformadas do SVAR",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  file.path(
    DIR_OUT,
    "series_ARDL_mesma_especificacao_SVAR.png"
  ),
  g_series,
  width = 10,
  height = 9,
  dpi = 300
)

# =============================================================================
# 8. CONSTRUIR REGRESSORES DEFASADOS
# =============================================================================
#
# O VAR(2) usa X_(t-1) e X_(t-2), sem X_t.
#
# Para o pacote ARDL:
# - criamos X_L1 = X_(t-1);
# - usamos q = 1 sobre X_L1;
# - isso produz X_(t-1) e X_(t-2).
#
# Para auditoria, também montamos explicitamente todos os lags 1 e 2.
# =============================================================================

base_ardl <- base_svar_ardl %>%
  arrange(data) %>%
  mutate(
    bets_L1  = lag(bets, 1),
    ibc_L1   = lag(ibc, 1),
    ipca_L1  = lag(ipca, 1),
    selic_L1 = lag(selic, 1)
  )

# Base explícita para auditoria e previsão
base_reg <- base_svar_ardl %>%
  arrange(data) %>%
  mutate(
    inad_L1 = lag(inad, 1),
    inad_L2 = lag(inad, 2),
    
    bets_L1 = lag(bets, 1),
    bets_L2 = lag(bets, 2),
    
    ibc_L1 = lag(ibc, 1),
    ibc_L2 = lag(ibc, 2),
    
    ipca_L1 = lag(ipca, 1),
    ipca_L2 = lag(ipca, 2),
    
    selic_L1 = lag(selic, 1),
    selic_L2 = lag(selic, 2)
  ) %>%
  drop_na() %>%
  arrange(data)

# =============================================================================
# 9. ARDL FIXO – MESMA DINÂMICA DO SVAR(2)
# =============================================================================

inicio_ts <- min(base_ardl$data)
fim_ts    <- max(base_ardl$data)

ts_ardl <- ts(
  base_ardl %>%
    select(
      inad,
      bets_L1,
      ibc_L1,
      ipca_L1,
      selic_L1
    ),
  start = c(
    year(inicio_ts),
    month(inicio_ts)
  ),
  frequency = 12
)

# Ordem:
# p = 2 para inad
# q = 1 para cada X_L1 -> lags efetivos t-1 e t-2 da variável original
modelo_ardl <- ARDL::ardl(
  inad ~
    bets_L1 +
    ibc_L1 +
    ipca_L1 +
    selic_L1,
  data = ts_ardl,
  order = c(
    2,
    1,
    1,
    1,
    1
  )
)

cat("\n============================================================")
cat("\n4. ARDL FIXO – MESMA DINÂMICA DO SVAR(2)")
cat("\n============================================================\n")

print(
  summary(
    modelo_ardl
  )
)

# Converter para lm
lm_ardl <- tryCatch(
  ARDL::to_lm(
    modelo_ardl,
    fix_names = TRUE,
    data_class = "ts"
  ),
  error = function(e) modelo_ardl
)

# =============================================================================
# 10. AUDITORIA – EQUAÇÃO EXPLÍCITA EQUIVALENTE
# =============================================================================
#
# Esta regressão OLS deve representar exatamente:
#
# inad_t ~ inad_(t-1) + inad_(t-2)
#        + bets_(t-1) + bets_(t-2)
#        + ibc_(t-1) + ibc_(t-2)
#        + ipca_(t-1) + ipca_(t-2)
#        + selic_(t-1) + selic_(t-2)
#
# Ela serve para conferir a indexação dos lags do pacote ARDL.
# =============================================================================

modelo_equiv <- lm(
  inad ~
    inad_L1 +
    inad_L2 +
    bets_L1 +
    bets_L2 +
    ibc_L1 +
    ibc_L2 +
    ipca_L1 +
    ipca_L2 +
    selic_L1 +
    selic_L2,
  data = base_reg
)

cat("\n============================================================")
cat("\n5. AUDITORIA – OLS EXPLÍCITO EQUIVALENTE")
cat("\n============================================================\n")

print(
  summary(
    modelo_equiv
  )
)

# =============================================================================
# 11. ERROS-PADRÃO HAC / NEWEY-WEST
# =============================================================================

lag_hac <- max(
  1,
  min(
    12,
    floor(
      nobs(modelo_equiv)^(1 / 4)
    )
  )
)

V_HAC <- sandwich::NeweyWest(
  modelo_equiv,
  lag = lag_hac,
  prewhite = FALSE,
  adjust = TRUE
)

coef_HAC_matrix <- lmtest::coeftest(
  modelo_equiv,
  vcov. = V_HAC
)

coef_HAC <- tibble(
  termo = rownames(
    coef_HAC_matrix
  ),
  coeficiente = coef_HAC_matrix[, 1],
  erro_padrao_HAC = coef_HAC_matrix[, 2],
  t_HAC = coef_HAC_matrix[, 3],
  p_valor_HAC = coef_HAC_matrix[, 4],
  significancia = case_when(
    coef_HAC_matrix[, 4] < 0.01 ~ "***",
    coef_HAC_matrix[, 4] < 0.05 ~ "**",
    coef_HAC_matrix[, 4] < 0.10 ~ "*",
    TRUE ~ ""
  )
)

cat("\n============================================================")
cat("\n6. COEFICIENTES HAC")
cat("\n============================================================\n")

print(
  coef_HAC,
  n = Inf
)

# =============================================================================
# 12. WALD-HAC CONJUNTO – BETS t-1 E t-2
# =============================================================================

wald_hac_bloco <- function(
    modelo,
    V,
    padrao
) {
  b <- coef(
    modelo
  )
  
  idx <- grep(
    padrao,
    names(b),
    fixed = TRUE
  )
  
  if (length(idx) == 0) {
    return(
      tibble(
        bloco = padrao,
        n_restricoes = 0,
        estatistica_Wald = NA_real_,
        F_aprox = NA_real_,
        p_valor = NA_real_
      )
    )
  }
  
  R <- matrix(
    0,
    nrow = length(idx),
    ncol = length(b)
  )
  
  for (j in seq_along(idx)) {
    R[j, idx[j]] <- 1
  }
  
  rb <- R %*% b
  
  RVRT <- R %*%
    V %*%
    t(R)
  
  Wald <- tryCatch(
    as.numeric(
      t(rb) %*%
        solve(RVRT) %*%
        rb
    ),
    error = function(e) NA_real_
  )
  
  q <- length(idx)
  
  F_aprox <- Wald / q
  
  p_valor <- if (is.finite(F_aprox)) {
    pf(
      F_aprox,
      df1 = q,
      df2 = df.residual(modelo),
      lower.tail = FALSE
    )
  } else {
    NA_real_
  }
  
  tibble(
    bloco = padrao,
    n_restricoes = q,
    estatistica_Wald = Wald,
    F_aprox = F_aprox,
    p_valor = p_valor
  )
}

wald_bets <- wald_hac_bloco(
  modelo_equiv,
  V_HAC,
  "bets_L"
)

cat("\n============================================================")
cat("\n7. WALD-HAC – BETS t-1 E t-2")
cat("\n============================================================\n")

print(
  wald_bets,
  n = Inf
)

# =============================================================================
# 13. SOMA DOS DOIS COEFICIENTES DE BETS – EFEITO DE CURTO PRAZO
# =============================================================================
#
# NÃO é multiplicador de longo prazo.
# Apenas testa H0: beta_Bets,t-1 + beta_Bets,t-2 = 0.
# =============================================================================

efeito_soma_hac <- function(
    modelo,
    V,
    termos
) {
  b <- coef(
    modelo
  )
  
  idx <- match(
    termos,
    names(b)
  )
  
  idx <- idx[
    !is.na(idx)
  ]
  
  if (length(idx) == 0) {
    return(
      tibble(
        efeito_soma = NA_real_,
        erro_padrao = NA_real_,
        t = NA_real_,
        p_valor = NA_real_
      )
    )
  }
  
  w <- rep(
    0,
    length(b)
  )
  
  w[idx] <- 1
  
  est <- sum(
    b[idx]
  )
  
  se <- sqrt(
    as.numeric(
      t(w) %*%
        V %*%
        w
    )
  )
  
  tt <- est / se
  
  pv <- 2 *
    pt(
      abs(tt),
      df = df.residual(modelo),
      lower.tail = FALSE
    )
  
  tibble(
    efeito_soma = est,
    erro_padrao = se,
    t = tt,
    p_valor = pv
  )
}

soma_bets <- efeito_soma_hac(
  modelo_equiv,
  V_HAC,
  c(
    "bets_L1",
    "bets_L2"
  )
)

cat("\n============================================================")
cat("\n8. SOMA DOS EFEITOS DE BETS EM t-1 E t-2")
cat("\n============================================================\n")

print(
  soma_bets,
  n = Inf
)

# =============================================================================
# 14. VIF – REGRESSORAS EXÓGENAS
# =============================================================================
#
# O VIF é calculado nos lags explícitos das quatro regressoras:
# Bets, IBC, IPCA e Selic.
# Não incluímos os lags da dependente nesta tabela.
# =============================================================================

vif_manual <- function(df_X) {
  X <- as.data.frame(
    df_X
  )
  
  sds <- vapply(
    X,
    sd,
    numeric(1),
    na.rm = TRUE
  )
  
  X <- X[
    ,
    is.finite(sds) &
      sds > 0,
    drop = FALSE
  ]
  
  if (ncol(X) < 2) {
    return(
      list(
        tabela = tibble(
          termo = names(X),
          VIF = NA_real_,
          tolerance = NA_real_
        ),
        condition_number = NA_real_
      )
    )
  }
  
  vifs <- sapply(
    seq_len(
      ncol(X)
    ),
    function(j) {
      y <- X[[j]]
      z <- X[
        ,
        -j,
        drop = FALSE
      ]
      
      fit <- lm(
        y ~ .,
        data = cbind(
          y = y,
          z
        )
      )
      
      r2 <- summary(
        fit
      )$r.squared
      
      if (!is.finite(r2)) {
        return(
          NA_real_
        )
      }
      
      if (r2 >= 1) {
        return(
          Inf
        )
      }
      
      1 / (1 - r2)
    }
  )
  
  cn <- tryCatch(
    kappa(
      scale(
        as.matrix(
          X
        )
      ),
      exact = TRUE
    ),
    error = function(e) NA_real_
  )
  
  list(
    tabela = tibble(
      termo = names(X),
      VIF = as.numeric(
        vifs
      ),
      tolerance = ifelse(
        is.finite(
          vifs
        ),
        1 / vifs,
        0
      )
    ),
    condition_number = cn
  )
}

X_exog <- base_reg %>%
  select(
    bets_L1,
    bets_L2,
    ibc_L1,
    ibc_L2,
    ipca_L1,
    ipca_L2,
    selic_L1,
    selic_L2
  )

vif_exog <- vif_manual(
  X_exog
)

cat("\n============================================================")
cat("\n9. VIF – REGRESSORAS EXÓGENAS")
cat("\n============================================================\n")

print(
  vif_exog$tabela,
  n = Inf
)

cat(
  "\nCondition Number:",
  round(
    vif_exog$condition_number,
    3
  ),
  "\n"
)

# =============================================================================
# 15. DIAGNÓSTICOS
# =============================================================================

n_modelo <- nobs(
  modelo_equiv
)

lag_diag <- max(
  1,
  min(
    12,
    floor(
      n_modelo / 5
    )
  )
)

bg <- tryCatch(
  lmtest::bgtest(
    modelo_equiv,
    order = lag_diag,
    type = "Chisq"
  ),
  error = function(e) NULL
)

lj <- tryCatch(
  Box.test(
    residuals(
      modelo_equiv
    ),
    lag = lag_diag,
    type = "Ljung-Box",
    fitdf = min(
      P_FIXO,
      lag_diag - 1
    )
  ),
  error = function(e) NULL
)

bp <- tryCatch(
  lmtest::bptest(
    modelo_equiv
  ),
  error = function(e) NULL
)

jb <- tryCatch(
  tseries::jarque.bera.test(
    residuals(
      modelo_equiv
    )
  ),
  error = function(e) NULL
)

reset <- tryCatch(
  lmtest::resettest(
    modelo_equiv,
    power = 2:3,
    type = "fitted"
  ),
  error = function(e) NULL
)

diagnosticos <- bind_rows(
  tibble(
    teste = "Breusch-Godfrey",
    H0 = "Ausência de autocorrelação serial",
    estatistica = if (is.null(bg)) NA_real_ else as.numeric(bg$statistic),
    p_valor = if (is.null(bg)) NA_real_ else as.numeric(bg$p.value)
  ),
  tibble(
    teste = "Ljung-Box",
    H0 = "Resíduos sem autocorrelação conjunta",
    estatistica = if (is.null(lj)) NA_real_ else as.numeric(lj$statistic),
    p_valor = if (is.null(lj)) NA_real_ else as.numeric(lj$p.value)
  ),
  tibble(
    teste = "Breusch-Pagan",
    H0 = "Homoscedasticidade",
    estatistica = if (is.null(bp)) NA_real_ else as.numeric(bp$statistic),
    p_valor = if (is.null(bp)) NA_real_ else as.numeric(bp$p.value)
  ),
  tibble(
    teste = "Jarque-Bera",
    H0 = "Normalidade dos resíduos",
    estatistica = if (is.null(jb)) NA_real_ else as.numeric(jb$statistic),
    p_valor = if (is.null(jb)) NA_real_ else as.numeric(jb$p.value)
  ),
  tibble(
    teste = "Ramsey RESET",
    H0 = "Forma funcional adequada",
    estatistica = if (is.null(reset)) NA_real_ else as.numeric(reset$statistic),
    p_valor = if (is.null(reset)) NA_real_ else as.numeric(reset$p.value)
  )
) %>%
  mutate(
    conclusao_5pct = ifelse(
      is.na(
        p_valor
      ),
      "Teste indisponível",
      ifelse(
        p_valor < 0.05,
        "Rejeita H0 a 5%",
        "Não rejeita H0 a 5%"
      )
    )
  )

cat("\n============================================================")
cat("\n10. DIAGNÓSTICOS")
cat("\n============================================================\n")

print(
  diagnosticos,
  n = Inf
)

# =============================================================================
# 16. ESTABILIDADE – CUSUM E MOSUM
# =============================================================================

estabilidade <- function(modelo) {
  mf <- model.frame(
    modelo
  )
  
  form <- formula(
    modelo
  )
  
  cusum_obj <- tryCatch(
    strucchange::efp(
      form,
      data = mf,
      type = "Rec-CUSUM"
    ),
    error = function(e) NULL
  )
  
  mosum_obj <- tryCatch(
    strucchange::efp(
      form,
      data = mf,
      type = "OLS-MOSUM"
    ),
    error = function(e) NULL
  )
  
  sct_cusum <- if (is.null(cusum_obj)) {
    NULL
  } else {
    tryCatch(
      strucchange::sctest(
        cusum_obj
      ),
      error = function(e) NULL
    )
  }
  
  sct_mosum <- if (is.null(mosum_obj)) {
    NULL
  } else {
    tryCatch(
      strucchange::sctest(
        mosum_obj
      ),
      error = function(e) NULL
    )
  }
  
  testes <- bind_rows(
    tibble(
      teste = "CUSUM",
      estatistica = if (is.null(sct_cusum)) NA_real_ else as.numeric(sct_cusum$statistic),
      p_valor = if (is.null(sct_cusum)) NA_real_ else as.numeric(sct_cusum$p.value)
    ),
    tibble(
      teste = "MOSUM",
      estatistica = if (is.null(sct_mosum)) NA_real_ else as.numeric(sct_mosum$statistic),
      p_valor = if (is.null(sct_mosum)) NA_real_ else as.numeric(sct_mosum$p.value)
    )
  )
  
  list(
    cusum = cusum_obj,
    mosum = mosum_obj,
    testes = testes
  )
}

estab <- estabilidade(
  modelo_equiv
)

cat("\n============================================================")
cat("\n11. ESTABILIDADE")
cat("\n============================================================\n")

print(
  estab$testes,
  n = Inf
)

if (!is.null(estab$cusum)) {
  png(
    file.path(
      DIR_OUT,
      "CUSUM_ARDL_SVAR.png"
    ),
    width = 1200,
    height = 800,
    res = 140
  )
  
  plot(
    estab$cusum,
    main = "CUSUM – ARDL com especificação do SVAR"
  )
  
  dev.off()
}

if (!is.null(estab$mosum)) {
  png(
    file.path(
      DIR_OUT,
      "MOSUM_ARDL_SVAR.png"
    ),
    width = 1200,
    height = 800,
    res = 140
  )
  
  plot(
    estab$mosum,
    main = "MOSUM – ARDL com especificação do SVAR"
  )
  
  dev.off()
}

# =============================================================================
# 17. MÉTRICAS IN-SAMPLE
# =============================================================================

rmse <- sqrt(
  mean(
    residuals(
      modelo_equiv
    )^2,
    na.rm = TRUE
  )
)

mae <- mean(
  abs(
    residuals(
      modelo_equiv
    )
  ),
  na.rm = TRUE
)

metricas <- tibble(
  modelo = "ARDL-equação da inadimplência / mesma especificação SVAR",
  n = nobs(
    modelo_equiv
  ),
  AIC = AIC(
    modelo_equiv
  ),
  BIC = BIC(
    modelo_equiv
  ),
  R2 = summary(
    modelo_equiv
  )$r.squared,
  R2_ajustado = summary(
    modelo_equiv
  )$adj.r.squared,
  RMSE_in_sample = rmse,
  MAE_in_sample = mae
)

cat("\n============================================================")
cat("\n12. MÉTRICAS IN-SAMPLE")
cat("\n============================================================\n")

print(
  metricas,
  n = Inf
)

# =============================================================================
# 18. VALIDAÇÃO ONE-STEP-AHEAD – ÚLTIMOS 12 MESES
# =============================================================================
#
# Como os regressores são todos defasados, o one-step-ahead usa apenas
# informação disponível em t-1/t-2.
# =============================================================================

formula_equiv <- formula(
  modelo_equiv
)

avaliar_oos <- function(
    dados,
    formula,
    n_holdout = 12
) {
  n <- nrow(
    dados
  )
  
  if (n <= n_holdout + 15) {
    return(
      list(
        tabela = tibble(),
        metricas = tibble(
          RMSE_OOS = NA_real_,
          MAE_OOS = NA_real_
        )
      )
    )
  }
  
  inicio_teste <- n - n_holdout + 1
  
  previsoes <- vector(
    "list",
    n_holdout
  )
  
  for (
    j in seq_len(
      n_holdout
    )
  ) {
    idx_t <- inicio_teste + j - 1
    
    treino <- dados[
      seq_len(
        idx_t - 1
      ),
      ,
      drop = FALSE
    ]
    
    teste <- dados[
      idx_t,
      ,
      drop = FALSE
    ]
    
    mod <- lm(
      formula,
      data = treino
    )
    
    pred <- as.numeric(
      predict(
        mod,
        newdata = teste
      )
    )
    
    previsoes[[j]] <- tibble(
      data = teste$data,
      observado = teste$inad,
      previsto = pred,
      erro = teste$inad - pred
    )
  }
  
  tab <- bind_rows(
    previsoes
  )
  
  mets <- tibble(
    RMSE_OOS = sqrt(
      mean(
        tab$erro^2,
        na.rm = TRUE
      )
    ),
    MAE_OOS = mean(
      abs(
        tab$erro
      ),
      na.rm = TRUE
    )
  )
  
  list(
    tabela = tab,
    metricas = mets
  )
}

oos <- avaliar_oos(
  base_reg,
  formula_equiv,
  N_HOLDOUT
)

cat("\n============================================================")
cat("\n13. VALIDAÇÃO OOS – ONE-STEP-AHEAD")
cat("\n============================================================\n")

print(
  oos$metricas,
  n = Inf
)

# Benchmark AR(2) somente para Δ inadimplência
formula_ar2 <- inad ~
  inad_L1 +
  inad_L2

oos_ar2 <- avaliar_oos(
  base_reg,
  formula_ar2,
  N_HOLDOUT
)

comparacao_oos <- bind_rows(
  oos$metricas %>%
    mutate(
      modelo = "ARDL / mesmas variáveis do SVAR"
    ),
  oos_ar2$metricas %>%
    mutate(
      modelo = "Benchmark AR(2)"
    )
) %>%
  select(
    modelo,
    everything()
  )

cat("\nComparação OOS:\n")

print(
  comparacao_oos,
  n = Inf
)

# =============================================================================
# 19. EXPORTAÇÃO
# =============================================================================

write.csv(
  base_svar_ardl,
  file.path(
    DIR_OUT,
    "base_transformada_mesma_do_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  base_reg,
  file.path(
    DIR_OUT,
    "base_regressao_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  tab_estacionariedade,
  file.path(
    DIR_OUT,
    "estacionariedade_variaveis_transformadas.csv"
  ),
  row.names = FALSE
)

write.csv(
  coef_HAC,
  file.path(
    DIR_OUT,
    "coeficientes_HAC_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  wald_bets,
  file.path(
    DIR_OUT,
    "Wald_HAC_Bets_lags_1_2.csv"
  ),
  row.names = FALSE
)

write.csv(
  soma_bets,
  file.path(
    DIR_OUT,
    "soma_curto_prazo_Bets_lags_1_2.csv"
  ),
  row.names = FALSE
)

write.csv(
  vif_exog$tabela,
  file.path(
    DIR_OUT,
    "VIF_exogenas_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  diagnosticos,
  file.path(
    DIR_OUT,
    "diagnosticos_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  estab$testes,
  file.path(
    DIR_OUT,
    "estabilidade_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  metricas,
  file.path(
    DIR_OUT,
    "metricas_in_sample_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  oos$tabela,
  file.path(
    DIR_OUT,
    "previsoes_OOS_ARDL_SVAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  comparacao_oos,
  file.path(
    DIR_OUT,
    "comparacao_OOS_ARDL_SVAR_vs_AR2.csv"
  ),
  row.names = FALSE
)

# Workbook consolidado
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb,
  "Base_transformada"
)
openxlsx::writeData(
  wb,
  "Base_transformada",
  base_svar_ardl
)

openxlsx::addWorksheet(
  wb,
  "Base_regressao"
)
openxlsx::writeData(
  wb,
  "Base_regressao",
  base_reg
)

openxlsx::addWorksheet(
  wb,
  "Estacionariedade"
)
openxlsx::writeData(
  wb,
  "Estacionariedade",
  tab_estacionariedade
)

openxlsx::addWorksheet(
  wb,
  "Coef_HAC"
)
openxlsx::writeData(
  wb,
  "Coef_HAC",
  coef_HAC
)

openxlsx::addWorksheet(
  wb,
  "Wald_Bets"
)
openxlsx::writeData(
  wb,
  "Wald_Bets",
  wald_bets
)

openxlsx::addWorksheet(
  wb,
  "Soma_Bets"
)
openxlsx::writeData(
  wb,
  "Soma_Bets",
  soma_bets
)

openxlsx::addWorksheet(
  wb,
  "VIF"
)
openxlsx::writeData(
  wb,
  "VIF",
  vif_exog$tabela
)

openxlsx::addWorksheet(
  wb,
  "Diagnosticos"
)
openxlsx::writeData(
  wb,
  "Diagnosticos",
  diagnosticos
)

openxlsx::addWorksheet(
  wb,
  "Estabilidade"
)
openxlsx::writeData(
  wb,
  "Estabilidade",
  estab$testes
)

openxlsx::addWorksheet(
  wb,
  "Metricas"
)
openxlsx::writeData(
  wb,
  "Metricas",
  metricas
)

openxlsx::addWorksheet(
  wb,
  "OOS"
)
openxlsx::writeData(
  wb,
  "OOS",
  comparacao_oos
)

openxlsx::saveWorkbook(
  wb,
  file.path(
    DIR_OUT,
    "Resultados_ARDL_mesma_especificacao_SVAR.xlsx"
  ),
  overwrite = TRUE
)

# Objetos R
saveRDS(
  modelo_ardl,
  file.path(
    DIR_OUT,
    "modelo_ARDL.rds"
  )
)

saveRDS(
  modelo_equiv,
  file.path(
    DIR_OUT,
    "modelo_equacao_explicita.rds"
  )
)

capture.output(
  summary(
    modelo_ardl
  ),
  file = file.path(
    DIR_OUT,
    "summary_ARDL.txt"
  )
)

capture.output(
  summary(
    modelo_equiv
  ),
  file = file.path(
    DIR_OUT,
    "summary_equacao_explicita.txt"
  )
)

# =============================================================================
# 20. RESUMO FINAL NO CONSOLE
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("ARDL – MESMA ESPECIFICAÇÃO DO SVAR – FINALIZADO\n")
cat("============================================================\n")

cat(
  "\nAmostra transformada:",
  as.character(
    min(
      base_svar_ardl$data
    )
  ),
  "a",
  as.character(
    max(
      base_svar_ardl$data
    )
  )
)

cat(
  "\nN antes de aplicar os 2 lags:",
  nrow(
    base_svar_ardl
  )
)

cat(
  "\nN efetivo da regressão:",
  nobs(
    modelo_equiv
  )
)

cat(
  "\nEspecificação: ARDL com 2 lags de ΔInad e 2 lags de cada regressora."
)

cat(
  "\nVariáveis: Δlog Bets real, Δlog IBC-Br, IPCA, Δ Selic, Δ Inad."
)

cat(
  "\nBounds/ECM: NÃO APLICÁVEL nesta especificação estacionária de curto prazo."
)

cat(
  "\n\nWald conjunto Bets t-1/t-2:\n"
)

print(
  wald_bets,
  n = Inf
)

cat(
  "\nSoma dos coeficientes de Bets t-1/t-2:\n"
)

print(
  soma_bets,
  n = Inf
)

cat(
  "\nVIF das regressoras exógenas:\n"
)

print(
  vif_exog$tabela,
  n = Inf
)

cat(
  "\nCondition Number:",
  round(
    vif_exog$condition_number,
    3
  ),
  "\n"
)

cat(
  "\nDiagnósticos:\n"
)

print(
  diagnosticos,
  n = Inf
)

cat(
  "\nValidação fora da amostra:\n"
)

print(
  comparacao_oos,
  n = Inf
)

cat(
  "\nResultados salvos em:\n",
  normalizePath(
    DIR_OUT,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

cat("============================================================\n")
