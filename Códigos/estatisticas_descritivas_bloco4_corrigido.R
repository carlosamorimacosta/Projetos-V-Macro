# =============================================================================
# PROJETOS V – ITAÚ / SFN
# BLOCO 4 – ESTATÍSTICAS DESCRITIVAS
# Dimensões: financeira, comportamental, setorial e macroeconômica
# Jan/2021 a Dez/2025 (amostra principal compatível com Bets)
# =============================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999, error = NULL)

# =============================================================================
# 0. CONFIGURAÇÕES
# =============================================================================

DATA_INICIO <- as.Date("2021-01-01")
DATA_FIM    <- as.Date("2025-12-01")

# A PNAD utilizada no projeto é rendimento médio real. Se o arquivo mudar para
# rendimento nominal, altere para FALSE para permitir deflação posterior.
RENDA_JA_REAL <- TRUE

# O arquivo do BCB utilizado nos códigos anteriores contém taxas mensais (% a.m.).
# Mantemos a série mensal original e também calculamos a taxa efetiva anual.
JUROS_ARQUIVO_EH_MENSAL <- TRUE

# -----------------------------------------------------------------------------
# Caminhos informados
# -----------------------------------------------------------------------------

arquivo_bets <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Bets/Bets_GGR_Mensal_2021_2025_estimado.xlsx"
arquivo_renda <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Renda/PNAD Contínua - renda média - geral.xlsx"
arquivo_selic <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Selic Meta.csv"
arquivo_comprometimento <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/comprometimento da renda por juros.csv"
arquivo_juros_modalidades <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Taxas médias das op de crédito livre - modalidades - completa.csv"

arquivo_itau_atual <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Itaú/Planilha de Séries Históricas - Demonstrativos do Itaú.xlsx"
arquivo_itau_descontinuado <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Itaú/Planilha de Séries Históricas - (Descontinuada).xlsx"
arquivo_itau_historico <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Itaú/Planilha de Séries Históricas.xlsx"

arquivo_ipca <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/ipca_202606SerieHist.xls"
arquivo_endividamento <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/endividamento bc.csv"
arquivo_desemprego <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Desemprego Pnad.csv"
arquivo_inad_modalidades <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplência - PF - modalidades.csv"
arquivo_serasa <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Cópia de inadimplencia-do-consumidor-jul26 - atualizada - serasa.xlsx"

# A série PF até 10 SM é produzida pelo script "Inadimplência PF até 10 sm.R".
# Esse caminho vem do código anexado. Se o arquivo estiver em outra pasta,
# altere SOMENTE esta linha.
arquivo_inad_10sm <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplência 10sm/Base_Final_Inadimplencia_PF.xlsx"

# Saídas
DIR_OUT <- "C:/Users/carlo/Downloads/Projetos V - Macro/Saidas_Estatisticas_Descritivas_Bloco4"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

# =============================================================================
# 1. PACOTES
# =============================================================================

pacotes <- c(
  "tidyverse", "lubridate", "readxl", "readr", "openxlsx",
  "zoo", "stringi", "scales"
)

instalar_ausentes <- function(pkgs) {
  ausentes <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ausentes) > 0) install.packages(ausentes, dependencies = TRUE)
}

instalar_ausentes(pacotes)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(readr)
  library(openxlsx)
  library(zoo)
  library(stringi)
  library(scales)
})

# =============================================================================
# 2. FUNÇÕES AUXILIARES
# =============================================================================

normalizar_nome <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

parse_numero <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "NaN", "-", "--", "...", "null", "NULL", "n.d.", "n.d")] <- NA_character_

  prop_virgula <- mean(grepl(",", s), na.rm = TRUE)
  if (!is.finite(prop_virgula)) prop_virgula <- 0

  if (prop_virgula > 0.20) {
    out <- readr::parse_number(
      s,
      locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
      na = c("", "NA", "NaN", "-", "--", "...", "n.d.", "n.d")
    )
  } else {
    out <- readr::parse_number(
      s,
      locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
      na = c("", "NA", "NaN", "-", "--", "...", "n.d.", "n.d")
    )
  }

  as.numeric(out)
}

mes_pt_numero <- function(x) {
  s <- normalizar_nome(trimws(as.character(x)))
  s <- gsub("_", "", s)

  num <- suppressWarnings(as.integer(s))
  out <- ifelse(!is.na(num) & num >= 1 & num <= 12, num, NA_integer_)

  chave <- substr(s, 1, 3)
  mapa <- c(
    jan = 1, fev = 2, mar = 3, abr = 4, mai = 5, jun = 6,
    jul = 7, ago = 8, set = 9, out = 10, nov = 11, dez = 12
  )

  idx <- is.na(out) & chave %in% names(mapa)
  out[idx] <- unname(mapa[chave[idx]])
  as.integer(out)
}

parse_data_mensal <- function(x) {
  if (inherits(x, "Date")) return(floor_date(x, "month"))
  if (inherits(x, c("POSIXct", "POSIXt"))) return(floor_date(as.Date(x), "month"))

  if (is.numeric(x)) {
    xx <- as.numeric(x)
    med <- suppressWarnings(median(xx, na.rm = TRUE))

    # Serial Excel
    if (is.finite(med) && med > 20000 && med < 80000) {
      return(floor_date(as.Date(xx, origin = "1899-12-30"), "month"))
    }

    # YYYYMM
    if (all(is.na(xx) | (xx >= 190001 & xx <= 210012))) {
      s <- sprintf("%06d", as.integer(xx))
      return(as.Date(paste0(substr(s, 1, 4), "-", substr(s, 5, 6), "-01")))
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

  # MM/YYYY, MM-YYYY
  idx2 <- is.na(out) & grepl("^\\d{1,2}[-/]\\d{4}$", s)
  if (any(idx2, na.rm = TRUE)) {
    ss <- gsub("-", "/", s[idx2])
    p <- strsplit(ss, "/", fixed = TRUE)
    out[idx2] <- as.Date(vapply(
      p,
      function(z) sprintf("%04d-%02d-01", as.integer(z[2]), as.integer(z[1])),
      character(1)
    ))
  }

  # YYYYMM
  idx3 <- is.na(out) & grepl("^\\d{6}$", s)
  if (any(idx3, na.rm = TRUE)) {
    ss <- s[idx3]
    out[idx3] <- as.Date(paste0(substr(ss, 1, 4), "-", substr(ss, 5, 6), "-01"))
  }

  # jul/11, ago/11 etc.
  idx4 <- is.na(out) & grepl("^[[:alpha:]À-ÿ]{3,}/\\d{2}$", s)
  if (any(idx4, na.rm = TRUE)) {
    z <- strsplit(iconv(tolower(s[idx4]), to = "ASCII//TRANSLIT"), "/", fixed = TRUE)
    out[idx4] <- as.Date(vapply(z, function(v) {
      m <- mes_pt_numero(v[1])
      yy <- suppressWarnings(as.integer(v[2]))
      ano <- ifelse(yy <= 69, 2000 + yy, 1900 + yy)
      if (is.na(m) || is.na(ano)) return(NA_character_)
      sprintf("%04d-%02d-01", ano, m)
    }, character(1)))
  }

  # Demais formatos
  idx5 <- is.na(out) & !is.na(s)
  if (any(idx5)) {
    d <- suppressWarnings(lubridate::parse_date_time(
      s[idx5],
      orders = c(
        "Ymd", "Y-m-d", "Y/m/d",
        "dmy", "d/m/Y", "d-m-Y",
        "mdy", "m/d/Y", "m-d-Y",
        "my", "m/Y", "m-Y"
      ),
      quiet = TRUE
    ))
    out[idx5] <- as.Date(d)
  }

  floor_date(out, "month")
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

  d <- suppressWarnings(lubridate::parse_date_time(
    s,
    orders = c(
      "dmy", "d/m/Y", "d-m-Y",
      "Ymd", "Y-m-d", "Y/m/d",
      "mdy", "m/d/Y", "m-d-Y"
    ),
    quiet = TRUE
  ))

  as.Date(d)
}

parse_periodo_pnad <- function(periodo) {
  s <- iconv(tolower(trimws(as.character(periodo))), from = "", to = "ASCII//TRANSLIT")
  s <- stringr::str_squish(s)

  ano <- suppressWarnings(as.integer(stringr::str_extract(s, "(19|20)\\d{2}$")))
  bloco <- stringr::str_remove(s, "\\s*(19|20)\\d{2}$")
  mes_final <- stringr::str_extract(bloco, "[^-]+$")
  mes <- mes_pt_numero(mes_final)

  out <- rep(as.Date(NA), length(s))
  ok <- !is.na(ano) & !is.na(mes)
  out[ok] <- as.Date(sprintf("%04d-%02d-01", ano[ok], mes[ok]))
  out
}

detectar_encoding_csv <- function(caminho) {
  enc <- tryCatch(readr::guess_encoding(caminho, n_max = 1000), error = function(e) NULL)
  if (is.null(enc) || nrow(enc) == 0 || is.na(enc$encoding[1])) return("UTF-8")
  enc$encoding[1]
}

ler_csv_flex <- function(caminho) {
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)

  enc <- detectar_encoding_csv(caminho)
  primeira <- readr::read_lines(
    caminho, n_max = 1,
    locale = readr::locale(encoding = enc),
    progress = FALSE
  )

  contar <- function(txt) {
    if (length(primeira) == 0 || is.na(primeira[1])) return(0L)
    stringr::str_count(primeira[1], stringr::fixed(txt))
  }

  n_pv <- contar(";")
  n_vg <- contar(",")
  n_tab <- contar("\t")

  delim <- if (n_tab >= max(n_pv, n_vg) && n_tab > 0) {
    "\t"
  } else if (n_pv > n_vg) {
    ";"
  } else {
    ","
  }

  out <- readr::read_delim(
    caminho,
    delim = delim,
    locale = readr::locale(encoding = enc),
    col_types = readr::cols(.default = "c"),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "unique"
  ) %>%
    as.data.frame()

  names(out) <- normalizar_nome(names(out))
  out
}

achar_coluna <- function(df, alternativas, obrigatoria = TRUE) {
  alternativas <- normalizar_nome(alternativas)
  nomes <- names(df)

  exata <- intersect(alternativas, nomes)
  if (length(exata) > 0) return(exata[1])

  for (alt in alternativas) {
    hit <- nomes[grepl(alt, nomes, fixed = TRUE)]
    if (length(hit) == 1) return(hit[1])
  }

  if (obrigatoria) {
    stop(
      "Não foi possível localizar coluna. Alternativas: ",
      paste(alternativas, collapse = ", "),
      "\nColunas disponíveis: ", paste(nomes, collapse = ", ")
    )
  }

  NA_character_
}

ultimo_nao_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  dplyr::last(x)
}

# Estatísticas seguras, com período de cobertura de cada série.
calcular_estatisticas <- function(df, variavel, rotulo, unidade, dimensao, coluna_data = "data") {
  x <- as.numeric(df[[variavel]])
  d <- as.Date(df[[coluna_data]])
  ok <- is.finite(x) & !is.na(d)

  if (!any(ok)) {
    return(tibble(
      Dimensao = dimensao, Variavel = rotulo, Unidade = unidade,
      N = 0L, Inicio = as.Date(NA), Fim = as.Date(NA),
      Media = NA_real_, Mediana = NA_real_, Desvio_Padrao = NA_real_,
      Minimo = NA_real_, P25 = NA_real_, P75 = NA_real_, Maximo = NA_real_
    ))
  }

  xv <- x[ok]
  dv <- d[ok]

  tibble(
    Dimensao = dimensao,
    Variavel = rotulo,
    Unidade = unidade,
    N = length(xv),
    Inicio = min(dv),
    Fim = max(dv),
    Media = mean(xv),
    Mediana = median(xv),
    Desvio_Padrao = if (length(xv) > 1) sd(xv) else NA_real_,
    Minimo = min(xv),
    P25 = as.numeric(quantile(xv, 0.25, names = FALSE)),
    P75 = as.numeric(quantile(xv, 0.75, names = FALSE)),
    Maximo = max(xv)
  )
}

resumo_evolucao <- function(df, variavel, rotulo, unidade, coluna_data = "data") {
  z <- df %>%
    select(data = all_of(coluna_data), valor = all_of(variavel)) %>%
    filter(!is.na(data), is.finite(valor)) %>%
    arrange(data)

  if (nrow(z) == 0) {
    return(tibble(
      Variavel = rotulo, Unidade = unidade,
      Inicio = as.Date(NA), Valor_Inicial = NA_real_,
      Fim = as.Date(NA), Valor_Final = NA_real_,
      Variacao_Absoluta = NA_real_, Variacao_Percentual = NA_real_
    ))
  }

  vi <- z$valor[1]
  vf <- z$valor[nrow(z)]

  tibble(
    Variavel = rotulo,
    Unidade = unidade,
    Inicio = z$data[1],
    Valor_Inicial = vi,
    Fim = z$data[nrow(z)],
    Valor_Final = vf,
    Variacao_Absoluta = vf - vi,
    Variacao_Percentual = ifelse(is.finite(vi) && vi != 0, 100 * (vf / vi - 1), NA_real_)
  )
}

# =============================================================================
# 3. LEITURA DAS BASES
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1 Inadimplência PF até 10 SM – saída do SCR.data
# -----------------------------------------------------------------------------

ler_inad_10sm <- function(caminho) {
  if (!file.exists(caminho)) {
    stop(
      "Não encontrei a base PF até 10 SM em:\n", caminho,
      "\nRode primeiro o script 'Inadimplência PF até 10 sm.R' ou corrija o caminho."
    )
  }

  abas <- readxl::excel_sheets(caminho)
  aba <- if ("Base Mensal" %in% abas) "Base Mensal" else abas[1]

  raw <- readxl::read_excel(caminho, sheet = aba, .name_repair = "unique") %>%
    as.data.frame()
  names(raw) <- normalizar_nome(names(raw))

  col_data <- achar_coluna(raw, c("data", "date", "mes", "periodo"))
  col_val <- achar_coluna(raw, c(
    "inadimplencia_pf_ate10sm_pct",
    "inadimplencia_pf_ate_10sm_pct",
    "inad_pf_10sm",
    "inadimplencia_pf_10sm"
  ))

  raw %>%
    transmute(
      data = parse_data_mensal(.data[[col_data]]),
      inad_pf_10sm = parse_numero(.data[[col_val]])
    ) %>%
    filter(!is.na(data)) %>%
    group_by(data) %>%
    summarise(inad_pf_10sm = ultimo_nao_na(inad_pf_10sm), .groups = "drop") %>%
    arrange(data)
}

# -----------------------------------------------------------------------------
# 3.2 Inadimplência PF – modalidades BCB
# -----------------------------------------------------------------------------

ler_inad_modalidades <- function(caminho) {
  raw <- ler_csv_flex(caminho)

  # O arquivo usado nos códigos anteriores possui Data + 8 séries.
  if (ncol(raw) >= 9) {
    raw9 <- raw[, 1:9]
    names(raw9) <- c(
      "data_raw",
      "inad_pf_total",
      "inad_recursos_livres_total",
      "inad_cheque_especial",
      "inad_credito_pessoal_nao_consignado",
      "inad_composicao_dividas",
      "inad_consignado_privado",
      "inad_nao_consignado_com_garantia",
      "inad_nao_consignado_sem_garantia"
    )

    return(
      raw9 %>%
        mutate(
          data = parse_data_mensal(data_raw),
          across(-c(data_raw, data), parse_numero)
        ) %>%
        select(-data_raw) %>%
        filter(!is.na(data)) %>%
        group_by(data) %>%
        summarise(across(everything(), ultimo_nao_na), .groups = "drop") %>%
        arrange(data)
    )
  }

  stop("Estrutura inesperada no arquivo de inadimplência por modalidades.")
}

# -----------------------------------------------------------------------------
# 3.3 Selic Meta – fim do mês e média mensal
# -----------------------------------------------------------------------------

ler_selic <- function(caminho) {
  raw <- ler_csv_flex(caminho)

  col_data <- achar_coluna(raw, c("data", "date"))
  col_val <- achar_coluna(raw, c(
    "432_taxa_de_juros_meta_selic_definida_pelo_copom_a_a",
    "selic_meta", "meta_selic", "taxa_selic", "selic", "valor"
  ))

  raw %>%
    transmute(
      data_original = parse_data_diaria(.data[[col_data]]),
      selic = parse_numero(.data[[col_val]])
    ) %>%
    filter(!is.na(data_original), is.finite(selic)) %>%
    mutate(data = floor_date(data_original, "month")) %>%
    group_by(data) %>%
    arrange(data_original, .by_group = TRUE) %>%
    summarise(
      selic_fim_mes = dplyr::last(selic),
      selic_media_mensal = mean(selic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(data)
}

# -----------------------------------------------------------------------------
# 3.4 IPCA mensal – IBGE
# -----------------------------------------------------------------------------

ler_ipca_ibge <- function(caminho) {
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)

  raw <- readxl::read_excel(
    caminho, sheet = 1, col_names = FALSE,
    .name_repair = "minimal"
  ) %>% as.data.frame(check.names = FALSE)

  if (ncol(raw) < 4) stop("Arquivo histórico do IPCA possui menos de 4 colunas.")

  ano_raw <- parse_numero(raw[[1]])
  ano <- ifelse(
    is.finite(ano_raw) & ano_raw >= 1900 & ano_raw <= 2100,
    as.integer(ano_raw), NA_integer_
  )
  ano <- zoo::na.locf(ano, na.rm = FALSE)

  mes <- mes_pt_numero(raw[[2]])
  ipca <- parse_numero(raw[[4]])

  valido <- !is.na(ano) & !is.na(mes) & !is.na(ipca)

  tibble(
    data = as.Date(sprintf("%04d-%02d-01", ano[valido], mes[valido])),
    ipca = as.numeric(ipca[valido])
  ) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
}

# -----------------------------------------------------------------------------
# 3.5 Bets – GGR mensal
# -----------------------------------------------------------------------------

ler_bets <- function(caminho) {
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)

  abas <- readxl::excel_sheets(caminho)
  aba <- if ("Base_mensal" %in% abas) "Base_mensal" else abas[1]

  raw <- readxl::read_excel(
    caminho, sheet = aba, skip = 2,
    .name_repair = "unique"
  ) %>% as.data.frame()
  names(raw) <- normalizar_nome(names(raw))

  col_data <- achar_coluna(raw, c("data", "date", "mes_ano", "competencia", "periodo"))
  col_bets <- achar_coluna(raw, c("ggr_bets_r_bi", "ggr_bets_rbi", "ggr_bets", "ggr", "bets"))

  raw %>%
    transmute(
      data = parse_data_mensal(.data[[col_data]]),
      bets_ggr_nominal_r_bi = parse_numero(.data[[col_bets]])
    ) %>%
    filter(!is.na(data), is.finite(bets_ggr_nominal_r_bi)) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
}

# -----------------------------------------------------------------------------
# 3.6 PNAD – desemprego (CSV horizontal)
# -----------------------------------------------------------------------------

ler_pnad_horizontal_csv <- function(caminho, nome_saida) {
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)

  enc <- detectar_encoding_csv(caminho)
  linhas <- readr::read_lines(
    caminho,
    locale = readr::locale(encoding = enc),
    progress = FALSE
  )

  idx_brasil <- which(grepl("^\\s*Brasil\\s*;", linhas, ignore.case = TRUE))[1]
  if (is.na(idx_brasil)) stop("Não encontrei a linha 'Brasil' em: ", caminho)

  # Procura, antes da linha Brasil, a linha com maior quantidade de períodos PNAD.
  candidatos <- seq_len(max(1, idx_brasil - 1))
  score <- vapply(candidatos, function(i) {
    sum(grepl("(19|20)\\d{2}", strsplit(linhas[i], ";", fixed = TRUE)[[1]]))
  }, integer(1))
  idx_periodos <- candidatos[which.max(score)]

  periodo_raw <- strsplit(linhas[idx_periodos], ";", fixed = TRUE)[[1]]
  valor_raw <- strsplit(linhas[idx_brasil], ";", fixed = TRUE)[[1]]

  n <- min(length(periodo_raw), length(valor_raw))
  periodo_raw <- trimws(periodo_raw[2:n])
  valor_raw <- trimws(valor_raw[2:n])

  out <- tibble(
    data = parse_periodo_pnad(periodo_raw),
    valor = parse_numero(valor_raw)
  ) %>%
    filter(!is.na(data), is.finite(valor)) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)

  names(out)[2] <- nome_saida
  out
}

# -----------------------------------------------------------------------------
# 3.7 PNAD – renda média (Excel SIDRA / PNAD Contínua)
# -----------------------------------------------------------------------------
# O arquivo informado pelo usuário é a Tabela 7437 do SIDRA em formato largo.
# Ele NÃO é mensal: traz valores ANUAIS (2015–2025) em blocos de colunas.
#
# Para comparação intertemporal, usamos preferencialmente a aba "Tabela 3":
#   "Rendimento médio mensal real ... a preços médios do último ano".
# Assim, os anos ficam expressos em uma mesma referência de preços.
#
# Extraímos:
#   - geografia = Brasil;
#   - tipo de rendimento = "Todas as fontes";
#   - frequência = anual;
#   - data de referência = dezembro de cada ano.
#
# IMPORTANTE: a função NÃO interpola nem replica os valores anuais para os 12
# meses. Isso evita criar falsa frequência mensal. Portanto, na base mensal a
# renda aparecerá apenas em dezembro de cada ano; as estatísticas da renda terão
# N igual ao número de anos efetivamente disponíveis.

ler_renda_pnad_excel <- function(caminho) {
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)

  abas <- readxl::excel_sheets(caminho)

  # Preferência substantiva: valores reais a preços médios do último ano.
  aba_preferida <- if ("Tabela 3" %in% abas) {
    "Tabela 3"
  } else if ("Tabela 1" %in% abas) {
    "Tabela 1"
  } else {
    abas[1]
  }

  raw <- readxl::read_excel(
    caminho,
    sheet = aba_preferida,
    col_names = FALSE,
    .name_repair = "minimal"
  ) %>%
    as.data.frame(check.names = FALSE)

  if (nrow(raw) < 6 || ncol(raw) < 4) {
    stop(
      "A aba '", aba_preferida,
      "' possui estrutura inesperada no arquivo de renda PNAD."
    )
  }

  # Matriz de texto apenas para localizar cabeçalhos/linhas.
  mat_txt <- as.matrix(raw)
  mat_txt <- apply(mat_txt, 2, as.character)
  mat_norm <- matrix(
    normalizar_nome(mat_txt),
    nrow = nrow(raw),
    ncol = ncol(raw)
  )

  # ---------------------------------------------------------------------------
  # 1) Localizar a linha "Brasil"
  # ---------------------------------------------------------------------------
  pos_brasil <- which(mat_norm == "brasil", arr.ind = TRUE)

  if (nrow(pos_brasil) == 0) {
    stop(
      "Não encontrei a linha 'Brasil' na aba '", aba_preferida,
      "' do arquivo de renda PNAD."
    )
  }

  # No SIDRA, a linha Brasil normalmente tem 'BR' na primeira coluna.
  linhas_brasil <- unique(pos_brasil[, "row"])
  linha_brasil <- linhas_brasil[1]

  if (ncol(raw) >= 1) {
    primeira_col_norm <- normalizar_nome(raw[[1]])
    candidatos_br <- which(primeira_col_norm == "br")
    if (length(candidatos_br) > 0) {
      linha_brasil <- candidatos_br[1]
    }
  }

  # ---------------------------------------------------------------------------
  # 2) Localizar a linha dos anos (2015, 2016, ..., 2025)
  # ---------------------------------------------------------------------------
  linhas_acima <- seq_len(max(1, linha_brasil - 1))

  score_anos <- vapply(
    linhas_acima,
    function(r) {
      vals <- suppressWarnings(as.integer(as.character(unlist(raw[r, ], use.names = FALSE))))
      sum(!is.na(vals) & vals >= 2000 & vals <= 2100)
    },
    integer(1)
  )

  linha_anos <- linhas_acima[which.max(score_anos)]

  if (max(score_anos) == 0) {
    stop(
      "Não encontrei os anos na aba '", aba_preferida,
      "' do arquivo de renda PNAD."
    )
  }

  anos_linha <- suppressWarnings(
    as.integer(as.character(unlist(raw[linha_anos, ], use.names = FALSE)))
  )

  # ---------------------------------------------------------------------------
  # 3) Localizar a linha dos tipos de rendimento e as colunas "Todas as fontes"
  # ---------------------------------------------------------------------------
  linhas_entre <- seq.int(linha_anos + 1, max(linha_anos + 1, linha_brasil - 1))
  linhas_entre <- linhas_entre[linhas_entre < linha_brasil]

  if (length(linhas_entre) == 0) {
    stop("Não encontrei a linha de tipos de rendimento no arquivo PNAD.")
  }

  score_todas_fontes <- vapply(
    linhas_entre,
    function(r) {
      vals <- normalizar_nome(unlist(raw[r, ], use.names = FALSE))
      sum(vals == "todas_as_fontes", na.rm = TRUE)
    },
    integer(1)
  )

  linha_tipos <- linhas_entre[which.max(score_todas_fontes)]

  if (max(score_todas_fontes) == 0) {
    stop(
      "Não encontrei a categoria 'Todas as fontes' na aba '",
      aba_preferida, "'."
    )
  }

  tipos <- normalizar_nome(unlist(raw[linha_tipos, ], use.names = FALSE))
  colunas_todas <- which(tipos == "todas_as_fontes")

  # ---------------------------------------------------------------------------
  # 4) Para cada bloco anual, associar a coluna 'Todas as fontes' ao ano mais
  #    próximo à esquerda na linha de anos.
  # ---------------------------------------------------------------------------
  extrair_ano_da_coluna <- function(coluna) {
    candidatas <- which(
      seq_along(anos_linha) <= coluna &
        !is.na(anos_linha) &
        anos_linha >= 2000 &
        anos_linha <= 2100
    )

    if (length(candidatas) == 0) return(NA_integer_)
    anos_linha[max(candidatas)]
  }

  anos <- vapply(colunas_todas, extrair_ano_da_coluna, integer(1))

  valores <- vapply(
    colunas_todas,
    function(j) parse_numero(raw[[j]][linha_brasil]),
    numeric(1)
  )

  out <- tibble(
    ano = anos,
    data = as.Date(sprintf("%04d-12-01", anos)),
    renda = valores
  ) %>%
    filter(
      !is.na(ano),
      ano >= 1900,
      ano <= 2100,
      !is.na(data),
      is.finite(renda)
    ) %>%
    distinct(ano, .keep_all = TRUE) %>%
    arrange(data) %>%
    select(data, renda)

  if (nrow(out) == 0) {
    stop(
      "A estrutura da PNAD foi reconhecida, mas nenhuma observação válida de ",
      "renda para Brasil / Todas as fontes foi extraída."
    )
  }

  message(
    "Renda PNAD importada com sucesso da aba '", aba_preferida, "': ",
    format(min(out$data), "%Y"), "–", format(max(out$data), "%Y"),
    " (", nrow(out), " observações anuais; referência em dezembro)."
  )

  out
}

# -----------------------------------------------------------------------------
# 3.8 Série BCB genérica – endividamento / comprometimento
# -----------------------------------------------------------------------------

ler_serie_bcb_generica <- function(caminho, nome_saida) {
  raw <- ler_csv_flex(caminho)
  if (ncol(raw) < 2) stop("Arquivo BCB com menos de duas colunas: ", caminho)

  col_data <- achar_coluna(raw, c("data", "date", "mes", "periodo"), obrigatoria = FALSE)
  if (is.na(col_data)) col_data <- names(raw)[1]

  candidatos <- setdiff(names(raw), col_data)
  col_val <- candidatos[1]

  out <- raw %>%
    transmute(
      data = parse_data_mensal(.data[[col_data]]),
      valor = parse_numero(.data[[col_val]])
    ) %>%
    filter(!is.na(data), is.finite(valor)) %>%
    group_by(data) %>%
    summarise(valor = ultimo_nao_na(valor), .groups = "drop") %>%
    arrange(data)

  names(out)[2] <- nome_saida
  out
}

# -----------------------------------------------------------------------------
# 3.9 Serasa
# -----------------------------------------------------------------------------

ler_serasa <- function(caminho) {
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)

  abas <- readxl::excel_sheets(caminho)
  aba <- if ("Consumidores Inadimplentes" %in% abas) "Consumidores Inadimplentes" else abas[1]

  raw <- readxl::read_excel(
    caminho,
    sheet = aba,
    skip = 3,
    col_names = FALSE,
    na = c("", "NA", "n.d.", "n.d")
  )

  if (ncol(raw) < 8) stop("Estrutura inesperada na base Serasa.")

  raw <- raw[, seq_len(min(14, ncol(raw)))]

  nomes <- c(
    "data", "serasa_inadimplentes_milhoes", "serasa_dividas_negativadas_milhoes",
    "serasa_dividas_negativadas_r_bilhoes", "serasa_dividas_media_por_cpf",
    "serasa_divida_media_r", "serasa_ticket_medio_r", "serasa_populacao_adulta_pct",
    "serasa_genero_f_milhoes", "serasa_genero_m_milhoes",
    "serasa_ate_25_milhoes", "serasa_26_40_milhoes",
    "serasa_41_60_milhoes", "serasa_acima_60_milhoes"
  )
  names(raw) <- nomes[seq_len(ncol(raw))]

  raw %>%
    mutate(
      data = parse_data_mensal(data),
      across(-data, parse_numero),
      # No Excel, essa coluna costuma vir como proporção (0,397 = 39,7%).
      serasa_populacao_adulta_pct = ifelse(
        is.finite(serasa_populacao_adulta_pct) & serasa_populacao_adulta_pct <= 1.5,
        100 * serasa_populacao_adulta_pct,
        serasa_populacao_adulta_pct
      )
    ) %>%
    filter(!is.na(data)) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
}

# -----------------------------------------------------------------------------
# 3.10 Taxas de juros por modalidade
# -----------------------------------------------------------------------------

escolher_coluna_por_tokens <- function(nomes, tokens, excluir = character(0), codigo = NULL) {
  nomes_norm <- normalizar_nome(nomes)

  if (!is.null(codigo)) {
    hit_codigo <- nomes[grepl(paste0("^", codigo, "_"), nomes_norm)]
    if (length(hit_codigo) >= 1) return(hit_codigo[1])
  }

  ok <- rep(TRUE, length(nomes_norm))
  for (tok in normalizar_nome(tokens)) {
    ok <- ok & grepl(tok, nomes_norm, fixed = TRUE)
  }
  for (tok in normalizar_nome(excluir)) {
    ok <- ok & !grepl(tok, nomes_norm, fixed = TRUE)
  }

  hits <- nomes[ok]
  if (length(hits) == 0) return(NA_character_)

  # Prefere nomes que explicitam PF / recursos livres quando houver vários.
  score <- as.integer(grepl("pessoas_fisicas|pf", normalizar_nome(hits))) +
    as.integer(grepl("recursos_livres|livre", normalizar_nome(hits)))
  hits[order(-score, nchar(hits))][1]
}

ler_juros_modalidades <- function(caminho) {
  raw <- ler_csv_flex(caminho)
  col_data <- achar_coluna(raw, c("data", "date", "mes", "periodo"), obrigatoria = FALSE)
  if (is.na(col_data)) col_data <- names(raw)[1]

  nomes <- names(raw)

  mapa <- c(
    juros_pessoal_nao_consignado = escolher_coluna_por_tokens(
      nomes, c("credito", "pessoal", "nao", "consignado"), codigo = "20742"
    ),
    juros_cartao_total = escolher_coluna_por_tokens(
      nomes, c("cartao", "credito", "total"), excluir = c("rotativo")
    ),
    juros_cartao_rotativo = escolher_coluna_por_tokens(
      nomes, c("cartao", "rotativo")
    ),
    juros_cheque_especial = escolher_coluna_por_tokens(
      nomes, c("cheque", "especial")
    ),
    juros_consignado = escolher_coluna_por_tokens(
      nomes, c("consignado"), excluir = c("nao_consignado")
    ),
    juros_veiculos = escolher_coluna_por_tokens(
      nomes, c("veiculos")
    )
  )

  faltantes <- names(mapa)[is.na(mapa)]
  if (length(faltantes) > 0) {
    warning(
      "Não identifiquei automaticamente estas taxas: ",
      paste(faltantes, collapse = ", "),
      "\nO código continuará; confira a aba 'Diagnostico_Importacao'."
    )
  }

  out <- tibble(data = parse_data_mensal(raw[[col_data]]))

  for (nm in names(mapa)) {
    col <- mapa[[nm]]
    out[[paste0(nm, "_am")]] <- if (!is.na(col)) parse_numero(raw[[col]]) else NA_real_
  }

  out <- out %>%
    filter(!is.na(data)) %>%
    group_by(data) %>%
    summarise(across(everything(), ultimo_nao_na), .groups = "drop") %>%
    arrange(data)

  if (JUROS_ARQUIVO_EH_MENSAL) {
    cols_am <- names(out)[endsWith(names(out), "_am")]
    for (col in cols_am) {
      nome_aa <- sub("_am$", "_aa", col)
      out[[nome_aa]] <- ((1 + out[[col]] / 100)^12 - 1) * 100
    }
  }

  attr(out, "mapa_colunas") <- tibble(
    Serie = names(mapa),
    Coluna_Encontrada = unname(mapa)
  )

  out
}

# -----------------------------------------------------------------------------
# 3.11 Itaú – NPL 90 dias, unindo arquivos históricos
# -----------------------------------------------------------------------------

parse_data_itau <- function(x) {
  s <- stringr::str_squish(as.character(x))
  out <- rep(as.Date(NA), length(s))

  num <- suppressWarnings(as.numeric(s))
  idx_num <- !is.na(num) & num > 30000
  out[idx_num] <- as.Date(num[idx_num], origin = "1899-12-30")

  s2 <- iconv(tolower(s), from = "", to = "ASCII//TRANSLIT")
  meses <- c(
    jan = "01", fev = "02", mar = "03", abr = "04", mai = "05", jun = "06",
    jul = "07", ago = "08", set = "09", out = "10", nov = "11", dez = "12"
  )
  for (m in names(meses)) {
    s2 <- stringr::str_replace_all(s2, paste0("/", m, "/"), paste0("/", meses[[m]], "/"))
  }

  idx_txt <- is.na(out) & !is.na(s2) & s2 != ""
  if (any(idx_txt)) {
    d1 <- suppressWarnings(lubridate::ymd(s2[idx_txt]))
    d2 <- suppressWarnings(lubridate::dmy(s2[idx_txt]))
    out[idx_txt] <- dplyr::coalesce(d1, d2)
  }

  as.Date(out)
}

parse_num_itau <- function(x) {
  suppressWarnings(readr::parse_number(
    as.character(x),
    locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
    na = c("", "NA", "-", "n.d.", "n.d")
  ))
}

extrair_npl_itau_arquivo <- function(caminho, prioridade, origem) {
  if (!file.exists(caminho)) {
    warning("Arquivo Itaú não encontrado e será ignorado: ", caminho)
    return(NULL)
  }

  abas <- readxl::excel_sheets(caminho)
  aba <- dplyr::case_when(
    "NPL_com_TVM" %in% abas ~ "NPL_com_TVM",
    "NPL_Nova segmentação" %in% abas ~ "NPL_Nova segmentação",
    TRUE ~ NA_character_
  )

  if (is.na(aba)) {
    warning("Nenhuma aba de NPL reconhecida em: ", caminho)
    return(NULL)
  }

  dados <- readxl::read_excel(
    caminho, sheet = aba, col_names = FALSE, col_types = "text",
    na = c("", "NA", "-", "n.d.", "n.d"),
    .name_repair = "unique_quiet"
  )

  config <- tibble::tribble(
    ~rotulo, ~linha_fallback, ~serie,
    "NPL 90 dias - Total", 14L, "itau_npl90_total_pct",
    "NPL 90 dias - Brasil", 15L, "itau_npl90_brasil_pct",
    "NPL 90 dias - Pessoas Físicas - Brasil", 18L, "itau_npl90_pf_brasil_pct",
    "NPL 90 dias - Micro, Pequenas e Médias Empresas - Brasil", 19L, "itau_npl90_mpme_brasil_pct",
    "NPL 90 dias - Grandes Empresas - Brasil", 20L, "itau_npl90_grandes_brasil_pct"
  )

  datas <- parse_data_itau(unlist(dados[2, -1], use.names = FALSE))

  lista <- purrr::pmap(config, function(rotulo, linha_fallback, serie) {
    labels <- stringr::str_squish(as.character(dados[[1]]))
    idx <- which(labels == stringr::str_squish(rotulo))
    linha <- if (length(idx) > 0) idx[1] else linha_fallback

    if (linha > nrow(dados)) {
      return(tibble(data_trimestre = as.Date(character()), valor = numeric()))
    }

    valores <- parse_num_itau(unlist(dados[linha, -1], use.names = FALSE)) * 100

    out <- tibble(
      data_trimestre = floor_date(datas, "quarter"),
      valor = valores
    ) %>%
      filter(!is.na(data_trimestre)) %>%
      distinct(data_trimestre, .keep_all = TRUE)

    names(out)[2] <- serie
    out
  })

  wide <- purrr::reduce(lista, full_join, by = "data_trimestre") %>%
    arrange(data_trimestre)

  wide$prioridade <- prioridade
  wide$origem <- origem
  wide
}

consolidar_npl_itau <- function() {
  bases <- list(
    extrair_npl_itau_arquivo(arquivo_itau_descontinuado, 1, "Descontinuada"),
    extrair_npl_itau_arquivo(arquivo_itau_historico, 2, "Historico"),
    extrair_npl_itau_arquivo(arquivo_itau_atual, 3, "Atual")
  )
  bases <- bases[!vapply(bases, is.null, logical(1))]
  if (length(bases) == 0) stop("Nenhuma base do Itaú pôde ser lida.")

  bind_rows(bases) %>%
    pivot_longer(
      cols = starts_with("itau_npl90_"),
      names_to = "serie",
      values_to = "valor"
    ) %>%
    filter(!is.na(valor)) %>%
    arrange(data_trimestre, serie, desc(prioridade)) %>%
    group_by(data_trimestre, serie) %>%
    slice(1) %>%
    ungroup() %>%
    select(data_trimestre, serie, valor) %>%
    pivot_wider(names_from = serie, values_from = valor) %>%
    arrange(data_trimestre)
}

# =============================================================================
# 4. IMPORTAÇÃO
# =============================================================================

message("\n============================================================")
message("IMPORTANDO AS BASES")
message("============================================================")

inad_10sm <- ler_inad_10sm(arquivo_inad_10sm)
inad_modalidades <- ler_inad_modalidades(arquivo_inad_modalidades)
selic <- ler_selic(arquivo_selic)
ipca <- ler_ipca_ibge(arquivo_ipca)
bets <- ler_bets(arquivo_bets)
desemprego <- ler_pnad_horizontal_csv(arquivo_desemprego, "desemprego")
renda <- ler_renda_pnad_excel(arquivo_renda)
endividamento <- ler_serie_bcb_generica(arquivo_endividamento, "endividamento")
comprometimento <- ler_serie_bcb_generica(arquivo_comprometimento, "comprometimento_juros")
serasa <- ler_serasa(arquivo_serasa)
juros_modalidades <- ler_juros_modalidades(arquivo_juros_modalidades)
itau_npl <- consolidar_npl_itau()

mapa_juros_importacao <- attr(juros_modalidades, "mapa_colunas")

# =============================================================================
# 5. BASE MENSAL PRINCIPAL – 2021M01 A 2025M12
# =============================================================================

calendario <- tibble(data = seq.Date(DATA_INICIO, DATA_FIM, by = "month"))

base_mensal <- calendario %>%
  left_join(inad_10sm, by = "data") %>%
  left_join(inad_modalidades, by = "data") %>%
  left_join(selic, by = "data") %>%
  left_join(ipca, by = "data") %>%
  left_join(bets, by = "data") %>%
  left_join(desemprego, by = "data") %>%
  left_join(renda, by = "data") %>%
  left_join(endividamento, by = "data") %>%
  left_join(comprometimento, by = "data") %>%
  left_join(
    serasa %>% select(
      data,
      serasa_inadimplentes_milhoes,
      serasa_populacao_adulta_pct,
      serasa_dividas_negativadas_r_bilhoes,
      serasa_divida_media_r
    ),
    by = "data"
  ) %>%
  left_join(juros_modalidades, by = "data") %>%
  arrange(data)

# -----------------------------------------------------------------------------
# 5.1 Deflator e Bets reais
# -----------------------------------------------------------------------------

base_mensal <- base_mensal %>%
  mutate(indice_precos = NA_real_)

idx_ipca <- which(!is.na(base_mensal$ipca))
if (length(idx_ipca) > 0) {
  fator <- 1 + base_mensal$ipca[idx_ipca] / 100
  base_mensal$indice_precos[idx_ipca] <- 100 * cumprod(fator) / cumprod(fator)[1]
}

ref_idx <- which(base_mensal$data == as.Date("2025-12-01") & !is.na(base_mensal$indice_precos))
if (length(ref_idx) == 0) ref_idx <- tail(which(!is.na(base_mensal$indice_precos)), 1)
indice_ref <- if (length(ref_idx) > 0) base_mensal$indice_precos[ref_idx[1]] else NA_real_

base_mensal <- base_mensal %>%
  mutate(
    renda_real = if (RENDA_JA_REAL) renda else renda * indice_ref / indice_precos,
    bets_ggr_real_r_bi = ifelse(
      !is.na(bets_ggr_nominal_r_bi) & !is.na(indice_precos),
      bets_ggr_nominal_r_bi * indice_ref / indice_precos,
      NA_real_
    ),
    crescimento_bets_real_mensal_pct = 100 * (log(bets_ggr_real_r_bi) - lag(log(bets_ggr_real_r_bi))),
    gap_inad_10sm_total_pp = inad_pf_10sm - inad_pf_total
  )

# Índice de intensidade Bets/Renda: útil como robustez descritiva; NÃO é a
# fração literal da renda agregada apostada.
primeiro_ratio <- base_mensal %>%
  filter(!is.na(bets_ggr_real_r_bi), !is.na(renda_real), renda_real > 0) %>%
  transmute(ratio = bets_ggr_real_r_bi / renda_real) %>%
  slice(1) %>%
  pull(ratio)

if (length(primeiro_ratio) == 1 && is.finite(primeiro_ratio) && primeiro_ratio != 0) {
  base_mensal <- base_mensal %>%
    mutate(
      bets_intensidade_idx = ifelse(
        !is.na(bets_ggr_real_r_bi) & !is.na(renda_real) & renda_real > 0,
        100 * (bets_ggr_real_r_bi / renda_real) / primeiro_ratio,
        NA_real_
      )
    )
} else {
  base_mensal$bets_intensidade_idx <- NA_real_
}

# =============================================================================
# 6. BASE SETORIAL TRIMESTRAL – ITAÚ x SFN
# =============================================================================

sfn_trimestral <- base_mensal %>%
  mutate(data_trimestre = floor_date(data, "quarter")) %>%
  group_by(data_trimestre) %>%
  summarise(
    sfn_inad_pf_10sm_pct = ultimo_nao_na(inad_pf_10sm),
    sfn_inad_pf_total_pct = ultimo_nao_na(inad_pf_total),
    .groups = "drop"
  )

base_setorial <- itau_npl %>%
  filter(
    data_trimestre >= floor_date(DATA_INICIO, "quarter"),
    data_trimestre <= floor_date(DATA_FIM, "quarter")
  ) %>%
  full_join(sfn_trimestral, by = "data_trimestre") %>%
  arrange(data_trimestre) %>%
  mutate(
    # Apenas medidas descritivas. As definições de NPL Itaú e SFN não são
    # perfeitamente equivalentes; não interpretar o nível do gap como causal.
    gap_itau_pf_menos_sfn_pf_pp = itau_npl90_pf_brasil_pct - sfn_inad_pf_total_pct,
    gap_itau_pf_menos_sfn_10sm_pp = itau_npl90_pf_brasil_pct - sfn_inad_pf_10sm_pct
  )

# =============================================================================
# 7. TABELAS DE ESTATÍSTICAS DESCRITIVAS POR DIMENSÃO
# =============================================================================

# -----------------------------------------------------------------------------
# 7.1 Financeira – capacidade de pagamento / inadimplência
# -----------------------------------------------------------------------------

config_financeira <- tribble(
  ~var, ~rotulo, ~unidade,
  "inad_pf_10sm", "Inadimplência PF até 10 SM – SFN", "%",
  "inad_pf_total", "Inadimplência PF total – SFN", "%",
  "gap_inad_10sm_total_pp", "Gap: PF até 10 SM menos PF total", "p.p.",
  "endividamento", "Endividamento das famílias", "%",
  "comprometimento_juros", "Comprometimento da renda com juros", "%",
  "serasa_populacao_adulta_pct", "População adulta inadimplente – Serasa", "%",
  "serasa_inadimplentes_milhoes", "Consumidores inadimplentes – Serasa", "milhões"
)

tab_financeira <- pmap_dfr(
  config_financeira,
  ~ calcular_estatisticas(base_mensal, ..1, ..2, ..3, "Financeira")
)

# -----------------------------------------------------------------------------
# 7.2 Financeira – custo do crédito por modalidade
# -----------------------------------------------------------------------------

config_juros <- tribble(
  ~var, ~rotulo, ~unidade,
  "juros_pessoal_nao_consignado_aa", "Crédito pessoal não consignado", "% a.a. efetiva",
  "juros_cartao_total_aa", "Cartão de crédito total", "% a.a. efetiva",
  "juros_cartao_rotativo_aa", "Cartão de crédito rotativo", "% a.a. efetiva",
  "juros_cheque_especial_aa", "Cheque especial", "% a.a. efetiva",
  "juros_consignado_aa", "Crédito consignado", "% a.a. efetiva",
  "juros_veiculos_aa", "Aquisição de veículos", "% a.a. efetiva"
)

# Se o arquivo um dia passar a conter taxas anuais, use as colunas sem conversão.
if (!JUROS_ARQUIVO_EH_MENSAL) {
  config_juros$var <- sub("_aa$", "_am", config_juros$var)
  config_juros$unidade <- "% a.a. (arquivo original)"
}

tab_juros <- pmap_dfr(
  config_juros,
  ~ calcular_estatisticas(base_mensal, ..1, ..2, ..3, "Financeira – custo do crédito")
)

# -----------------------------------------------------------------------------
# 7.3 Comportamental – Bets
# -----------------------------------------------------------------------------

config_comportamental <- tribble(
  ~var, ~rotulo, ~unidade,
  "bets_ggr_nominal_r_bi", "GGR mensal das bets – nominal", "R$ bilhões",
  "bets_ggr_real_r_bi", "GGR mensal das bets – real (preços de dez/2025)", "R$ bilhões",
  "crescimento_bets_real_mensal_pct", "Crescimento mensal do GGR real", "%",
  "bets_intensidade_idx", "Índice de intensidade Bets/Renda (base inicial = 100)", "índice"
)

tab_comportamental <- pmap_dfr(
  config_comportamental,
  ~ calcular_estatisticas(base_mensal, ..1, ..2, ..3, "Comportamental")
)

# -----------------------------------------------------------------------------
# 7.4 Macroeconômica
# -----------------------------------------------------------------------------

config_macro <- tribble(
  ~var, ~rotulo, ~unidade,
  "selic_fim_mes", "Meta Selic – fim do mês", "% a.a.",
  "ipca", "IPCA – variação mensal", "% a.m.",
  "desemprego", "Taxa de desocupação – PNAD Contínua", "%",
  "renda_real", "Rendimento médio real – PNAD Contínua", "R$"
)

tab_macro <- pmap_dfr(
  config_macro,
  ~ calcular_estatisticas(base_mensal, ..1, ..2, ..3, "Macroeconômica")
)

# -----------------------------------------------------------------------------
# 7.5 Setorial – Itaú x SFN (trimestral)
# -----------------------------------------------------------------------------

config_setorial <- tribble(
  ~var, ~rotulo, ~unidade,
  "itau_npl90_total_pct", "NPL 90 dias – Itaú Total", "%",
  "itau_npl90_brasil_pct", "NPL 90 dias – Itaú Brasil", "%",
  "itau_npl90_pf_brasil_pct", "NPL 90 dias – Itaú PF Brasil", "%",
  "sfn_inad_pf_total_pct", "Inadimplência PF total – SFN", "%",
  "sfn_inad_pf_10sm_pct", "Inadimplência PF até 10 SM – SFN", "%",
  "gap_itau_pf_menos_sfn_pf_pp", "Gap Itaú PF menos SFN PF total", "p.p.",
  "gap_itau_pf_menos_sfn_10sm_pp", "Gap Itaú PF menos SFN PF até 10 SM", "p.p."
)

tab_setorial <- pmap_dfr(
  config_setorial,
  ~ calcular_estatisticas(
    base_setorial, ..1, ..2, ..3, "Setorial",
    coluna_data = "data_trimestre"
  )
)

# Junta todas as tabelas em uma visão única.
tab_descritivas_completa <- bind_rows(
  tab_financeira,
  tab_juros,
  tab_comportamental,
  tab_setorial,
  tab_macro
)

# =============================================================================
# 8. EVOLUÇÃO NO PERÍODO – FACILITA OS COMENTÁRIOS DO BLOCO 4
# =============================================================================

config_evolucao <- bind_rows(
  config_financeira %>% mutate(Grupo = "Financeira"),
  config_comportamental %>% mutate(Grupo = "Comportamental"),
  config_macro %>% mutate(Grupo = "Macroeconômica")
)

tab_evolucao <- pmap_dfr(
  config_evolucao,
  function(var, rotulo, unidade, Grupo) {
    resumo_evolucao(base_mensal, var, rotulo, unidade) %>% mutate(Dimensao = Grupo, .before = 1)
  }
)

# Evolução setorial trimestral.
tab_evolucao_setorial <- pmap_dfr(
  config_setorial,
  function(var, rotulo, unidade) {
    resumo_evolucao(
      base_setorial %>% rename(data = data_trimestre),
      var, rotulo, unidade
    ) %>% mutate(Dimensao = "Setorial", .before = 1)
  }
)

tab_evolucao <- bind_rows(tab_evolucao, tab_evolucao_setorial)

# =============================================================================
# 9. MÉDIAS ANUAIS – FATO ESTILIZADO TEMPORAL
# =============================================================================

vars_anuais <- c(
  "inad_pf_10sm", "inad_pf_total", "endividamento", "comprometimento_juros",
  "serasa_populacao_adulta_pct", "bets_ggr_real_r_bi", "selic_fim_mes",
  "ipca", "desemprego", "renda_real"
)
vars_anuais <- intersect(vars_anuais, names(base_mensal))

medias_anuais <- base_mensal %>%
  mutate(ano = year(data)) %>%
  group_by(ano) %>%
  summarise(
    across(
      all_of(vars_anuais),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# =============================================================================
# 10. CORRELAÇÕES DESCRITIVAS – 2021-2025
# =============================================================================
# ATENÇÃO: correlação em nível não é evidência causal e pode refletir tendência
# comum / não estacionariedade. O uso aqui é apenas descritivo.

vars_cor <- c(
  "inad_pf_10sm",
  "inad_pf_total",
  "bets_ggr_real_r_bi",
  "selic_fim_mes",
  "ipca",
  "desemprego",
  "renda_real",
  "endividamento",
  "comprometimento_juros",
  "juros_pessoal_nao_consignado_aa",
  "juros_cartao_rotativo_aa",
  "juros_cheque_especial_aa"
)
vars_cor <- intersect(vars_cor, names(base_mensal))

mat_cor <- cor(
  base_mensal %>% select(all_of(vars_cor)),
  use = "pairwise.complete.obs",
  method = "pearson"
)

mat_n <- matrix(
  NA_integer_, nrow = length(vars_cor), ncol = length(vars_cor),
  dimnames = list(vars_cor, vars_cor)
)
for (i in seq_along(vars_cor)) {
  for (j in seq_along(vars_cor)) {
    mat_n[i, j] <- sum(complete.cases(base_mensal[[vars_cor[i]]], base_mensal[[vars_cor[j]]]))
  }
}

cor_df <- as.data.frame(mat_cor) %>% rownames_to_column("Variavel")
cor_n_df <- as.data.frame(mat_n) %>% rownames_to_column("Variavel")

# =============================================================================
# 11. QUALIDADE / COBERTURA DOS DADOS
# =============================================================================

qualidade_dados <- tibble(
  Serie = names(base_mensal)[-1],
  N_Observacoes = map_int(base_mensal[-1], ~ sum(!is.na(.x))),
  N_Faltantes = map_int(base_mensal[-1], ~ sum(is.na(.x))),
  Percentual_Faltante = map_dbl(base_mensal[-1], ~ 100 * mean(is.na(.x))),
  Inicio = map_dbl(base_mensal[-1], function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) return(NA_real_)
    as.numeric(base_mensal$data[min(idx)])
  }),
  Fim = map_dbl(base_mensal[-1], function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) return(NA_real_)
    as.numeric(base_mensal$data[max(idx)])
  })
) %>%
  mutate(
    Inicio = as.Date(Inicio, origin = "1970-01-01"),
    Fim = as.Date(Fim, origin = "1970-01-01")
  ) %>%
  arrange(desc(Percentual_Faltante))

# Dicionário enxuto para a entrega.
dicionario <- tribble(
  ~Variavel, ~Dimensao, ~Fonte, ~Observacao,
  "inad_pf_10sm", "Financeira", "BCB / SCR.data", "PF com renda até 10 salários mínimos",
  "inad_pf_total", "Financeira", "BCB", "Inadimplência PF total",
  "endividamento", "Financeira", "BCB", "Indicador de endividamento das famílias",
  "comprometimento_juros", "Financeira", "BCB", "Comprometimento de renda com juros",
  "serasa_populacao_adulta_pct", "Financeira", "Serasa", "% da população adulta inadimplente",
  "bets_ggr_nominal_r_bi", "Comportamental", "Base de Bets construída pelo grupo", "2021-2023 contém valores estimados/mensalizados; 2024-2025 têm maior conteúdo observado",
  "bets_ggr_real_r_bi", "Comportamental", "Base de Bets + IPCA", "GGR deflacionado para preços de dez/2025",
  "bets_intensidade_idx", "Comportamental", "Base de Bets + PNAD", "Índice de robustez; não interpretar como percentual literal da renda apostado",
  "itau_npl90_pf_brasil_pct", "Setorial", "RI Itaú", "NPL 90 dias PF Brasil; frequência trimestral",
  "selic_fim_mes", "Macroeconômica", "BCB", "Meta Selic vigente no fim de cada mês",
  "ipca", "Macroeconômica", "IBGE", "Variação mensal do IPCA",
  "desemprego", "Macroeconômica", "PNAD Contínua", "Taxa de desocupação, trimestre móvel datado no último mês",
  "renda_real", "Macroeconômica", "PNAD Contínua", "Rendimento médio real"
)

# =============================================================================
# 12. GRÁFICOS PARA O BLOCO 4
# =============================================================================

tema_projeto <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# -----------------------------------------------------------------------------
# 12.1 Inadimplência PF até 10 SM x PF total
# -----------------------------------------------------------------------------

g1_dados <- base_mensal %>%
  select(data, inad_pf_10sm, inad_pf_total) %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  mutate(
    serie = recode(
      serie,
      inad_pf_10sm = "PF até 10 SM",
      inad_pf_total = "PF total"
    )
  )

g1 <- ggplot(g1_dados, aes(data, valor, linetype = serie)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Inadimplência de pessoas físicas",
    subtitle = "SFN: público até 10 salários mínimos versus PF total",
    x = NULL, y = "Inadimplência (%)", linetype = NULL
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tema_projeto

ggsave(file.path(DIR_OUT, "01_inadimplencia_10sm_vs_total.png"), g1, width = 10, height = 5.5, dpi = 300)

# -----------------------------------------------------------------------------
# 12.2 Boxplot das taxas de juros por modalidade
# -----------------------------------------------------------------------------

vars_juros_plot <- intersect(config_juros$var, names(base_mensal))

if (length(vars_juros_plot) > 0) {
  nomes_juros <- setNames(config_juros$rotulo, config_juros$var)

  g2_dados <- base_mensal %>%
    select(data, all_of(vars_juros_plot)) %>%
    pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
    mutate(serie = recode(serie, !!!nomes_juros)) %>%
    filter(is.finite(valor))

  g2 <- ggplot(g2_dados, aes(x = reorder(serie, valor, median, na.rm = TRUE), y = valor)) +
    geom_boxplot(na.rm = TRUE) +
    coord_flip() +
    labs(
      title = "Custo do crédito PF por modalidade",
      subtitle = "Distribuição mensal, 2021–2025",
      x = NULL,
      y = if (JUROS_ARQUIVO_EH_MENSAL) "Taxa efetiva anual (%)" else "Taxa (%)"
    ) +
    tema_projeto

  ggsave(file.path(DIR_OUT, "02_boxplot_juros_modalidades.png"), g2, width = 10, height = 6.5, dpi = 300)
}

# -----------------------------------------------------------------------------
# 12.3 Evolução do GGR real das Bets
# -----------------------------------------------------------------------------

g3 <- ggplot(base_mensal, aes(data, bets_ggr_real_r_bi)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Evolução do GGR mensal das bets",
    subtitle = "Valores reais a preços de dezembro de 2025",
    x = NULL, y = "R$ bilhões"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tema_projeto

ggsave(file.path(DIR_OUT, "03_ggr_bets_real.png"), g3, width = 10, height = 5.5, dpi = 300)

# -----------------------------------------------------------------------------
# 12.4 Itaú x SFN – frequência trimestral
# -----------------------------------------------------------------------------

g4_dados <- base_setorial %>%
  select(
    data_trimestre,
    itau_npl90_pf_brasil_pct,
    sfn_inad_pf_total_pct,
    sfn_inad_pf_10sm_pct
  ) %>%
  pivot_longer(-data_trimestre, names_to = "serie", values_to = "valor") %>%
  mutate(
    serie = recode(
      serie,
      itau_npl90_pf_brasil_pct = "Itaú – NPL 90 dias PF Brasil",
      sfn_inad_pf_total_pct = "SFN – Inadimplência PF total",
      sfn_inad_pf_10sm_pct = "SFN – Inadimplência PF até 10 SM"
    )
  )

g4 <- ggplot(g4_dados, aes(data_trimestre, valor, linetype = serie)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.6, na.rm = TRUE) +
  labs(
    title = "Itaú e Sistema Financeiro Nacional",
    subtitle = "Comparação descritiva trimestral; definições das séries não são perfeitamente equivalentes",
    x = NULL, y = "%", linetype = NULL
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tema_projeto

ggsave(file.path(DIR_OUT, "04_itau_vs_sfn_trimestral.png"), g4, width = 11, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 12.5 Painel macroeconômico
# -----------------------------------------------------------------------------

g5_dados <- base_mensal %>%
  select(data, selic_fim_mes, ipca, desemprego, renda_real) %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  mutate(
    serie = recode(
      serie,
      selic_fim_mes = "Meta Selic – fim do mês (% a.a.)",
      ipca = "IPCA (% a.m.)",
      desemprego = "Desocupação PNAD (%)",
      renda_real = "Rendimento médio real (R$)"
    )
  )

g5 <- ggplot(g5_dados, aes(data, valor)) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  facet_wrap(~ serie, scales = "free_y", ncol = 2) +
  labs(
    title = "Ambiente macroeconômico",
    subtitle = "Variáveis de controle utilizadas na análise",
    x = NULL, y = NULL
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  tema_projeto +
  theme(legend.position = "none")

ggsave(file.path(DIR_OUT, "05_painel_macroeconomico.png"), g5, width = 11, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# 12.6 Heatmap de correlações descritivas
# -----------------------------------------------------------------------------

nomes_cor <- c(
  inad_pf_10sm = "Inad. até 10 SM",
  inad_pf_total = "Inad. PF total",
  bets_ggr_real_r_bi = "Bets – GGR real",
  selic_fim_mes = "Selic",
  ipca = "IPCA",
  desemprego = "Desemprego",
  renda_real = "Renda real",
  endividamento = "Endividamento",
  comprometimento_juros = "Comprometimento",
  juros_pessoal_nao_consignado_aa = "Juros pessoal não cons.",
  juros_cartao_rotativo_aa = "Juros rotativo",
  juros_cheque_especial_aa = "Juros cheque especial"
)

cor_long <- as.data.frame(mat_cor) %>%
  rownames_to_column("v1") %>%
  pivot_longer(-v1, names_to = "v2", values_to = "cor") %>%
  mutate(
    v1_label = recode(v1, !!!nomes_cor),
    v2_label = recode(v2, !!!nomes_cor),
    v1_label = factor(v1_label, levels = unname(nomes_cor[vars_cor])),
    v2_label = factor(v2_label, levels = rev(unname(nomes_cor[vars_cor])))
  )

# n de pares para cada célula
n_long <- as.data.frame(mat_n) %>%
  rownames_to_column("v1") %>%
  pivot_longer(-v1, names_to = "v2", values_to = "n")

cor_long <- cor_long %>%
  left_join(n_long, by = c("v1", "v2")) %>%
  mutate(rotulo = ifelse(n >= 12, sprintf("%.2f", cor), paste0("n=", n)))

g6 <- ggplot(cor_long, aes(v1_label, v2_label, fill = cor)) +
  geom_tile() +
  geom_text(aes(label = rotulo), size = 2.6) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Correlações descritivas – 2021 a 2025",
    subtitle = "Correlação de Pearson em nível; não implica causalidade",
    x = NULL, y = NULL, fill = "Correlação"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(DIR_OUT, "06_heatmap_correlacoes.png"), g6, width = 10.5, height = 9, dpi = 300)

# =============================================================================
# 13. EXPORTAÇÃO PARA EXCEL
# =============================================================================

# Arredondamento apenas nas tabelas de apresentação; a base mensal permanece
# com precisão original.
formatar_tab <- function(df, digitos = 3) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digitos)))
}

wb <- createWorkbook()

abas <- list(
  "4_1_Financeira" = formatar_tab(tab_financeira),
  "4_2_Juros" = formatar_tab(tab_juros),
  "4_3_Comportamental" = formatar_tab(tab_comportamental),
  "4_4_Setorial" = formatar_tab(tab_setorial),
  "4_5_Macroeconomica" = formatar_tab(tab_macro),
  "Descritivas_Completas" = formatar_tab(tab_descritivas_completa),
  "Evolucao_2021_2025" = formatar_tab(tab_evolucao),
  "Medias_Anuais" = formatar_tab(medias_anuais),
  "Correlacoes" = formatar_tab(cor_df),
  "N_Correlacoes" = cor_n_df,
  "Base_Mensal_2021_2025" = base_mensal,
  "Base_Setorial_Trimestral" = base_setorial,
  "Qualidade_Dados" = formatar_tab(qualidade_dados),
  "Diagnostico_Importacao" = mapa_juros_importacao,
  "Dicionario" = dicionario
)

estilo_header <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fgFill = "#D9E2F3",
  border = "Bottom"
)

for (nome_aba in names(abas)) {
  addWorksheet(wb, nome_aba)
  writeData(wb, nome_aba, abas[[nome_aba]], headerStyle = estilo_header, withFilter = TRUE)
  freezePane(wb, nome_aba, firstRow = TRUE)
  setColWidths(wb, nome_aba, cols = 1:ncol(abas[[nome_aba]]), widths = "auto")
}

saveWorkbook(
  wb,
  file.path(DIR_OUT, "estatisticas_descritivas_bloco4.xlsx"),
  overwrite = TRUE
)

# CSVs individuais também facilitam uso no Word/LaTeX.
readr::write_csv(tab_financeira, file.path(DIR_OUT, "tabela_4_1_financeira.csv"), na = "")
readr::write_csv(tab_juros, file.path(DIR_OUT, "tabela_4_2_juros_modalidades.csv"), na = "")
readr::write_csv(tab_comportamental, file.path(DIR_OUT, "tabela_4_3_comportamental_bets.csv"), na = "")
readr::write_csv(tab_setorial, file.path(DIR_OUT, "tabela_4_4_setorial_itau_sfn.csv"), na = "")
readr::write_csv(tab_macro, file.path(DIR_OUT, "tabela_4_5_macroeconomica.csv"), na = "")
readr::write_csv(base_mensal, file.path(DIR_OUT, "base_mensal_bloco4_2021_2025.csv"), na = "")

# =============================================================================
# 14. RESULTADOS NO CONSOLE
# =============================================================================

cat("\n============================================================\n")
cat("ESTATÍSTICAS DESCRITIVAS – BLOCO 4 CONCLUÍDAS\n")
cat("============================================================\n\n")

cat("Amostra principal:", format(DATA_INICIO, "%m/%Y"), "a", format(DATA_FIM, "%m/%Y"), "\n")
cat("Número de meses no calendário:", nrow(base_mensal), "\n\n")

cat("--- 4.1 Financeira ---\n")
print(formatar_tab(tab_financeira), n = Inf)

cat("\n--- 4.2 Juros por modalidade ---\n")
print(formatar_tab(tab_juros), n = Inf)

cat("\n--- 4.3 Comportamental / Bets ---\n")
print(formatar_tab(tab_comportamental), n = Inf)

cat("\n--- 4.4 Setorial / Itaú x SFN ---\n")
print(formatar_tab(tab_setorial), n = Inf)

cat("\n--- 4.5 Macroeconômica ---\n")
print(formatar_tab(tab_macro), n = Inf)

cat("\nArquivos salvos em:\n", DIR_OUT, "\n")
cat("\nPrincipal arquivo: estatisticas_descritivas_bloco4.xlsx\n")

# =============================================================================
# FIM
# =============================================================================
