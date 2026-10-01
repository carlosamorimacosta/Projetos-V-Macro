# ============================================================================
# SFN – INADIMPLÊNCIA PF ATÉ 10 SALÁRIOS MÍNIMOS
# ARDL REVISADO: DIAGNÓSTICOS, ESTACIONARIEDADE, QUEBRAS, MULTICOLINEARIDADE,
# JUROS LIVRES PF, BETS REAIS, BENCHMARK AR(4) E VALIDAÇÃO FORA DA AMOSTRA
#
# PRINCIPAIS MUDANÇAS EM RELAÇÃO AO SCRIPT ANTERIOR:
# 1) Modelo principal usa JUROS LIVRES PF diretamente, em vez de Selic + spread.
# 2) Selic isolada e Selic + spread ficam como robustez.
# 3) max_p = 4 e max_q = 2 para reduzir overfitting/post-selection.
# 4) Bets são deflacionadas e entram em log no modelo principal.
# 5) É criada também uma proxy de intensidade Bets/Renda, normalizada em índice.
# 6) Renda entra em log e há opção explícita para evitar dupla deflação.
# 7) Testes de raiz unitária são ampliados: ADF (none/drift/trend), PP, KPSS,
#    1ª e 2ª diferenças e Zivot-Andrews.
# 8) Bounds só é interpretado se não houver evidência/inconclusão sobre I(2).
# 9) CUSUM, MOSUM, Bai-Perron e robustez com tendência são calculados.
# 10) VIF é reportado para todos os termos e separadamente para regressoras
#     exógenas, excluindo lags da dependente.
# 11) Especificação vencedora sem Bets é reestimada na amostra máxima 2020–2026.
# 12) É incluído benchmark AR(4) puro.
# 13) É feita avaliação one-step-ahead em janela final, com RMSE e MAE.
# 14) É feito teste Wald-HAC conjunto dos lags de Bets.
#
# ATENÇÃO:
# - O GGR nacional não é uma medida direta da exposição das famílias <=10 SM.
# - A proxy Bets/Renda abaixo é apenas uma ROBUSTEZ de intensidade, não uma
#   medida literal de "percentual da renda apostado".
# - Confirme se o arquivo PNAD de renda já é "rendimento médio REAL".
#   Se já for real, mantenha renda_ja_real <- TRUE.
# ============================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(2026)

# ============================================================================
# 0. CONFIGURAÇÕES
# ============================================================================

data_inicio_desejado <- as.Date("2020-01-01")
data_fim_desejado    <- as.Date("2026-07-31")

# ----------------------- CAMINHOS --------------------------------------------

arquivo_inad_10sm <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplência 10sm/Base_Final_Inadimplencia_PF.xlsx"
arquivo_inad_geral <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplencia de crédito - pesssoa física - SGS.csv"

arquivo_spread <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Spread_Mensal_Credito_Livre_PF.csv"
arquivo_selic  <- arquivo_spread

# CAMINHO INFORMADO PELO USUÁRIO
arquivo_juros_livre_bruto <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Taxas médias das op de crédito livre - modalidades - completa.csv"

arquivo_ipca <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/ipca_202606SerieHist.xls"
arquivo_desemprego <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Desemprego Pnad.csv"
arquivo_renda <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Renda/rendimento médio pnad.csv"
arquivo_bets <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Bets/Bets_GGR_Mensal_2021_2025_estimado.xlsx"

aba_inad_10sm <- "Base Mensal"
aba_ipca <- 1
aba_bets <- "Base_mensal"

# IMPORTANTE: marque FALSE apenas se você confirmar que a renda do CSV é nominal.
renda_ja_real <- TRUE

# ARDL mais parcimonioso
max_p <- 4
max_q <- 2
min_obs_por_coef <- 4

lag_diagnostico <- 12
max_lag_adf <- 6

# Holdout para robustez preditiva:
# os últimos 12 meses utilizáveis serão avaliados one-step-ahead.
n_holdout <- 12

usar_bounds_exato <- FALSE
R_bounds_exato <- 40000

dir_saida <- "C:/Users/carlo/Downloads/output_SFN_ARDL_revisado"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# ============================================================================
# 1. PACOTES
# ============================================================================

pacotes <- c(
  "tidyverse", "lubridate", "zoo", "ARDL", "lmtest", "sandwich",
  "tseries", "urca", "car", "strucchange", "ggplot2", "openxlsx", "readxl"
)

instalar_ausentes <- function(pkgs) {
  ausentes <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ausentes) > 0) install.packages(ausentes, dependencies = TRUE)
}
instalar_ausentes(pacotes)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(ARDL)
  library(lmtest)
  library(sandwich)
  library(tseries)
  library(urca)
  library(car)
  library(strucchange)
  library(ggplot2)
  library(openxlsx)
  library(readxl)
})

# ============================================================================
# 2. FUNÇÕES DE IMPORTAÇÃO
# ============================================================================

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
      s, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )
  } else {
    out <- readr::parse_number(
      s, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    )
  }
  as.numeric(out)
}

parse_data_mensal <- function(x) {
  if (inherits(x, "Date")) return(floor_date(x, "month"))
  if (inherits(x, c("POSIXct", "POSIXt"))) return(floor_date(as.Date(x), "month"))
  
  if (is.numeric(x)) {
    xx <- as.numeric(x)
    med <- suppressWarnings(median(xx, na.rm = TRUE))
    
    if (is.finite(med) && med > 20000 && med < 80000) {
      return(floor_date(as.Date(xx, origin = "1899-12-30"), "month"))
    }
    
    if (all(is.na(xx) | (xx >= 190001 & xx <= 210012))) {
      s <- sprintf("%06d", as.integer(xx))
      return(as.Date(paste0(substr(s, 1, 4), "-", substr(s, 5, 6), "-01")))
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
    out[idx2] <- as.Date(vapply(
      p,
      function(z) sprintf("%04d-%02d-01", as.integer(z[2]), as.integer(z[1])),
      character(1)
    ))
  }
  
  idx3 <- is.na(out) & grepl("^\\d{6}$", s)
  if (any(idx3, na.rm = TRUE)) {
    ss <- s[idx3]
    out[idx3] <- as.Date(
      paste0(substr(ss, 1, 4), "-", substr(ss, 5, 6), "-01")
    )
  }
  
  idx4 <- is.na(out) & !is.na(s)
  if (any(idx4)) {
    d <- suppressWarnings(lubridate::parse_date_time(
      s[idx4],
      orders = c("Ymd", "Y-m-d", "Y/m/d", "dmy", "d/m/Y", "d-m-Y",
                 "mdy", "m/d/Y", "m-d-Y"),
      quiet = TRUE
    ))
    out[idx4] <- as.Date(d)
  }
  
  floor_date(out, "month")
}

mes_pt_numero <- function(x) {
  s <- iconv(tolower(trimws(as.character(x))), from = "", to = "ASCII//TRANSLIT")
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

# Formato BCB: jul/11, ago/11, ...
parse_mes_aa_bcb <- function(x) {
  s <- iconv(tolower(trimws(as.character(x))), from = "", to = "ASCII//TRANSLIT")
  partes <- strsplit(s, "/", fixed = TRUE)
  
  out <- vapply(partes, function(z) {
    if (length(z) != 2) return(NA_character_)
    m <- mes_pt_numero(z[1])
    yy <- suppressWarnings(as.integer(z[2]))
    if (is.na(m) || is.na(yy)) return(NA_character_)
    ano <- ifelse(yy <= 69, 2000 + yy, 1900 + yy)
    sprintf("%04d-%02d-01", ano, m)
  }, character(1))
  
  as.Date(out)
}

validar_serie_mensal <- function(df, nome) {
  if (!all(c("data", nome) %in% names(df))) {
    stop("Estrutura inválida para a série ", nome, ".")
  }
  
  df <- df %>% arrange(data)
  
  if (anyDuplicated(df$data)) {
    print(df %>% count(data) %>% filter(n > 1))
    stop("A série ", nome, " possui mais de uma observação no mesmo mês.")
  }
  
  if (all(is.na(df[[nome]]))) {
    stop("A série ", nome, " foi importada, mas todos os valores são NA.")
  }
  
  df
}

detectar_encoding_csv <- function(caminho) {
  enc <- tryCatch(readr::guess_encoding(caminho, n_max = 1000), error = function(e) NULL)
  if (is.null(enc) || nrow(enc) == 0 || is.na(enc$encoding[1])) return("UTF-8")
  enc$encoding[1]
}

ler_arquivo_generico <- function(caminho, aba = 1) {
  if (!file.exists(caminho)) {
    stop("\nArquivo não encontrado:\n", caminho,
         "\n\nCorrija o caminho no bloco CONFIGURAÇÕES.")
  }
  
  ext <- tolower(tools::file_ext(caminho))
  
  if (ext %in% c("xlsx", "xlsm", "xls")) {
    df <- readxl::read_excel(caminho, sheet = aba, .name_repair = "unique")
  } else {
    encoding_usar <- detectar_encoding_csv(caminho)
    
    primeira <- readr::read_lines(
      caminho, n_max = 1,
      locale = readr::locale(encoding = encoding_usar),
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
    
    df <- readr::read_delim(
      caminho,
      delim = delim,
      locale = readr::locale(encoding = encoding_usar),
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
  
  achou <- intersect(alternativas, names(df))
  if (length(achou) > 0) return(achou[1])
  
  nomes_sem_codigo <- sub("^[0-9]+_", "", names(df))
  for (alt in alternativas) {
    idx <- which(nomes_sem_codigo == alt)
    if (length(idx) == 1) return(names(df)[idx])
  }
  
  for (alt in alternativas) {
    idx <- which(grepl(alt, names(df), fixed = TRUE))
    if (length(idx) == 1) return(names(df)[idx])
  }
  
  stop(
    "\nNão encontrei a coluna de ", nome_logico,
    ".\nNomes aceitos: ", paste(alternativas, collapse = ", "),
    "\nColunas encontradas: ", paste(names(df), collapse = ", ")
  )
}

ler_serie_unica <- function(caminho, aba, nome_final, alternativas_valor) {
  df <- ler_arquivo_generico(caminho, aba)
  
  col_data <- encontrar_coluna(
    df,
    c("data", "date", "mes", "mes_ano", "competencia", "periodo"),
    "data"
  )
  
  col_valor <- encontrar_coluna(df, alternativas_valor, nome_final)
  
  out <- df %>%
    transmute(
      data = parse_data_mensal(.data[[col_data]]),
      valor = parse_numero(.data[[col_valor]])
    ) %>%
    filter(
      !is.na(data),
      data >= floor_date(data_inicio_desejado, "month"),
      data <= floor_date(data_fim_desejado, "month")
    ) %>%
    arrange(data)
  
  names(out)[2] <- nome_final
  validar_serie_mensal(out, nome_final)
}

# ============================================================================
# 3. LEITORES ESPECÍFICOS
# ============================================================================

ler_ipca_ibge <- function(caminho, aba = 1) {
  raw <- readxl::read_excel(
    caminho, sheet = aba, col_names = FALSE, .name_repair = "minimal"
  ) %>% as.data.frame(check.names = FALSE)
  
  if (ncol(raw) < 4) stop("Arquivo histórico do IPCA possui menos de 4 colunas.")
  
  ano_raw <- suppressWarnings(parse_numero(raw[[1]]))
  ano <- ifelse(
    is.finite(ano_raw) & ano_raw >= 1900 & ano_raw <= 2100,
    as.integer(ano_raw), NA_integer_
  )
  ano <- zoo::na.locf(ano, na.rm = FALSE)
  
  mes <- mes_pt_numero(raw[[2]])
  ipca <- suppressWarnings(parse_numero(raw[[4]]))
  
  valido <- !is.na(ano) & !is.na(mes) & mes >= 1 & mes <= 12 & !is.na(ipca)
  
  out <- tibble(
    data = as.Date(sprintf("%04d-%02d-01", ano[valido], mes[valido])),
    ipca = as.numeric(ipca[valido])
  ) %>%
    filter(
      data >= floor_date(data_inicio_desejado, "month"),
      data <= floor_date(data_fim_desejado, "month")
    ) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
  
  validar_serie_mensal(out, "ipca")
}

parse_periodo_pnad <- function(periodo) {
  s <- iconv(tolower(trimws(as.character(periodo))), from = "", to = "ASCII//TRANSLIT")
  
  ano <- suppressWarnings(as.integer(sub(".*\\s([0-9]{4})$", "\\1", s)))
  ultimo_mes <- sub(".*-([a-z]+)\\s[0-9]{4}$", "\\1", s)
  mes <- mes_pt_numero(ultimo_mes)
  
  ok <- !is.na(ano) & !is.na(mes)
  out <- rep(as.Date(NA), length(s))
  out[ok] <- as.Date(sprintf("%04d-%02d-01", ano[ok], mes[ok]))
  out
}

ler_linha_brasil_pnad <- function(caminho, nome_final) {
  encoding_usar <- detectar_encoding_csv(caminho)
  linhas <- readr::read_lines(
    caminho,
    locale = readr::locale(encoding = encoding_usar),
    progress = FALSE
  )
  
  i_brasil <- which(grepl("^\\s*Brasil\\s*;", linhas, ignore.case = TRUE))[1]
  if (is.na(i_brasil) || i_brasil <= 1) stop("Não encontrei a linha Brasil: ", caminho)
  
  periodo_raw <- strsplit(linhas[i_brasil - 1], ";", fixed = TRUE)[[1]]
  valor_raw   <- strsplit(linhas[i_brasil], ";", fixed = TRUE)[[1]]
  
  n <- min(length(periodo_raw), length(valor_raw))
  periodo_raw <- trimws(periodo_raw[2:n])
  valor_raw   <- trimws(valor_raw[2:n])
  
  out <- tibble(
    data = parse_periodo_pnad(periodo_raw),
    valor = parse_numero(valor_raw)
  ) %>%
    filter(
      !is.na(data), !is.na(valor),
      data >= floor_date(data_inicio_desejado, "month"),
      data <= floor_date(data_fim_desejado, "month")
    ) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
  
  names(out)[2] <- nome_final
  validar_serie_mensal(out, nome_final)
}

ler_bets_ggr_mensal <- function(caminho, aba = "Base_mensal") {
  raw <- readxl::read_excel(
    caminho, sheet = aba, skip = 2, .name_repair = "unique"
  ) %>% as.data.frame(check.names = FALSE)
  
  names(raw) <- normalizar_nome(names(raw))
  
  col_data <- encontrar_coluna(
    raw, c("data", "date", "mes_ano", "competencia", "periodo"),
    "data da série mensal de Bets"
  )
  
  col_bets <- encontrar_coluna(
    raw,
    c("ggr_bets_r_bi", "ggr_bets_rbi", "ggr_bets", "ggr", "bets"),
    "GGR mensal das Bets"
  )
  
  out <- raw %>%
    transmute(
      data = parse_data_mensal(.data[[col_data]]),
      bets = parse_numero(.data[[col_bets]])
    ) %>%
    filter(
      !is.na(data), !is.na(bets),
      data >= floor_date(data_inicio_desejado, "month"),
      data <= floor_date(data_fim_desejado, "month")
    ) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
  
  validar_serie_mensal(out, "bets")
}

# ----------------------------------------------------------------------------
# JUROS LIVRES PF – série 25462 "Total" (% a.m.)
#
# O arquivo fornecido contém várias modalidades. Para evitar escolha arbitrária
# de cartão, cheque especial etc., usamos a série agregada PF - TOTAL (25462).
#
# Conversão para taxa efetiva anual:
# (1 + i_mensal)^12 - 1
# ----------------------------------------------------------------------------

ler_juros_livre_pf_total <- function(caminho) {
  encoding_usar <- detectar_encoding_csv(caminho)
  
  raw <- readr::read_delim(
    caminho,
    delim = ";",
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = encoding_usar
    ),
    col_types = readr::cols(.default = "c"),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "unique"
  ) %>% as.data.frame()
  
  names(raw) <- normalizar_nome(names(raw))
  
  col_data <- encontrar_coluna(raw, c("data"), "data dos juros livres PF")
  
  # Busca PRIORITARIAMENTE o código 25462
  candidatos <- names(raw)[grepl("^25462_", names(raw))]
  
  if (length(candidatos) != 1) {
    candidatos <- names(raw)[
      grepl("pessoas_fisicas_total", names(raw), fixed = TRUE) &
        grepl("recursos_livres", names(raw), fixed = TRUE)
    ]
  }
  
  if (length(candidatos) != 1) {
    stop(
      "Não consegui identificar unicamente a série 25462 - ",
      "Taxa média mensal de juros livres PF - Total."
    )
  }
  
  col_juros <- candidatos[1]
  
  out <- raw %>%
    transmute(
      data = parse_mes_aa_bcb(.data[[col_data]]),
      juros_livre_pf_am = parse_numero(.data[[col_juros]])
    ) %>%
    filter(
      !is.na(data), !is.na(juros_livre_pf_am),
      data >= floor_date(data_inicio_desejado, "month"),
      data <= floor_date(data_fim_desejado, "month")
    ) %>%
    mutate(
      juros_livre_pf_aa = ((1 + juros_livre_pf_am / 100)^12 - 1) * 100
    ) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
  
  validar_serie_mensal(out %>% select(data, juros_livre_pf_aa), "juros_livre_pf_aa")
  out
}

# ============================================================================
# 4. IMPORTAÇÃO
# ============================================================================

message("\n============================================================")
message("1. IMPORTANDO AS SÉRIES")
message("============================================================")

inad_10sm <- ler_serie_unica(
  arquivo_inad_10sm, aba_inad_10sm, "inad_pf_10sm",
  c(
    "inadimplencia_pf_ate10sm_pct", "inadimplencia_pf_ate_10sm_pct",
    "inad_pf_10sm", "inadimplencia_pf_10sm", "inadimplencia_pf_ate10sm"
  )
)

inad_geral <- ler_serie_unica(
  arquivo_inad_geral, 1, "inad_pf_geral",
  c(
    "inadimplencia_da_carteira_de_credito_pessoas_fisicas_total",
    "inadimplencia_pf_total", "inadimplencia", "valor"
  )
)

selic_df <- ler_serie_unica(
  arquivo_selic, 1, "selic",
  c("selic_media_mensal_pct_aa", "selic_media_mensal", "selic_meta", "selic")
)

spread_df <- ler_serie_unica(
  arquivo_spread, 1, "spread_livre_pf",
  c(
    "spread_livre_pf_pp_aa", "spread_livre_pf",
    "spread_credito_livre_pf", "spread_pf"
  )
)

juros_livre_df <- ler_juros_livre_pf_total(arquivo_juros_livre_bruto)

ipca_df <- ler_ipca_ibge(arquivo_ipca, aba_ipca)
desemprego_df <- ler_linha_brasil_pnad(arquivo_desemprego, "desemprego")
renda_df <- ler_linha_brasil_pnad(arquivo_renda, "renda")
bets_df <- ler_bets_ggr_mensal(arquivo_bets, aba_bets)

# ============================================================================
# 5. BASE MESTRA, DEFLATOR, RENDA REAL E BETS REAIS
# ============================================================================

message("\n============================================================")
message("2. CONSTRUINDO BASE MESTRA")
message("============================================================")

data_inicio_master <- min(
  c(
    min(inad_10sm$data), min(inad_geral$data), min(selic_df$data),
    min(spread_df$data), min(juros_livre_df$data), min(ipca_df$data),
    min(desemprego_df$data), min(renda_df$data), min(bets_df$data)
  ),
  na.rm = TRUE
)

data_fim_master <- max(
  c(
    max(inad_10sm$data), max(inad_geral$data), max(selic_df$data),
    max(spread_df$data), max(juros_livre_df$data), max(ipca_df$data),
    max(desemprego_df$data), max(renda_df$data), max(bets_df$data)
  ),
  na.rm = TRUE
)

base_master <- tibble(
  data = seq.Date(data_inicio_master, data_fim_master, by = "month")
) %>%
  left_join(inad_10sm, by = "data") %>%
  left_join(inad_geral, by = "data") %>%
  left_join(selic_df, by = "data") %>%
  left_join(spread_df, by = "data") %>%
  left_join(juros_livre_df, by = "data") %>%
  left_join(ipca_df, by = "data") %>%
  left_join(desemprego_df, by = "data") %>%
  left_join(renda_df, by = "data") %>%
  left_join(bets_df, by = "data") %>%
  arrange(data)

# Índice de preços encadeado a partir do IPCA mensal.
# Base arbitrária = 100 no primeiro mês disponível; relações reais não dependem
# da base escolhida.
base_master <- base_master %>%
  mutate(
    fator_ipca = 1 + ipca / 100,
    indice_precos = ifelse(!is.na(ipca), cumprod(replace_na(fator_ipca, 1)), NA_real_)
  )

# Como cumprod com NAs fora da cobertura pode ser inconveniente, reconstruímos
# o índice apenas no bloco observado do IPCA.
idx_ipca <- which(!is.na(base_master$ipca))
if (length(idx_ipca) > 0) {
  fator <- 1 + base_master$ipca[idx_ipca] / 100
  base_master$indice_precos[idx_ipca] <- 100 * cumprod(fator) / cumprod(fator)[1]
}

# Referência de preços: último mês de 2025 se disponível; caso contrário,
# último mês do IPCA.
ref_data <- as.Date("2025-12-01")
ref_idx <- which(base_master$data == ref_data & !is.na(base_master$indice_precos))

if (length(ref_idx) == 0) {
  ref_idx <- tail(which(!is.na(base_master$indice_precos)), 1)
}
indice_ref <- base_master$indice_precos[ref_idx]

base_master <- base_master %>%
  mutate(
    renda_original = renda,
    
    # Evita dupla deflação se a PNAD já for "rendimento médio real".
    renda_real = if (renda_ja_real) {
      renda
    } else {
      renda * indice_ref / indice_precos
    },
    
    log_renda_real = ifelse(renda_real > 0, log(renda_real), NA_real_),
    
    # Bets em preços da referência.
    bets_real = ifelse(
      !is.na(bets) & !is.na(indice_precos),
      bets * indice_ref / indice_precos,
      NA_real_
    ),
    
    log_bets_real = ifelse(bets_real > 0, log(bets_real), NA_real_)
  )

# Proxy de intensidade: GGR real / renda real, normalizada = 100 no primeiro mês.
# NÃO interpretar como fração literal da renda agregada das famílias.
primeiro_ratio <- base_master %>%
  filter(!is.na(bets_real), !is.na(renda_real), renda_real > 0) %>%
  transmute(ratio = bets_real / renda_real) %>%
  slice(1) %>%
  pull(ratio)

base_master <- base_master %>%
  mutate(
    bets_intensidade_idx = ifelse(
      !is.na(bets_real) & !is.na(renda_real) & renda_real > 0,
      100 * (bets_real / renda_real) / primeiro_ratio,
      NA_real_
    ),
    gap_inad_10sm_geral = inad_pf_10sm - inad_pf_geral,
    tendencia = row_number()
  )

# Validação da identidade aproximada juros = Selic + spread apenas para diagnóstico.
base_master <- base_master %>%
  mutate(
    juros_implicito_selic_spread = selic + spread_livre_pf,
    diff_juros_raw_vs_implicito = juros_livre_pf_aa - juros_implicito_selic_spread
  )

# ============================================================================
# 6. AMOSTRAS
# ============================================================================

vars_com_bets <- c(
  "inad_pf_10sm", "juros_livre_pf_aa", "selic", "spread_livre_pf",
  "ipca", "desemprego", "log_renda_real", "log_bets_real",
  "bets_intensidade_idx"
)

vars_sem_bets <- c(
  "inad_pf_10sm", "juros_livre_pf_aa", "selic", "spread_livre_pf",
  "ipca", "desemprego", "log_renda_real"
)

base_comum <- base_master %>%
  filter(
    data >= data_inicio_desejado,
    data <= data_fim_desejado
  ) %>%
  filter(if_all(all_of(vars_com_bets), ~ !is.na(.x))) %>%
  arrange(data)

base_sem_bets_full <- base_master %>%
  filter(
    data >= data_inicio_desejado,
    data <= data_fim_desejado
  ) %>%
  filter(if_all(all_of(vars_sem_bets), ~ !is.na(.x))) %>%
  arrange(data)

if (nrow(base_comum) < 36) {
  stop("A amostra comum com Bets ficou pequena demais: ", nrow(base_comum), " observações.")
}

data_inicio <- min(base_comum$data)
data_fim    <- max(base_comum$data)

message(
  "Amostra comum COMPARÁVEL com/sem Bets: ",
  format(data_inicio, "%Y-%m"), " a ", format(data_fim, "%Y-%m"),
  " (n=", nrow(base_comum), ")."
)

message(
  "Amostra máxima SEM Bets: ",
  format(min(base_sem_bets_full$data), "%Y-%m"), " a ",
  format(max(base_sem_bets_full$data), "%Y-%m"),
  " (n=", nrow(base_sem_bets_full), ")."
)

# ============================================================================
# 7. DESCRITIVAS, CORRELAÇÕES E CORRELAÇÃO COM TENDÊNCIA
# ============================================================================

vars_desc <- c(
  "inad_pf_10sm", "inad_pf_geral", "selic", "spread_livre_pf",
  "juros_livre_pf_aa", "ipca", "desemprego", "renda_real",
  "log_renda_real", "bets", "bets_real", "log_bets_real",
  "bets_intensidade_idx", "gap_inad_10sm_geral"
)

vars_desc <- intersect(vars_desc, names(base_comum))

descritivas <- map_dfr(vars_desc, function(v) {
  x <- base_comum[[v]]
  tibble(
    variavel = v,
    media = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    minimo = min(x, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE),
    desvio_padrao = sd(x, na.rm = TRUE)
  )
})

mat_cor <- cor(
  base_comum %>% select(all_of(vars_desc)),
  use = "pairwise.complete.obs"
)

cor_tendencia <- map_dfr(
  setdiff(vars_desc, "gap_inad_10sm_geral"),
  function(v) {
    ok <- complete.cases(base_comum[[v]], base_comum$tendencia)
    tibble(
      variavel = v,
      correlacao_com_tempo = cor(
        base_comum[[v]][ok],
        base_comum$tendencia[ok]
      )
    )
  }
) %>% arrange(desc(abs(correlacao_com_tempo)))

# ============================================================================
# 8. TESTES DE ESTACIONARIEDADE AMPLIADOS
# ============================================================================

message("\n============================================================")
message("3. TESTES DE ESTACIONARIEDADE AMPLIADOS")
message("============================================================")

crit5 <- function(obj, stat_name = NULL) {
  cv <- obj@cval
  if (is.null(dim(cv))) {
    return(as.numeric(cv["5pct"]))
  }
  
  if (!is.null(stat_name) && stat_name %in% rownames(cv)) {
    return(as.numeric(cv[stat_name, "5pct"]))
  }
  
  as.numeric(cv[1, "5pct"])
}

adf_decisao <- function(z, tipo = c("none", "drift", "trend")) {
  tipo <- match.arg(tipo)
  z <- as.numeric(na.omit(z))
  
  if (length(z) < 20) return(list(stat = NA_real_, crit = NA_real_, rejeita = NA))
  
  obj <- tryCatch(
    urca::ur.df(
      z,
      type = tipo,
      lags = min(max_lag_adf, floor(length(z) / 5)),
      selectlags = "BIC"
    ),
    error = function(e) NULL
  )
  
  if (is.null(obj)) return(list(stat = NA_real_, crit = NA_real_, rejeita = NA))
  
  stat_name <- switch(tipo, none = "tau1", drift = "tau2", trend = "tau3")
  stat <- tryCatch(as.numeric(obj@teststat[1, stat_name]), error = function(e) as.numeric(obj@teststat[1]))
  cv <- tryCatch(crit5(obj, stat_name), error = function(e) NA_real_)
  
  list(stat = stat, crit = cv, rejeita = is.finite(stat) && is.finite(cv) && stat < cv)
}

pp_decisao <- function(z, modelo = c("constant", "trend")) {
  modelo <- match.arg(modelo)
  z <- as.numeric(na.omit(z))
  
  obj <- tryCatch(
    urca::ur.pp(z, type = "Z-tau", model = modelo, lags = "short"),
    error = function(e) NULL
  )
  
  if (is.null(obj)) return(list(stat = NA_real_, crit = NA_real_, rejeita = NA))
  
  stat <- as.numeric(obj@teststat)
  cv <- tryCatch(as.numeric(obj@cval["5pct"]), error = function(e) NA_real_)
  
  list(stat = stat, crit = cv, rejeita = is.finite(stat) && is.finite(cv) && stat < cv)
}

kpss_decisao <- function(z, tipo = c("mu", "tau")) {
  tipo <- match.arg(tipo)
  z <- as.numeric(na.omit(z))
  
  obj <- tryCatch(
    urca::ur.kpss(z, type = tipo, lags = "short"),
    error = function(e) NULL
  )
  
  if (is.null(obj)) return(list(stat = NA_real_, crit = NA_real_, estacionaria = NA))
  
  stat <- as.numeric(obj@teststat)
  cv <- tryCatch(as.numeric(obj@cval["5pct"]), error = function(e) NA_real_)
  
  # KPSS H0 = estacionariedade
  list(
    stat = stat,
    crit = cv,
    estacionaria = is.finite(stat) && is.finite(cv) && stat < cv
  )
}

za_resultado <- function(z) {
  z <- as.numeric(na.omit(z))
  lag_za <- min(4, max(1, floor(length(z)^(1/3))))
  
  obj <- tryCatch(
    urca::ur.za(z, model = "both", lag = lag_za),
    error = function(e) NULL
  )
  
  if (is.null(obj)) {
    return(list(stat = NA_real_, break_index = NA_integer_, rejeita = NA))
  }
  
  stat <- as.numeric(obj@teststat)
  # ur.za tem críticos específicos; 5% normalmente em cval["5pct"]
  cv <- tryCatch(as.numeric(obj@cval["5pct"]), error = function(e) NA_real_)
  
  list(
    stat = stat,
    break_index = tryCatch(as.integer(obj@bpoint), error = function(e) NA_integer_),
    rejeita = is.finite(stat) && is.finite(cv) && stat < cv
  )
}

avaliar_ordem <- function(x, nome) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  
  roda_bloco <- function(z) {
    a_none  <- adf_decisao(z, "none")
    a_drift <- adf_decisao(z, "drift")
    a_trend <- adf_decisao(z, "trend")
    pp_c    <- pp_decisao(z, "constant")
    pp_t    <- pp_decisao(z, "trend")
    kp_mu   <- kpss_decisao(z, "mu")
    kp_tau  <- kpss_decisao(z, "tau")
    
    # Núcleo principal: ADF drift, PP constant, KPSS level.
    sinais_core <- c(
      a_drift$rejeita,
      pp_c$rejeita,
      kp_mu$estacionaria
    )
    
    estacionaria_core <- sum(sinais_core %in% TRUE, na.rm = TRUE) >= 2
    
    list(
      adf_none = a_none,
      adf_drift = a_drift,
      adf_trend = a_trend,
      pp_constant = pp_c,
      pp_trend = pp_t,
      kpss_mu = kp_mu,
      kpss_tau = kp_tau,
      estacionaria_core = estacionaria_core
    )
  }
  
  nivel <- roda_bloco(x)
  d1 <- roda_bloco(diff(x))
  d2 <- roda_bloco(diff(x, differences = 2))
  za <- za_resultado(x)
  
  ordem <- case_when(
    nivel$estacionaria_core ~ "I(0)",
    d1$estacionaria_core ~ "I(1)",
    !d1$estacionaria_core && d2$estacionaria_core ~ "Possível I(2)",
    TRUE ~ "Inconclusivo"
  )
  
  tibble(
    variavel = nome,
    integracao = ordem,
    
    adf_drift_nivel_stat = nivel$adf_drift$stat,
    adf_drift_nivel_crit5 = nivel$adf_drift$crit,
    adf_drift_nivel_rejeita = nivel$adf_drift$rejeita,
    
    adf_trend_nivel_rejeita = nivel$adf_trend$rejeita,
    pp_const_nivel_rejeita = nivel$pp_constant$rejeita,
    pp_trend_nivel_rejeita = nivel$pp_trend$rejeita,
    kpss_level_nivel_estacionaria = nivel$kpss_mu$estacionaria,
    kpss_trend_nivel_estacionaria = nivel$kpss_tau$estacionaria,
    
    d1_core_estacionaria = d1$estacionaria_core,
    d2_core_estacionaria = d2$estacionaria_core,
    
    za_stat = za$stat,
    za_break_index = za$break_index,
    za_rejeita_raiz_com_quebra = za$rejeita
  )
}

vars_est <- c(
  "inad_pf_10sm",
  "juros_livre_pf_aa",
  "selic",
  "spread_livre_pf",
  "ipca",
  "desemprego",
  "log_renda_real",
  "log_bets_real",
  "bets_intensidade_idx"
)

tab_estacionariedade <- map_dfr(
  vars_est,
  ~ avaliar_ordem(base_comum[[.x]], .x)
)

print(tab_estacionariedade, n = Inf)

bounds_seguro <- !any(
  tab_estacionariedade$integracao %in% c("Possível I(2)", "Inconclusivo")
)

if (!bounds_seguro) {
  warning(
    "Bounds/ECM serão BLOQUEADOS porque ao menos uma série ainda é ",
    "Possível I(2) ou Inconclusiva. Consulte a aba estacionariedade."
  )
}

# ============================================================================
# 9. PREPARAÇÃO DAS DEFASAGENS
# ============================================================================

preparar_lags <- function(df) {
  df %>%
    arrange(data) %>%
    mutate(
      juros_livre_pf_aa_L1 = lag(juros_livre_pf_aa, 1),
      selic_L1 = lag(selic, 1),
      spread_livre_pf_L1 = lag(spread_livre_pf, 1),
      ipca_L1 = lag(ipca, 1),
      desemprego_L1 = lag(desemprego, 1),
      log_renda_real_L1 = lag(log_renda_real, 1),
      log_bets_real_L1 = lag(log_bets_real, 1),
      bets_intensidade_idx_L1 = lag(bets_intensidade_idx, 1)
    )
}

base_ardl <- preparar_lags(base_comum)
base_ardl_full_sem <- preparar_lags(base_sem_bets_full)

# ============================================================================
# 10. FUNÇÕES ARDL
# ============================================================================

aicc_modelo <- function(modelo) {
  ll <- logLik(modelo)
  k <- attr(ll, "df")
  n <- nobs(modelo)
  aic <- AIC(modelo)
  if (n - k - 1 <= 0) return(Inf)
  as.numeric(aic + (2 * k * (k + 1)) / (n - k - 1))
}

hqic_modelo <- function(modelo) {
  ll <- logLik(modelo)
  k <- attr(ll, "df")
  n <- nobs(modelo)
  as.numeric(-2 * as.numeric(ll) + 2 * k * log(log(n)))
}

to_lm_safe <- function(modelo) {
  tryCatch(
    ARDL::to_lm(modelo, fix_names = TRUE, data_class = "ts"),
    error = function(e) modelo
  )
}

criar_ts <- function(df, vars, inicio) {
  z <- df %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric))
  
  ts(
    z,
    start = c(year(inicio), month(inicio)),
    frequency = 12
  )
}

extrair_q <- function(linha, x_vars) {
  as.integer(unlist(
    linha[1, paste0("q_", x_vars), drop = FALSE],
    use.names = FALSE
  ))
}

buscar_ardl <- function(
    ts_data,
    y_var,
    x_vars,
    inicio_amostra,
    fim_amostra,
    max_p = 4,
    max_q = 2,
    causal = TRUE,
    nome_modelo = "modelo"
) {
  message("\nIniciando busca: ", nome_modelo)
  
  max_lag_original <- if (causal) max(max_p, max_q + 1) else max(max_p, max_q)
  
  inicio_comum <- seq.Date(
    floor_date(inicio_amostra, "month"),
    by = "month",
    length.out = max_lag_original + 1
  )[max_lag_original + 1]
  
  start_ts <- c(year(inicio_comum), month(inicio_comum))
  end_ts <- c(year(fim_amostra), month(fim_amostra))
  
  n_comum <- length(seq.Date(inicio_comum, fim_amostra, by = "month"))
  
  grid_list <- c(
    list(p = 1:max_p),
    setNames(
      rep(list(0:max_q), length(x_vars)),
      paste0("q", seq_along(x_vars))
    )
  )
  
  grid <- do.call(
    expand.grid,
    c(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  )
  
  form <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
  
  resultados <- vector("list", nrow(grid))
  
  for (i in seq_len(nrow(grid))) {
    row <- grid[i, , drop = FALSE]
    p <- as.integer(row$p)
    q <- as.integer(unlist(
      row[1, paste0("q", seq_along(x_vars)), drop = FALSE],
      use.names = FALSE
    ))
    
    k_aprox <- 1 + p + sum(q + 1)
    
    if (n_comum / k_aprox < min_obs_por_coef) next
    
    mod <- tryCatch(
      ARDL::ardl(
        formula = form,
        data = ts_data,
        order = c(p, q),
        start = start_ts,
        end = end_ts
      ),
      error = function(e) NULL
    )
    
    if (is.null(mod)) next
    
    ll <- tryCatch(logLik(mod), error = function(e) NULL)
    if (is.null(ll)) next
    
    out <- tibble(
      p = p,
      AIC = AIC(mod),
      AICc = aicc_modelo(mod),
      BIC = BIC(mod),
      HQIC = hqic_modelo(mod),
      n_parametros = attr(ll, "df"),
      n_observacoes_efetivas = nobs(mod)
    )
    
    for (j in seq_along(x_vars)) {
      out[[paste0("q_", x_vars[j])]] <- q[j]
    }
    
    resultados[[i]] <- out
  }
  
  tab <- bind_rows(resultados)
  
  if (nrow(tab) == 0) {
    stop("Nenhum ARDL admissível em ", nome_modelo)
  }
  
  tab <- tab %>% arrange(BIC, AICc, HQIC, AIC)
  
  attr(tab, "formula") <- form
  attr(tab, "x_vars") <- x_vars
  attr(tab, "start_ts") <- start_ts
  attr(tab, "end_ts") <- end_ts
  attr(tab, "ts_data") <- ts_data
  attr(tab, "inicio_comum") <- inicio_comum
  attr(tab, "nome_modelo") <- nome_modelo
  
  message("Busca concluída. Modelos admissíveis: ", nrow(tab))
  tab
}

ajustar_linha <- function(busca, linha, start_override = NULL, end_override = NULL) {
  x_vars <- attr(busca, "x_vars")
  q <- extrair_q(linha, x_vars)
  
  ARDL::ardl(
    formula = attr(busca, "formula"),
    data = attr(busca, "ts_data"),
    order = c(as.integer(linha$p), q),
    start = if (is.null(start_override)) attr(busca, "start_ts") else start_override,
    end = if (is.null(end_override)) attr(busca, "end_ts") else end_override
  )
}

autocor_diagnostico <- function(modelo, p_ardl) {
  lm_m <- to_lm_safe(modelo)
  n <- nobs(lm_m)
  lag_use <- max(1, min(lag_diagnostico, floor(n / 5)))
  
  bg <- tryCatch(
    lmtest::bgtest(lm_m, order = lag_use, type = "Chisq"),
    error = function(e) NULL
  )
  
  lb <- tryCatch(
    Box.test(
      residuals(lm_m),
      lag = lag_use,
      type = "Ljung-Box",
      fitdf = min(p_ardl, lag_use - 1)
    ),
    error = function(e) NULL
  )
  
  list(
    bg_p = if (is.null(bg)) NA_real_ else as.numeric(bg$p.value),
    ljung_p = if (is.null(lb)) NA_real_ else as.numeric(lb$p.value)
  )
}

selecionar_modelo_final <- function(busca, max_candidatos = 100) {
  candidatos <- busca %>%
    arrange(BIC, AICc, HQIC, AIC) %>%
    slice_head(n = min(max_candidatos, nrow(busca)))
  
  for (i in seq_len(nrow(candidatos))) {
    linha <- candidatos[i, , drop = FALSE]
    mod <- tryCatch(ajustar_linha(busca, linha), error = function(e) NULL)
    if (is.null(mod)) next
    
    d <- autocor_diagnostico(mod, linha$p)
    
    if (
      !is.na(d$bg_p) && !is.na(d$ljung_p) &&
      d$bg_p >= 0.05 && d$ljung_p >= 0.05
    ) {
      return(list(
        linha = linha,
        modelo = mod,
        bg_p = d$bg_p,
        ljung_p = d$ljung_p,
        passou = TRUE,
        rank_bic = i
      ))
    }
  }
  
  warning(
    "Nenhum dos melhores candidatos passou BG e Ljung-Box simultaneamente. ",
    "Mantido o menor BIC como referência."
  )
  
  linha <- candidatos[1, , drop = FALSE]
  mod <- ajustar_linha(busca, linha)
  d <- autocor_diagnostico(mod, linha$p)
  
  list(
    linha = linha,
    modelo = mod,
    bg_p = d$bg_p,
    ljung_p = d$ljung_p,
    passou = FALSE,
    rank_bic = 1
  )
}

# ============================================================================
# 11. DIAGNÓSTICOS, HAC, VIF, WALD
# ============================================================================

diagnosticar_modelo <- function(modelo_ardl, p_ardl) {
  lm_m <- to_lm_safe(modelo_ardl)
  n <- nobs(lm_m)
  lag_use <- max(1, min(lag_diagnostico, floor(n / 5)))
  
  bg <- tryCatch(lmtest::bgtest(lm_m, order = lag_use, type = "Chisq"), error = function(e) NULL)
  lj <- tryCatch(
    Box.test(
      residuals(lm_m),
      lag = lag_use,
      type = "Ljung-Box",
      fitdf = min(p_ardl, lag_use - 1)
    ),
    error = function(e) NULL
  )
  bp <- tryCatch(lmtest::bptest(lm_m), error = function(e) NULL)
  jb <- tryCatch(tseries::jarque.bera.test(residuals(lm_m)), error = function(e) NULL)
  reset <- tryCatch(
    lmtest::resettest(lm_m, power = 2:3, type = "fitted"),
    error = function(e) NULL
  )
  
  tabela <- bind_rows(
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
        is.na(p_valor),
        "Teste indisponível",
        ifelse(p_valor < 0.05, "Rejeita H0 a 5%", "Não rejeita H0 a 5%")
      )
    )
  
  list(lm = lm_m, tabela = tabela)
}

coef_hac <- function(modelo_lm) {
  lag_hac <- max(1, min(12, floor(nobs(modelo_lm)^(1 / 4))))
  
  V <- sandwich::NeweyWest(
    modelo_lm,
    lag = lag_hac,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  tab <- lmtest::coeftest(modelo_lm, vcov. = V)
  
  tibble(
    termo = rownames(tab),
    coeficiente = tab[, 1],
    erro_padrao_HAC = tab[, 2],
    t_HAC = tab[, 3],
    p_valor_HAC = tab[, 4],
    significancia = case_when(
      tab[, 4] < 0.01 ~ "***",
      tab[, 4] < 0.05 ~ "**",
      tab[, 4] < 0.10 ~ "*",
      TRUE ~ ""
    )
  )
}

vif_manual <- function(modelo_lm, somente_exogenas = FALSE) {
  X <- model.matrix(modelo_lm)
  
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  
  if (somente_exogenas) {
    # Remove lags da dependente; evita confundir persistência AR com
    # multicolinearidade entre controles macro.
    manter <- !grepl("inad_pf_10sm", colnames(X), fixed = TRUE)
    X <- X[, manter, drop = FALSE]
  }
  
  if (ncol(X) < 2) {
    return(list(
      tabela = tibble(termo = colnames(X), VIF = NA_real_, tolerance = NA_real_),
      condition_number = NA_real_
    ))
  }
  
  sds <- apply(X, 2, sd, na.rm = TRUE)
  X <- X[, is.finite(sds) & sds > 0, drop = FALSE]
  
  vifs <- sapply(seq_len(ncol(X)), function(j) {
    y <- X[, j]
    z <- X[, -j, drop = FALSE]
    fit <- tryCatch(lm(y ~ z), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    r2 <- summary(fit)$r.squared
    if (!is.finite(r2)) return(NA_real_)
    if (r2 >= 1) return(Inf)
    1 / (1 - r2)
  })
  
  cn <- tryCatch(kappa(scale(X), exact = TRUE), error = function(e) NA_real_)
  
  list(
    tabela = tibble(
      termo = colnames(X),
      VIF = as.numeric(vifs),
      tolerance = ifelse(is.finite(vifs), 1 / vifs, 0)
    ),
    condition_number = cn
  )
}

efeito_acumulado <- function(modelo_lm, padrao) {
  b <- coef(modelo_lm)
  idx <- grep(padrao, names(b), fixed = TRUE)
  
  if (length(idx) == 0) {
    return(tibble(
      variavel = padrao,
      efeito_acumulado = NA_real_,
      erro_padrao = NA_real_,
      estatistica = NA_real_,
      p_valor = NA_real_
    ))
  }
  
  lag_hac <- max(1, min(12, floor(nobs(modelo_lm)^(1 / 4))))
  V <- sandwich::NeweyWest(
    modelo_lm, lag = lag_hac, prewhite = FALSE, adjust = TRUE
  )
  
  w <- rep(0, length(b))
  w[idx] <- 1
  
  est <- sum(b[idx])
  se <- sqrt(as.numeric(t(w) %*% V %*% w))
  tt <- est / se
  pv <- 2 * pt(abs(tt), df = df.residual(modelo_lm), lower.tail = FALSE)
  
  tibble(
    variavel = padrao,
    efeito_acumulado = est,
    erro_padrao = se,
    estatistica = tt,
    p_valor = pv
  )
}

wald_hac_conjunto <- function(modelo_lm, padrao) {
  b <- coef(modelo_lm)
  idx <- grep(padrao, names(b), fixed = TRUE)
  
  if (length(idx) == 0) {
    return(tibble(
      bloco = padrao,
      n_restricoes = 0,
      estatistica_Wald = NA_real_,
      F_aprox = NA_real_,
      p_valor = NA_real_
    ))
  }
  
  lag_hac <- max(1, min(12, floor(nobs(modelo_lm)^(1 / 4))))
  V <- sandwich::NeweyWest(
    modelo_lm, lag = lag_hac, prewhite = FALSE, adjust = TRUE
  )
  
  R <- matrix(0, nrow = length(idx), ncol = length(b))
  for (i in seq_along(idx)) R[i, idx[i]] <- 1
  
  Rb <- R %*% b
  RVRT <- R %*% V %*% t(R)
  
  W <- tryCatch(
    as.numeric(t(Rb) %*% solve(RVRT) %*% Rb),
    error = function(e) NA_real_
  )
  
  q <- length(idx)
  F_aprox <- W / q
  p <- if (is.finite(F_aprox)) {
    pf(F_aprox, df1 = q, df2 = df.residual(modelo_lm), lower.tail = FALSE)
  } else {
    NA_real_
  }
  
  tibble(
    bloco = padrao,
    n_restricoes = q,
    estatistica_Wald = W,
    F_aprox = F_aprox,
    p_valor = p
  )
}

# ============================================================================
# 12. ESTABILIDADE E QUEBRAS ESTRUTURAIS
# ============================================================================

estabilidade_modelo <- function(modelo_lm, datas_modelo = NULL, prefixo = "modelo") {
  # Reconstrói uma regressão "limpa" a partir da matriz do modelo.
  # Isso evita problemas com fórmulas contendo L(...) geradas pelo pacote ARDL.
  y <- as.numeric(model.response(model.frame(modelo_lm)))
  X <- model.matrix(modelo_lm)
  
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  
  dados_est <- as.data.frame(X, check.names = TRUE)
  dados_est$y_modelo <- y
  
  f <- as.formula(
    paste("y_modelo ~", paste(setdiff(names(dados_est), "y_modelo"), collapse = " + "))
  )
  
  cusum <- tryCatch(
    strucchange::efp(f, data = dados_est, type = "Rec-CUSUM"),
    error = function(e) NULL
  )
  
  mosum <- tryCatch(
    strucchange::efp(f, data = dados_est, type = "OLS-MOSUM"),
    error = function(e) NULL
  )
  
  teste_cusum <- if (!is.null(cusum)) tryCatch(sctest(cusum), error = function(e) NULL) else NULL
  teste_mosum <- if (!is.null(mosum)) tryCatch(sctest(mosum), error = function(e) NULL) else NULL
  
  bp <- tryCatch(
    strucchange::breakpoints(f, data = dados_est, h = 0.15),
    error = function(e) NULL
  )
  
  bp_otimo <- if (!is.null(bp)) {
    tryCatch(strucchange::breakpoints(bp), error = function(e) NULL)
  } else {
    NULL
  }
  
  indices_bp <- if (!is.null(bp_otimo)) bp_otimo$breakpoints else NA_integer_
  indices_bp <- indices_bp[is.finite(indices_bp)]
  
  datas_bp <- rep(as.Date(NA), length(indices_bp))
  if (!is.null(datas_modelo) && length(indices_bp) > 0) {
    indices_validos <- indices_bp[indices_bp <= length(datas_modelo)]
    datas_bp[seq_along(indices_validos)] <- datas_modelo[indices_validos]
  }
  
  tabela_testes <- tibble(
    teste = c("CUSUM", "MOSUM"),
    estatistica = c(
      if (is.null(teste_cusum)) NA_real_ else as.numeric(teste_cusum$statistic),
      if (is.null(teste_mosum)) NA_real_ else as.numeric(teste_mosum$statistic)
    ),
    p_valor = c(
      if (is.null(teste_cusum)) NA_real_ else as.numeric(teste_cusum$p.value),
      if (is.null(teste_mosum)) NA_real_ else as.numeric(teste_mosum$p.value)
    )
  )
  
  tabela_breaks <- if (length(indices_bp) > 0) {
    tibble(
      break_index = indices_bp,
      break_data = datas_bp
    )
  } else {
    tibble(
      break_index = NA_integer_,
      break_data = as.Date(NA)
    )
  }
  
  # Gráficos
  if (!is.null(cusum)) {
    png(file.path(dir_saida, paste0("CUSUM_", prefixo, ".png")), 1000, 650)
    plot(cusum, main = paste("CUSUM –", prefixo))
    dev.off()
  }
  
  if (!is.null(mosum)) {
    png(file.path(dir_saida, paste0("MOSUM_", prefixo, ".png")), 1000, 650)
    plot(mosum, main = paste("MOSUM –", prefixo))
    dev.off()
  }
  
  list(testes = tabela_testes, breaks = tabela_breaks, bp_obj = bp)
}

robustez_tendencia <- function(modelo_lm) {
  y <- as.numeric(model.response(model.frame(modelo_lm)))
  X <- model.matrix(modelo_lm)
  
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  
  dados_tend <- as.data.frame(X, check.names = TRUE)
  dados_tend$y_modelo <- y
  dados_tend$tendencia_linear <- seq_len(nrow(dados_tend))
  
  f <- as.formula(
    paste(
      "y_modelo ~",
      paste(setdiff(names(dados_tend), "y_modelo"), collapse = " + ")
    )
  )
  
  fit <- tryCatch(
    lm(f, data = dados_tend),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(tibble(
      modelo = "com tendência",
      n = NA_integer_,
      AIC = NA_real_,
      BIC = NA_real_,
      R2_ajustado = NA_real_,
      RESET_p = NA_real_
    ))
  }
  
  reset <- tryCatch(
    lmtest::resettest(fit, power = 2:3, type = "fitted"),
    error = function(e) NULL
  )
  
  tibble(
    modelo = "com tendência",
    n = nobs(fit),
    AIC = AIC(fit),
    BIC = BIC(fit),
    R2_ajustado = summary(fit)$adj.r.squared,
    RESET_p = if (is.null(reset)) NA_real_ else reset$p.value
  )
}

# ============================================================================
# 13. BOUNDS / ECM
# ============================================================================

rodar_bounds <- function(modelo_ardl) {
  if (!bounds_seguro) {
    return(list(
      conclusao = "NÃO INTERPRETADO – ordem de integração ainda não segura",
      bounds = NULL,
      recm = NULL,
      longo_prazo = NULL
    ))
  }
  
  b <- tryCatch({
    if (usar_bounds_exato) {
      ARDL::bounds_f_test(
        modelo_ardl,
        case = 3,
        alpha = 0.05,
        pvalue = TRUE,
        exact = TRUE,
        R = R_bounds_exato,
        test = "F"
      )
    } else {
      ARDL::bounds_f_test(
        modelo_ardl,
        case = 3,
        alpha = 0.05,
        pvalue = TRUE,
        exact = FALSE,
        test = "F"
      )
    }
  }, error = function(e) NULL)
  
  if (is.null(b)) {
    return(list(
      conclusao = "Bounds Test não disponível",
      bounds = NULL,
      recm = NULL,
      longo_prazo = NULL
    ))
  }
  
  f <- as.numeric(b$statistic[1])
  
  # A estrutura do pacote pode variar; tentamos usar os limites retornados.
  lim <- suppressWarnings(as.numeric(b$parameters))
  lim <- lim[is.finite(lim)]
  
  conclusao <- "INCONCLUSIVO"
  
  if (length(lim) >= 2) {
    I0 <- min(lim[1:2], na.rm = TRUE)
    I1 <- max(lim[1:2], na.rm = TRUE)
    
    conclusao <- if (f > I1) {
      "SIM – evidência de cointegração"
    } else if (f < I0) {
      "NÃO – sem evidência de cointegração"
    } else {
      "INCONCLUSIVO"
    }
  }
  
  recm_obj <- NULL
  lr_obj <- NULL
  
  if (grepl("^SIM", conclusao)) {
    recm_obj <- tryCatch(ARDL::recm(modelo_ardl, case = 3), error = function(e) NULL)
    lr_obj <- tryCatch(
      ARDL::multipliers(modelo_ardl, type = "lr", se = TRUE),
      error = function(e) NULL
    )
  }
  
  list(
    conclusao = conclusao,
    bounds = b,
    recm = recm_obj,
    longo_prazo = lr_obj
  )
}

# ============================================================================
# 14. MODELOS PRINCIPAIS
# ============================================================================

# PRINCIPAL: custo efetivo de crédito para PF, sem decompor em Selic + spread.
x_principal_sem <- c(
  "juros_livre_pf_aa_L1",
  "ipca_L1",
  "desemprego_L1",
  "log_renda_real_L1"
)

x_principal_com <- c(
  x_principal_sem,
  "log_bets_real_L1"
)

ts_principal_sem <- criar_ts(
  base_ardl,
  c("inad_pf_10sm", x_principal_sem),
  data_inicio
)

ts_principal_com <- criar_ts(
  base_ardl,
  c("inad_pf_10sm", x_principal_com),
  data_inicio
)

busca_principal_sem <- buscar_ardl(
  ts_principal_sem,
  "inad_pf_10sm",
  x_principal_sem,
  data_inicio,
  data_fim,
  max_p,
  max_q,
  TRUE,
  "PRINCIPAL SEM BETS – Juros Livre PF"
)

busca_principal_com <- buscar_ardl(
  ts_principal_com,
  "inad_pf_10sm",
  x_principal_com,
  data_inicio,
  data_fim,
  max_p,
  max_q,
  TRUE,
  "PRINCIPAL COM BETS – Juros Livre PF + log GGR real"
)

sel_principal_sem <- selecionar_modelo_final(busca_principal_sem)
sel_principal_com <- selecionar_modelo_final(busca_principal_com)

modelo_sem <- sel_principal_sem$modelo
modelo_com <- sel_principal_com$modelo
linha_sem <- sel_principal_sem$linha
linha_com <- sel_principal_com$linha

lm_sem <- to_lm_safe(modelo_sem)
lm_com <- to_lm_safe(modelo_com)

diag_sem <- diagnosticar_modelo(modelo_sem, linha_sem$p)
diag_com <- diagnosticar_modelo(modelo_com, linha_com$p)

coef_sem <- coef_hac(lm_sem)
coef_com <- coef_hac(lm_com)

vif_sem_total <- vif_manual(lm_sem, FALSE)
vif_com_total <- vif_manual(lm_com, FALSE)
vif_sem_exog <- vif_manual(lm_sem, TRUE)
vif_com_exog <- vif_manual(lm_com, TRUE)

wald_bets <- wald_hac_conjunto(lm_com, "log_bets_real_L1")
efeito_bets <- efeito_acumulado(lm_com, "log_bets_real_L1")

bounds_sem <- rodar_bounds(modelo_sem)
bounds_com <- rodar_bounds(modelo_com)

# ============================================================================
# 15. DATAS DOS MODELOS E ESTABILIDADE
# ============================================================================

criar_datas_modelo <- function(modelo_lm, busca) {
  inicio <- attr(busca, "inicio_comum")
  seq.Date(inicio, by = "month", length.out = nobs(modelo_lm))
}

datas_sem <- criar_datas_modelo(lm_sem, busca_principal_sem)
datas_com <- criar_datas_modelo(lm_com, busca_principal_com)

estab_sem <- estabilidade_modelo(lm_sem, datas_sem, "principal_sem_bets")
estab_com <- estabilidade_modelo(lm_com, datas_com, "principal_com_bets")

rob_tend_sem <- robustez_tendencia(lm_sem)
rob_tend_com <- robustez_tendencia(lm_com)

# ============================================================================
# 16. ROBUSTEZ 1 – SELIC ISOLADA
# ============================================================================

x_selic_sem <- c(
  "selic_L1",
  "ipca_L1",
  "desemprego_L1",
  "log_renda_real_L1"
)

x_selic_com <- c(
  x_selic_sem,
  "log_bets_real_L1"
)

ts_selic_sem <- criar_ts(base_ardl, c("inad_pf_10sm", x_selic_sem), data_inicio)
ts_selic_com <- criar_ts(base_ardl, c("inad_pf_10sm", x_selic_com), data_inicio)

busca_selic_sem <- buscar_ardl(
  ts_selic_sem, "inad_pf_10sm", x_selic_sem,
  data_inicio, data_fim, max_p, max_q, TRUE,
  "ROBUSTEZ SELIC – SEM BETS"
)

busca_selic_com <- buscar_ardl(
  ts_selic_com, "inad_pf_10sm", x_selic_com,
  data_inicio, data_fim, max_p, max_q, TRUE,
  "ROBUSTEZ SELIC – COM BETS"
)

sel_selic_sem <- selecionar_modelo_final(busca_selic_sem)
sel_selic_com <- selecionar_modelo_final(busca_selic_com)

# ============================================================================
# 17. ROBUSTEZ 2 – DECOMPOSIÇÃO SELIC + SPREAD
# ============================================================================

x_decomp_sem <- c(
  "selic_L1",
  "spread_livre_pf_L1",
  "ipca_L1",
  "desemprego_L1",
  "log_renda_real_L1"
)

x_decomp_com <- c(
  x_decomp_sem,
  "log_bets_real_L1"
)

ts_decomp_sem <- criar_ts(base_ardl, c("inad_pf_10sm", x_decomp_sem), data_inicio)
ts_decomp_com <- criar_ts(base_ardl, c("inad_pf_10sm", x_decomp_com), data_inicio)

busca_decomp_sem <- buscar_ardl(
  ts_decomp_sem, "inad_pf_10sm", x_decomp_sem,
  data_inicio, data_fim, max_p, max_q, TRUE,
  "ROBUSTEZ SELIC + SPREAD – SEM BETS"
)

busca_decomp_com <- buscar_ardl(
  ts_decomp_com, "inad_pf_10sm", x_decomp_com,
  data_inicio, data_fim, max_p, max_q, TRUE,
  "ROBUSTEZ SELIC + SPREAD – COM BETS"
)

sel_decomp_sem <- selecionar_modelo_final(busca_decomp_sem)
sel_decomp_com <- selecionar_modelo_final(busca_decomp_com)

# ============================================================================
# 18. ROBUSTEZ 3 – BETS/Renda (índice de intensidade)
# ============================================================================

x_intensidade <- c(
  "juros_livre_pf_aa_L1",
  "ipca_L1",
  "desemprego_L1",
  "log_renda_real_L1",
  "bets_intensidade_idx_L1"
)

ts_intensidade <- criar_ts(
  base_ardl,
  c("inad_pf_10sm", x_intensidade),
  data_inicio
)

busca_intensidade <- buscar_ardl(
  ts_intensidade,
  "inad_pf_10sm",
  x_intensidade,
  data_inicio,
  data_fim,
  max_p,
  max_q,
  TRUE,
  "ROBUSTEZ – Bets/Renda índice"
)

sel_intensidade <- selecionar_modelo_final(busca_intensidade)

# ============================================================================
# 19. REESTIMAÇÃO DA ESPECIFICAÇÃO SEM BETS NA AMOSTRA MÁXIMA
# ============================================================================

# Mantém a ORDEM escolhida na amostra comparável e apenas amplia a janela.
q_sem <- extrair_q(linha_sem, x_principal_sem)
p_sem <- as.integer(linha_sem$p)

inicio_full <- min(base_sem_bets_full$data)
fim_full <- max(base_sem_bets_full$data)

ts_full_sem <- criar_ts(
  base_ardl_full_sem,
  c("inad_pf_10sm", x_principal_sem),
  inicio_full
)

# Reestima com a maior amostra possível respeitando os lags efetivos.
max_lag_efetivo_sem <- max(p_sem, q_sem + 1)
inicio_full_efetivo <- seq.Date(
  inicio_full, by = "month", length.out = max_lag_efetivo_sem + 1
)[max_lag_efetivo_sem + 1]

modelo_sem_full <- ARDL::ardl(
  formula = as.formula(
    paste("inad_pf_10sm ~", paste(x_principal_sem, collapse = " + "))
  ),
  data = ts_full_sem,
  order = c(p_sem, q_sem),
  start = c(year(inicio_full_efetivo), month(inicio_full_efetivo)),
  end = c(year(fim_full), month(fim_full))
)

lm_sem_full <- to_lm_safe(modelo_sem_full)
diag_sem_full <- diagnosticar_modelo(modelo_sem_full, p_sem)
coef_sem_full <- coef_hac(lm_sem_full)

# ============================================================================
# 20. BENCHMARK AR(4) PURO
# ============================================================================

ajustar_ar_p <- function(df, p = 4) {
  z <- df %>% arrange(data)
  
  for (j in 1:p) {
    z[[paste0("inad_L", j)]] <- dplyr::lag(z$inad_pf_10sm, j)
  }
  
  termos <- paste0("inad_L", 1:p)
  
  z2 <- z %>%
    select(data, inad_pf_10sm, all_of(termos)) %>%
    drop_na()
  
  fit <- lm(
    as.formula(paste("inad_pf_10sm ~", paste(termos, collapse = " + "))),
    data = z2
  )
  
  list(modelo = fit, dados = z2)
}

benchmark_ar4 <- ajustar_ar_p(base_comum, 4)

metricas_lm <- function(nome, fit) {
  tibble(
    modelo = nome,
    n = nobs(fit),
    AIC = AIC(fit),
    BIC = BIC(fit),
    R2_ajustado = summary(fit)$adj.r.squared,
    RMSE_in_sample = sqrt(mean(residuals(fit)^2, na.rm = TRUE))
  )
}

# ============================================================================
# 21. MÉTRICAS COMPARÁVEIS
# ============================================================================

metricas_ardl <- function(nome, modelo_ardl, diag) {
  lm_m <- diag$lm
  
  tibble(
    modelo = nome,
    n = nobs(lm_m),
    AIC = AIC(modelo_ardl),
    AICc = aicc_modelo(modelo_ardl),
    BIC = BIC(modelo_ardl),
    HQIC = hqic_modelo(modelo_ardl),
    R2_ajustado = summary(lm_m)$adj.r.squared,
    RMSE_in_sample = sqrt(mean(residuals(lm_m)^2, na.rm = TRUE)),
    BG_p = diag$tabela$p_valor[diag$tabela$teste == "Breusch-Godfrey"],
    LjungBox_p = diag$tabela$p_valor[diag$tabela$teste == "Ljung-Box"],
    BP_p = diag$tabela$p_valor[diag$tabela$teste == "Breusch-Pagan"],
    RESET_p = diag$tabela$p_valor[diag$tabela$teste == "Ramsey RESET"],
    JB_p = diag$tabela$p_valor[diag$tabela$teste == "Jarque-Bera"]
  )
}

comparacao_principal <- bind_rows(
  metricas_ardl("Principal SEM Bets", modelo_sem, diag_sem),
  metricas_ardl("Principal COM Bets", modelo_com, diag_com)
)

metricas_robustez <- bind_rows(
  metricas_ardl(
    "Selic SEM Bets",
    sel_selic_sem$modelo,
    diagnosticar_modelo(sel_selic_sem$modelo, sel_selic_sem$linha$p)
  ),
  metricas_ardl(
    "Selic COM Bets",
    sel_selic_com$modelo,
    diagnosticar_modelo(sel_selic_com$modelo, sel_selic_com$linha$p)
  ),
  metricas_ardl(
    "Selic+Spread SEM Bets",
    sel_decomp_sem$modelo,
    diagnosticar_modelo(sel_decomp_sem$modelo, sel_decomp_sem$linha$p)
  ),
  metricas_ardl(
    "Selic+Spread COM Bets",
    sel_decomp_com$modelo,
    diagnosticar_modelo(sel_decomp_com$modelo, sel_decomp_com$linha$p)
  ),
  metricas_ardl(
    "Bets/Renda índice",
    sel_intensidade$modelo,
    diagnosticar_modelo(sel_intensidade$modelo, sel_intensidade$linha$p)
  )
)

# ============================================================================
# 22. VALIDAÇÃO ONE-STEP-AHEAD
# ============================================================================

# Constrói a matriz de regressão equivalente à especificação selecionada.
criar_design_selecionado <- function(df, y_var, x_vars, linha) {
  z <- df %>% arrange(data)
  p <- as.integer(linha$p)
  q <- extrair_q(linha, x_vars)
  
  termos <- character(0)
  
  for (j in 1:p) {
    nm <- paste0("Y_L", j)
    z[[nm]] <- dplyr::lag(z[[y_var]], j)
    termos <- c(termos, nm)
  }
  
  for (k in seq_along(x_vars)) {
    x <- x_vars[k]
    
    for (j in 0:q[k]) {
      nm <- paste0("X", k, "_L", j)
      z[[nm]] <- dplyr::lag(z[[x]], j)
      termos <- c(termos, nm)
    }
  }
  
  z %>%
    select(data, all_of(y_var), all_of(termos)) %>%
    drop_na()
}

rolling_one_step <- function(design, y_var, nome_modelo, n_holdout = 12) {
  if (nrow(design) <= n_holdout + 20) {
    return(tibble(
      modelo = nome_modelo,
      n_teste = NA_integer_,
      RMSE_OOS = NA_real_,
      MAE_OOS = NA_real_
    ))
  }
  
  inicio_teste <- nrow(design) - n_holdout + 1
  previsoes <- rep(NA_real_, n_holdout)
  observados <- design[[y_var]][inicio_teste:nrow(design)]
  
  termos <- setdiff(names(design), c("data", y_var))
  form <- as.formula(paste(y_var, "~", paste(termos, collapse = " + ")))
  
  for (h in seq_len(n_holdout)) {
    i <- inicio_teste + h - 1
    
    treino <- design[1:(i - 1), , drop = FALSE]
    teste <- design[i, , drop = FALSE]
    
    fit <- tryCatch(lm(form, data = treino), error = function(e) NULL)
    
    if (!is.null(fit)) {
      previsoes[h] <- tryCatch(
        as.numeric(predict(fit, newdata = teste)),
        error = function(e) NA_real_
      )
    }
  }
  
  ok <- is.finite(previsoes) & is.finite(observados)
  
  tibble(
    modelo = nome_modelo,
    n_teste = sum(ok),
    RMSE_OOS = sqrt(mean((observados[ok] - previsoes[ok])^2)),
    MAE_OOS = mean(abs(observados[ok] - previsoes[ok]))
  )
}

design_sem <- criar_design_selecionado(
  base_ardl,
  "inad_pf_10sm",
  x_principal_sem,
  linha_sem
)

design_com <- criar_design_selecionado(
  base_ardl,
  "inad_pf_10sm",
  x_principal_com,
  linha_com
)

# Benchmark AR(4) para a MESMA janela final.
design_ar4 <- benchmark_ar4$dados %>%
  rename(
    Y_L1 = inad_L1,
    Y_L2 = inad_L2,
    Y_L3 = inad_L3,
    Y_L4 = inad_L4
  )

validacao_oos <- bind_rows(
  rolling_one_step(
    design_sem, "inad_pf_10sm", "Principal SEM Bets", n_holdout
  ),
  rolling_one_step(
    design_com, "inad_pf_10sm", "Principal COM Bets", n_holdout
  ),
  rolling_one_step(
    design_ar4, "inad_pf_10sm", "Benchmark AR(4)", n_holdout
  )
)

# ============================================================================
# 23. OBSERVADO X AJUSTADO + ACF/PACF
# ============================================================================

criar_fit_df <- function(modelo_lm, datas) {
  tibble(
    data = datas,
    observado = as.numeric(model.response(model.frame(modelo_lm))),
    ajustado = as.numeric(fitted(modelo_lm)),
    residuo = as.numeric(residuals(modelo_lm))
  )
}

fit_sem <- criar_fit_df(lm_sem, datas_sem)
fit_com <- criar_fit_df(lm_com, datas_com)

plot_fit <- function(df, titulo, arquivo) {
  g <- df %>%
    select(data, observado, ajustado) %>%
    pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
    ggplot(aes(data, valor, linetype = serie)) +
    geom_line(linewidth = 0.8) +
    labs(title = titulo, x = NULL, y = NULL, linetype = NULL) +
    theme_minimal(base_size = 12)
  
  ggsave(
    file.path(dir_saida, arquivo),
    g,
    width = 8,
    height = 4.5,
    dpi = 150
  )
}

plot_fit(
  fit_sem,
  "Observado × Ajustado – Principal sem Bets",
  "observado_ajustado_principal_sem_bets.png"
)

plot_fit(
  fit_com,
  "Observado × Ajustado – Principal com Bets",
  "observado_ajustado_principal_com_bets.png"
)

salvar_acf_pacf <- function(res, prefixo) {
  png(file.path(dir_saida, paste0("acf_", prefixo, ".png")), 1000, 650)
  acf(res, main = paste("ACF –", prefixo))
  dev.off()
  
  png(file.path(dir_saida, paste0("pacf_", prefixo, ".png")), 1000, 650)
  pacf(res, main = paste("PACF –", prefixo))
  dev.off()
}

salvar_acf_pacf(residuals(lm_sem), "principal_sem_bets")
salvar_acf_pacf(residuals(lm_com), "principal_com_bets")

# ============================================================================
# 24. FORMATAÇÃO DE ORDENS/LAGS
# ============================================================================

fmt_ordem <- function(linha, x_vars) {
  q <- extrair_q(linha, x_vars)
  paste0(
    "ARDL(",
    paste(c(as.integer(linha$p), q), collapse = ","),
    ")"
  )
}

mostrar_lags <- function(linha, x_vars, nomes) {
  q <- extrair_q(linha, x_vars)
  
  tibble(
    variavel = nomes,
    q_ARDL_sobre_X_L1 = q,
    primeiro_lag_original = 1,
    ultimo_lag_original = q + 1
  )
}

lags_sem <- mostrar_lags(
  linha_sem,
  x_principal_sem,
  c("Juros Livre PF", "IPCA", "Desemprego", "Log Renda Real")
)

lags_com <- mostrar_lags(
  linha_com,
  x_principal_com,
  c("Juros Livre PF", "IPCA", "Desemprego", "Log Renda Real", "Log Bets Real")
)

# ============================================================================
# 25. EXPORTAÇÃO BOUNDS/ECM
# ============================================================================

exportar_bounds <- function(obj) {
  if (!is.null(obj$bounds)) {
    tryCatch(as.data.frame(obj$bounds$tab), error = function(e) {
      data.frame(resultado = obj$conclusao)
    })
  } else {
    data.frame(resultado = obj$conclusao)
  }
}

exportar_lr <- function(obj) {
  if (!is.null(obj$longo_prazo)) {
    as.data.frame(obj$longo_prazo)
  } else {
    data.frame(resultado = "Não calculado")
  }
}

exportar_ecm <- function(obj) {
  if (!is.null(obj$recm)) {
    as.data.frame(summary(obj$recm)$coefficients)
  } else {
    data.frame(resultado = "Não calculado")
  }
}

# ============================================================================
# 26. EXPORTAÇÃO DOS RESULTADOS
# ============================================================================

abas <- list(
  base_comum = base_comum,
  base_sem_bets_full = base_sem_bets_full,
  
  descritivas = descritivas,
  correlacoes = as.data.frame(mat_cor),
  correlacao_tendencia = cor_tendencia,
  
  estacionariedade = tab_estacionariedade,
  
  ranking_principal_sem = head(busca_principal_sem, 250),
  ranking_principal_com = head(busca_principal_com, 250),
  
  lags_principal_sem = lags_sem,
  lags_principal_com = lags_com,
  
  coef_principal_sem_HAC = coef_sem,
  coef_principal_com_HAC = coef_com,
  coef_sem_full_HAC = coef_sem_full,
  
  diag_principal_sem = diag_sem$tabela,
  diag_principal_com = diag_com$tabela,
  diag_sem_full = diag_sem_full$tabela,
  
  VIF_sem_total = vif_sem_total$tabela,
  VIF_com_total = vif_com_total$tabela,
  VIF_sem_exogenas = vif_sem_exog$tabela,
  VIF_com_exogenas = vif_com_exog$tabela,
  
  Wald_Bets_HAC = wald_bets,
  Efeito_Bets = efeito_bets,
  
  estabilidade_sem = estab_sem$testes,
  estabilidade_com = estab_com$testes,
  breaks_sem = estab_sem$breaks,
  breaks_com = estab_com$breaks,
  
  robustez_tendencia_sem = rob_tend_sem,
  robustez_tendencia_com = rob_tend_com,
  
  comparacao_principal = comparacao_principal,
  metricas_robustez = metricas_robustez,
  validacao_OOS = validacao_oos,
  
  benchmark_AR4 = metricas_lm("Benchmark AR(4)", benchmark_ar4$modelo),
  
  bounds_sem = exportar_bounds(bounds_sem),
  bounds_com = exportar_bounds(bounds_com),
  longo_prazo_sem = exportar_lr(bounds_sem),
  longo_prazo_com = exportar_lr(bounds_com),
  ECM_sem = exportar_ecm(bounds_sem),
  ECM_com = exportar_ecm(bounds_com),
  
  ranking_rob_selic_sem = head(busca_selic_sem, 100),
  ranking_rob_selic_com = head(busca_selic_com, 100),
  ranking_rob_decomp_sem = head(busca_decomp_sem, 100),
  ranking_rob_decomp_com = head(busca_decomp_com, 100),
  ranking_rob_bets_renda = head(busca_intensidade, 100)
)

openxlsx::write.xlsx(
  abas,
  file = file.path(dir_saida, "resultados_SFN_ARDL_revisado.xlsx"),
  overwrite = TRUE
)

readr::write_csv(
  busca_principal_sem,
  file.path(dir_saida, "ranking_ARDL_principal_sem_bets.csv")
)

readr::write_csv(
  busca_principal_com,
  file.path(dir_saida, "ranking_ARDL_principal_com_bets.csv")
)

readr::write_csv(
  validacao_oos,
  file.path(dir_saida, "validacao_out_of_sample.csv")
)

# ============================================================================
# 27. SÍNTESE FINAL
# ============================================================================

cat("\n\n==================================================================\n")
cat("SFN – ARDL REVISADO – INADIMPLÊNCIA PF ATÉ 10 SM\n")
cat("==================================================================\n\n")

cat("AMOSTRA COMPARÁVEL COM/SEM BETS:\n")
cat(
  format(data_inicio, "%YM%m"), "–", format(data_fim, "%YM%m"),
  " | n = ", nrow(base_comum), "\n\n", sep = ""
)

cat("MODELO PRINCIPAL SEM BETS:\n")
cat(fmt_ordem(linha_sem, x_principal_sem), "\n")
print(comparacao_principal %>% filter(modelo == "Principal SEM Bets"))
cat("\n")

cat("MODELO PRINCIPAL COM BETS:\n")
cat(fmt_ordem(linha_com, x_principal_com), "\n")
print(comparacao_principal %>% filter(modelo == "Principal COM Bets"))
cat("\n")

cat("TESTE WALD-HAC CONJUNTO – BETS:\n")
print(wald_bets)
cat("\n")

cat("EFEITO ACUMULADO – BETS:\n")
print(efeito_bets)
cat("\n")

cat("VIF DAS REGRESSORAS EXÓGENAS – SEM BETS:\n")
print(vif_sem_exog$tabela, n = Inf)
cat("Condition Number exógenas:", round(vif_sem_exog$condition_number, 3), "\n\n")

cat("VIF DAS REGRESSORAS EXÓGENAS – COM BETS:\n")
print(vif_com_exog$tabela, n = Inf)
cat("Condition Number exógenas:", round(vif_com_exog$condition_number, 3), "\n\n")

cat("ESTACIONARIEDADE:\n")
print(tab_estacionariedade %>% select(variavel, integracao), n = Inf)
cat("\nBounds seguro? ", ifelse(bounds_seguro, "SIM", "NÃO"), "\n", sep = "")
cat("Bounds sem Bets: ", bounds_sem$conclusao, "\n", sep = "")
cat("Bounds com Bets: ", bounds_com$conclusao, "\n\n", sep = "")

cat("ESTABILIDADE – SEM BETS:\n")
print(estab_sem$testes)
print(estab_sem$breaks)
cat("\n")

cat("ESTABILIDADE – COM BETS:\n")
print(estab_com$testes)
print(estab_com$breaks)
cat("\n")

cat("ROBUSTEZ COM TENDÊNCIA:\n")
print(bind_rows(
  rob_tend_sem %>% mutate(especificacao = "SEM BETS"),
  rob_tend_com %>% mutate(especificacao = "COM BETS")
))
cat("\n")

cat("VALIDAÇÃO FORA DA AMOSTRA (one-step-ahead):\n")
print(validacao_oos)
cat("\n")

cat("ESPECIFICAÇÃO SEM BETS REESTIMADA NA AMOSTRA MÁXIMA:\n")
cat(
  format(inicio_full, "%YM%m"), "–", format(fim_full, "%YM%m"),
  " | n efetivo = ", nobs(lm_sem_full), "\n", sep = ""
)
print(coef_sem_full, n = Inf)
cat("\n")

cat("ROBUSTEZ: SELIC ISOLADA / SELIC+SPREAD / BETS-RENDA:\n")
print(metricas_robustez)
cat("\n")

cat("Arquivo principal exportado em:\n")
cat(file.path(dir_saida, "resultados_SFN_ARDL_revisado.xlsx"), "\n\n")

cat("FIM.\n")
