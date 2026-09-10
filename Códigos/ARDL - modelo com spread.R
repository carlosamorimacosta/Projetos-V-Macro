# ============================================================================
# SFN – INADIMPLÊNCIA PF ATÉ 10 SALÁRIOS MÍNIMOS
# Jan/2020 a Jul/2026
#
# ESTRUTURA DE ENTRADA: UM ARQUIVO POR VARIÁVEL
#
# arquivo 1: inad_pf_10sm
# arquivo 2: inad_pf_geral
# arquivo 3: selic
# arquivo 4: spread_livre_pf
# arquivo 5: ipca
# arquivo 6: desemprego
# arquivo 7: renda
# arquivo 8: bets
#
# CADA ARQUIVO DEVE TER:
#   - uma coluna de data
#   - uma coluna com a respectiva variável
#
# O script aceita CSV e XLSX.
#
# MODELO 1 – SEM BETS
#   inad_pf_10sm_t =
#     f(lags da própria inadimplência,
#       Selic, SpreadLivrePF, IPCA, desemprego, renda)
#
# MODELO 2 – COM BETS
#   inad_pf_10sm_t =
#     f(lags da própria inadimplência,
#       Selic, SpreadLivrePF, IPCA, desemprego, renda, bets)
#
# A inadimplência PF geral é importada, preservada e usada como benchmark.
# Por padrão NÃO entra nos modelos principais, para evitar relação mecânica
# com a inadimplência do subgrupo <=10SM.
#
# As regressoras do modelo causal principal são deslocadas em 1 mês:
#   q = 0 => X_{t-1}
#   q = 1 => X_{t-1}, X_{t-2}
#   ...
#
# Critérios de seleção:
#   1. BIC
#   2. AICc
#   3. HQIC
#   4. AIC
#
# O menor BIC não é aceito automaticamente: o modelo também deve apresentar
# diagnóstico residual adequado, especialmente ausência de autocorrelação.
# ============================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(2026)

# ============================================================================
# 0. CONFIGURAÇÕES
# ============================================================================

data_inicio <- as.Date("2020-01-01")
data_fim    <- as.Date("2026-07-31")

# ============================================================================
# >>> EDITE OS CAMINHOS DOS OITO ARQUIVOS AQUI <<<
# ============================================================================

arquivo_inad_10sm <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplência 10sm/Base_Final_Inadimplencia_PF.xlsx"
arquivo_inad_geral <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplencia de crédito - pesssoa física - SGS.csv"
arquivo_selic <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Selic Meta.csv"
arquivo_spread <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Spread_Mensal_Credito_Livre_PF.csv"
arquivo_ipca <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/ipca_202606SerieHist.xls"
arquivo_desemprego <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Desemprego Pnad.csv"
arquivo_renda <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Renda/PNAD Contínua - renda média - geral.xlsx"
arquivo_bets <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Bets/GGR Bets Brasil.xlsx"

# Para arquivos XLSX, indique a aba (nome ou número).
# Para CSV, este objeto é ignorado.
aba_inad_10sm <- 1
aba_inad_geral <- 1
aba_selic <- 1
aba_spread <- 1
aba_ipca <- 1
aba_desemprego <- 1
aba_renda <- 1
aba_bets <- 1

# Por padrão a inadimplência geral é apenas benchmark.
incluir_inad_geral_no_modelo <- FALSE

# Defasagens máximas
max_p <- 6
max_q <- 6

# Parcimônia
min_obs_por_coef <- 4

# Diagnóstico mensal
lag_diagnostico <- 12

# Robustez contemporânea
rodar_robustez_contemporanea <- TRUE

# Bounds
usar_bounds_exato <- FALSE
R_bounds_exato <- 40000

# Saídas
dir_saida <- "output_SFN_modelos_com_sem_bets"
if (!dir.exists(dir_saida)) {
  dir.create(dir_saida, recursive = TRUE)
}

# ============================================================================
# 1. PACOTES
# ============================================================================

pacotes <- c(
  "tidyverse",
  "lubridate",
  "zoo",
  "ARDL",
  "lmtest",
  "sandwich",
  "tseries",
  "urca",
  "car",
  "strucchange",
  "ggplot2",
  "openxlsx"
)

instalar_ausentes <- function(pkgs) {
  ausentes <- pkgs[
    !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(ausentes) > 0) {
    message(
      "Instalando pacotes ausentes: ",
      paste(ausentes, collapse = ", ")
    )
    install.packages(ausentes, dependencies = TRUE)
  }
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
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  s <- trimws(as.character(x))
  
  s[
    s %in% c("", "NA", "NaN", "-", "--", "...", "null", "NULL")
  ] <- NA_character_
  
  usa_virgula <- mean(
    grepl(",", s),
    na.rm = TRUE
  ) > 0.20
  
  if (is.na(usa_virgula)) {
    usa_virgula <- FALSE
  }
  
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
    return(
      floor_date(x, "month")
    )
  }
  
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(
      floor_date(as.Date(x), "month")
    )
  }
  
  if (is.numeric(x)) {
    
    xx <- as.numeric(x)
    
    # Serial do Excel
    med <- suppressWarnings(
      median(xx, na.rm = TRUE)
    )
    
    if (
      is.finite(med) &&
      med > 20000 &&
      med < 80000
    ) {
      
      return(
        floor_date(
          as.Date(
            xx,
            origin = "1899-12-30"
          ),
          "month"
        )
      )
    }
    
    # YYYYMM
    if (
      all(
        is.na(xx) |
        (xx >= 190001 & xx <= 210012)
      )
    ) {
      
      s <- sprintf(
        "%06d",
        as.integer(xx)
      )
      
      return(
        as.Date(
          paste0(
            substr(s, 1, 4),
            "-",
            substr(s, 5, 6),
            "-01"
          )
        )
      )
    }
  }
  
  s <- trimws(
    as.character(x)
  )
  
  s[
    s %in% c("", "NA", "NaN")
  ] <- NA_character_
  
  out <- rep(
    as.Date(NA),
    length(s)
  )
  
  # YYYY-MM
  idx1 <- grepl(
    "^\\d{4}[-/]\\d{1,2}$",
    s
  )
  
  if (any(idx1, na.rm = TRUE)) {
    
    ss <- gsub(
      "/",
      "-",
      s[idx1]
    )
    
    out[idx1] <- as.Date(
      paste0(
        ss,
        "-01"
      )
    )
  }
  
  # MM/YYYY
  idx2 <- is.na(out) &
    grepl(
      "^\\d{1,2}[-/]\\d{4}$",
      s
    )
  
  if (any(idx2, na.rm = TRUE)) {
    
    ss <- gsub(
      "-",
      "/",
      s[idx2]
    )
    
    p <- strsplit(
      ss,
      "/",
      fixed = TRUE
    )
    
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
  
  # YYYYMM
  idx3 <- is.na(out) &
    grepl(
      "^\\d{6}$",
      s
    )
  
  if (any(idx3, na.rm = TRUE)) {
    
    ss <- s[idx3]
    
    out[idx3] <- as.Date(
      paste0(
        substr(ss, 1, 4),
        "-",
        substr(ss, 5, 6),
        "-01"
      )
    )
  }
  
  # Datas completas
  idx4 <- is.na(out) &
    !is.na(s)
  
  if (any(idx4)) {
    
    d <- suppressWarnings(
      lubridate::parse_date_time(
        s[idx4],
        orders = c(
          "Ymd",
          "Y-m-d",
          "Y/m/d",
          "dmy",
          "d/m/Y",
          "d-m-Y",
          "mdy",
          "m/d/Y",
          "m-d-Y"
        ),
        quiet = TRUE
      )
    )
    
    out[idx4] <- as.Date(d)
  }
  
  floor_date(
    out,
    "month"
  )
}

ler_arquivo_generico <- function(
    caminho,
    aba = 1
) {
  
  if (!file.exists(caminho)) {
    stop(
      "\nArquivo não encontrado:\n",
      caminho,
      "\n\nCorrija o caminho no bloco CONFIGURAÇÕES."
    )
  }
  
  ext <- tolower(
    tools::file_ext(caminho)
  )
  
  if (ext %in% c("xlsx", "xlsm")) {
    
    df <- openxlsx::read.xlsx(
      caminho,
      sheet = aba,
      detectDates = TRUE
    )
    
  } else if (ext == "xls") {
    
    stop(
      "\nArquivo .xls detectado: ",
      caminho,
      "\nConverta para .xlsx ou .csv."
    )
    
  } else {
    
    primeira <- readLines(
      caminho,
      n = 1,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    contar <- function(txt) {
      lengths(
        regmatches(
          primeira,
          gregexpr(
            txt,
            primeira,
            fixed = TRUE
          )
        )
      )
    }
    
    n_pv <- contar(";")
    n_vg <- contar(",")
    n_tab <- contar("\t")
    
    delim <- if (
      n_tab >= max(n_pv, n_vg)
    ) {
      "\t"
    } else if (
      n_pv > n_vg
    ) {
      ";"
    } else {
      ","
    }
    
    df <- readr::read_delim(
      caminho,
      delim = delim,
      col_types = readr::cols(.default = "c"),
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  
  names(df) <- normalizar_nome(
    names(df)
  )
  
  as.data.frame(df)
}

encontrar_coluna <- function(
    df,
    alternativas,
    nome_logico
) {
  
  achou <- intersect(
    alternativas,
    names(df)
  )
  
  if (length(achou) == 0) {
    
    stop(
      "\nNão encontrei a coluna de ",
      nome_logico,
      ".\nNomes aceitos: ",
      paste(
        alternativas,
        collapse = ", "
      ),
      "\nColunas encontradas: ",
      paste(
        names(df),
        collapse = ", "
      )
    )
  }
  
  achou[1]
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
      "data",
      "date",
      "mes",
      "mes_ano",
      "competencia",
      "periodo"
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
        data_inicio,
        "month"
      ),
      data <= floor_date(
        data_fim,
        "month"
      )
    ) %>%
    arrange(data)
  
  if (
    anyDuplicated(
      out$data
    )
  ) {
    
    dup <- out %>%
      count(data) %>%
      filter(n > 1)
    
    print(dup)
    
    stop(
      "\nO arquivo ",
      basename(caminho),
      " possui mais de uma observação no mesmo mês."
    )
  }
  
  names(out)[2] <- nome_final
  
  out
}

# ============================================================================
# 3. IMPORTAÇÃO DOS OITO ARQUIVOS
# ============================================================================

message("\n============================================================")
message("1. IMPORTANDO AS OITO SÉRIES")
message("============================================================")

# ============================================================================
# 3. IMPORTAÇÃO DOS OITO ARQUIVOS
# ============================================================================

message("\n============================================================")
message("1. IMPORTANDO AS OITO SÉRIES")
message("============================================================")

inad_10sm <- ler_serie_unica(
  arquivo_inad_10sm,
  aba_inad_10sm,
  "inad_pf_10sm",
  c(
    "inad_pf_10sm",
    "inadimplencia_pf_10sm",
    "inadimplencia_pf_ate_10sm",
    "inadimplencia_pf_ate10sm",
    "inad_pf_ate10sm",
    "inadimplencia_ate_10_sm",
    "inadimplencia_ate10sm",
    "inadimplencia_pf_ate10sm_pct",
    "inadimplencia_pf_ate_10sm_pct",
    "valor"
  )
)

inad_geral <- ler_serie_unica(
  arquivo_inad_geral,
  aba_inad_geral,
  "inad_pf_geral",
  c(
    "inad_pf_geral",
    "inadimplencia_pf_geral",
    "inadimplencia_geral",
    "inad_pf_total",
    "inadimplencia_pf_total",
    "inadimplencia",
    "valor"
  )
)

selic_df <- ler_serie_unica(
  arquivo_selic,
  aba_selic,
  "selic",
  c(
    "selic",
    "selic_meta",
    "taxa_selic",
    "meta_selic",
    "valor"
  )
)

spread_df <- ler_serie_unica(
  arquivo_spread,
  aba_spread,
  "spread_livre_pf",
  c(
    "spread_livre_pf",
    "spreadlivrepf",
    "spread_pf_livre",
    "spread_credito_livre_pf",
    "spread_pf",
    "valor"
  )
)

ipca_df <- ler_serie_unica(
  arquivo_ipca,
  aba_ipca,
  "ipca",
  c(
    "ipca",
    "inflacao",
    "inflacao_ipca",
    "valor"
  )
)

desemprego_df <- ler_serie_unica(
  arquivo_desemprego,
  aba_desemprego,
  "desemprego",
  c(
    "desemprego",
    "taxa_desemprego",
    "desocupacao",
    "taxa_desocupacao",
    "valor"
  )
)

renda_df <- ler_serie_unica(
  arquivo_renda,
  aba_renda,
  "renda",
  c(
    "renda",
    "renda_real",
    "rendimento_real",
    "rendimento_medio_real",
    "massa_renda_real",
    "valor"
  )
)

bets_df <- ler_serie_unica(
  arquivo_bets,
  aba_bets,
  "bets",
  c(
    "bets",
    "bet",
    "apostas",
    "apostas_esportivas",
    "ggr",
    "ggr_bets",
    "indice_bets",
    "crescimento_bets",
    "valor"
  )
)

# ============================================================================
# 4. CONSTRUÇÃO DA BASE FINAL
# ============================================================================

message("\n============================================================")
message("2. CONSTRUINDO BASE FINAL")
message("============================================================")

calendario <- tibble(
  data = seq.Date(
    floor_date(
      data_inicio,
      "month"
    ),
    floor_date(
      data_fim,
      "month"
    ),
    by = "month"
  )
)

base_final <- calendario %>%
  left_join(
    inad_10sm,
    by = "data"
  ) %>%
  left_join(
    inad_geral,
    by = "data"
  ) %>%
  left_join(
    selic_df,
    by = "data"
  ) %>%
  left_join(
    spread_df,
    by = "data"
  ) %>%
  left_join(
    ipca_df,
    by = "data"
  ) %>%
  left_join(
    desemprego_df,
    by = "data"
  ) %>%
  left_join(
    renda_df,
    by = "data"
  ) %>%
  left_join(
    bets_df,
    by = "data"
  ) %>%
  arrange(data)

vars_essenciais <- c(
  "inad_pf_10sm",
  "inad_pf_geral",
  "selic",
  "spread_livre_pf",
  "ipca",
  "desemprego",
  "renda",
  "bets"
)

# Gap apenas para diagnóstico
base_final <- base_final %>%
  mutate(
    gap_inad_10sm_geral =
      inad_pf_10sm -
      inad_pf_geral
  )

# Verificação de NAs
faltantes <- base_final %>%
  filter(
    if_any(
      all_of(vars_essenciais),
      is.na
    )
  )

if (
  nrow(faltantes) > 0
) {
  
  message(
    "\nForam encontrados meses com dados ausentes:"
  )
  
  print(
    faltantes %>%
      select(
        data,
        all_of(vars_essenciais)
      )
  )
  
  stop(
    "\nHá NAs nas séries entre 2020M01 e 2026M07.\n",
    "O script não interpola, não preenche com zero e não usa informação futura."
  )
}

message("\nPrimeiras observações:")
print(
  head(base_final)
)

message("\nÚltimas observações:")
print(
  tail(base_final)
)

message("\nResumo:")
print(
  summary(base_final)
)

# Exportação
readr::write_csv(
  base_final,
  file.path(
    dir_saida,
    "base_final_SFN_2020M01_2026M07.csv"
  )
)

openxlsx::write.xlsx(
  base_final,
  file.path(
    dir_saida,
    "base_final_SFN_2020M01_2026M07.xlsx"
  ),
  overwrite = TRUE
)

# ============================================================================
# 5. ESTATÍSTICAS DESCRITIVAS E CORRELAÇÕES
# ============================================================================

message("\n============================================================")
message("3. ESTATÍSTICA DESCRITIVA")
message("============================================================")

vars_numericas <- setdiff(
  names(base_final),
  "data"
)

descritivas <- map_dfr(
  vars_numericas,
  function(v) {
    
    x <- base_final[[v]]
    
    tibble(
      variavel = v,
      media = mean(
        x,
        na.rm = TRUE
      ),
      mediana = median(
        x,
        na.rm = TRUE
      ),
      minimo = min(
        x,
        na.rm = TRUE
      ),
      maximo = max(
        x,
        na.rm = TRUE
      ),
      variancia = var(
        x,
        na.rm = TRUE
      ),
      desvio_padrao = sd(
        x,
        na.rm = TRUE
      )
    )
  }
)

print(
  descritivas
)

mat_cor <- cor(
  base_final %>%
    select(
      all_of(
        vars_numericas
      )
    ),
  use = "complete.obs"
)

message("\nMatriz de correlação:")
print(
  round(
    mat_cor,
    4
  )
)

cor_inad <- cor(
  base_final$inad_pf_10sm,
  base_final$inad_pf_geral
)

cor_selic_spread <- cor(
  base_final$selic,
  base_final$spread_livre_pf
)

message(
  sprintf(
    "\nCorrelação Inad <=10SM × Inad PF geral: %.4f",
    cor_inad
  )
)

message(
  sprintf(
    "Correlação Selic × Spread Livre PF: %.4f",
    cor_selic_spread
  )
)

# ============================================================================
# 6. GRÁFICOS DAS SÉRIES
# ============================================================================

for (
  v in vars_numericas
) {
  
  g <- ggplot(
    base_final,
    aes(
      x = data,
      y = .data[[v]]
    )
  ) +
    geom_line(
      linewidth = 0.75
    ) +
    labs(
      title = v,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(
      base_size = 12
    )
  
  ggsave(
    filename = file.path(
      dir_saida,
      paste0(
        "serie_",
        v,
        ".png"
      )
    ),
    plot = g,
    width = 8,
    height = 4.5,
    dpi = 150
  )
}

# ============================================================================
# 7. TESTES DE ESTACIONARIEDADE
# ============================================================================

message("\n============================================================")
message("4. TESTES DE ESTACIONARIEDADE")
message("============================================================")

teste_estacionariedade <- function(
    x,
    nome
) {
  
  x <- na.omit(
    as.numeric(x)
  )
  
  testes <- function(z) {
    
    z <- na.omit(z)
    
    list(
      adf = tryCatch(
        suppressWarnings(
          tseries::adf.test(
            z,
            alternative = "stationary"
          )$p.value
        ),
        error = function(e) NA_real_
      ),
      
      pp = tryCatch(
        suppressWarnings(
          tseries::pp.test(
            z,
            alternative = "stationary"
          )$p.value
        ),
        error = function(e) NA_real_
      ),
      
      kpss = tryCatch(
        suppressWarnings(
          tseries::kpss.test(
            z,
            null = "Level"
          )$p.value
        ),
        error = function(e) NA_real_
      )
    )
  }
  
  nivel <- testes(x)
  d1 <- testes(
    diff(x)
  )
  
  estacionaria <- function(tt) {
    
    sinais <- c(
      !is.na(tt$adf) &&
        tt$adf < 0.05,
      
      !is.na(tt$pp) &&
        tt$pp < 0.05,
      
      !is.na(tt$kpss) &&
        tt$kpss > 0.05
    )
    
    sum(sinais) >= 2
  }
  
  integracao <- if (
    estacionaria(nivel)
  ) {
    
    "I(0)"
    
  } else if (
    estacionaria(d1)
  ) {
    
    "I(1)"
    
  } else {
    
    "Possível I(2) / inconclusivo"
  }
  
  tibble(
    variavel = nome,
    adf_nivel = nivel$adf,
    pp_nivel = nivel$pp,
    kpss_nivel = nivel$kpss,
    adf_diff1 = d1$adf,
    pp_diff1 = d1$pp,
    kpss_diff1 = d1$kpss,
    integracao = integracao
  )
}

tab_estacionariedade <- map_dfr(
  vars_essenciais,
  ~ teste_estacionariedade(
    base_final[[.x]],
    .x
  )
)

print(
  tab_estacionariedade
)

ha_I2 <- any(
  grepl(
    "I\\(2\\)",
    tab_estacionariedade$integracao
  )
)

if (ha_I2) {
  
  warning(
    "Pelo menos uma série é possivelmente I(2) ou inconclusiva. ",
    "O Bounds Test não deve ser interpretado até resolver a ordem de integração."
  )
}

# ============================================================================
# 8. PREPARAÇÃO DAS DEFASAGENS
# ============================================================================

message("\n============================================================")
message("5. PREPARANDO DEFASAGENS")
message("============================================================")

base_ardl <- base_final %>%
  mutate(
    selic_L1 = lag(
      selic,
      1
    ),
    spread_livre_pf_L1 = lag(
      spread_livre_pf,
      1
    ),
    ipca_L1 = lag(
      ipca,
      1
    ),
    desemprego_L1 = lag(
      desemprego,
      1
    ),
    renda_L1 = lag(
      renda,
      1
    ),
    bets_L1 = lag(
      bets,
      1
    ),
    inad_pf_geral_L1 = lag(
      inad_pf_geral,
      1
    )
  )

criar_ts <- function(
    df,
    vars
) {
  
  z <- df %>%
    select(
      all_of(vars)
    ) %>%
    mutate(
      across(
        everything(),
        as.numeric
      )
    )
  
  ts(
    z,
    start = c(
      year(data_inicio),
      month(data_inicio)
    ),
    frequency = 12
  )
}

aicc_modelo <- function(
    modelo
) {
  
  ll <- logLik(
    modelo
  )
  
  k <- attr(
    ll,
    "df"
  )
  
  n <- nobs(
    modelo
  )
  
  aic <- AIC(
    modelo
  )
  
  if (
    n - k - 1 <= 0
  ) {
    return(Inf)
  }
  
  as.numeric(
    aic +
      (
        2 *
          k *
          (k + 1)
      ) /
      (
        n -
          k -
          1
      )
  )
}

hqic_modelo <- function(
    modelo
) {
  
  ll <- logLik(
    modelo
  )
  
  k <- attr(
    ll,
    "df"
  )
  
  n <- nobs(
    modelo
  )
  
  as.numeric(
    -2 *
      as.numeric(ll) +
      2 *
      k *
      log(
        log(n)
      )
  )
}

to_lm_safe <- function(
    modelo
) {
  
  tryCatch(
    ARDL::to_lm(
      modelo,
      fix_names = TRUE,
      data_class = "ts"
    ),
    error = function(e) modelo
  )
}

extrair_q <- function(
    linha,
    x_vars
) {
  
  as.integer(
    unlist(
      linha[
        1,
        paste0(
          "q_",
          x_vars
        ),
        drop = FALSE
      ],
      use.names = FALSE
    )
  )
}

# ============================================================================
# 9. BUSCA ARDL
# ============================================================================

buscar_ardl <- function(
    ts_data,
    y_var,
    x_vars,
    max_p = 6,
    max_q = 6,
    causal = TRUE,
    nome_modelo = "modelo"
) {
  
  message(
    "\nIniciando busca: ",
    nome_modelo
  )
  
  max_lag_original <- if (
    causal
  ) {
    max(
      max_p,
      max_q + 1
    )
  } else {
    max(
      max_p,
      max_q
    )
  }
  
  inicio_comum <- seq.Date(
    floor_date(
      data_inicio,
      "month"
    ),
    by = "month",
    length.out =
      max_lag_original +
      1
  )[
    max_lag_original +
      1
  ]
  
  start_ts <- c(
    year(
      inicio_comum
    ),
    month(
      inicio_comum
    )
  )
  
  end_ts <- c(
    year(
      data_fim
    ),
    month(
      data_fim
    )
  )
  
  n_comum <- length(
    seq.Date(
      inicio_comum,
      floor_date(
        data_fim,
        "month"
      ),
      by = "month"
    )
  )
  
  grid_list <- c(
    list(
      p = 1:max_p
    ),
    
    setNames(
      rep(
        list(
          0:max_q
        ),
        length(
          x_vars
        )
      ),
      paste0(
        "q",
        seq_along(
          x_vars
        )
      )
    )
  )
  
  grid <- do.call(
    expand.grid,
    c(
      grid_list,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  )
  
  form <- as.formula(
    paste(
      y_var,
      "~",
      paste(
        x_vars,
        collapse = " + "
      )
    )
  )
  
  resultados <- vector(
    "list",
    nrow(
      grid
    )
  )
  
  for (
    i in seq_len(
      nrow(
        grid
      )
    )
  ) {
    
    row <- grid[
      i,
      ,
      drop = FALSE
    ]
    
    p <- as.integer(
      row$p
    )
    
    q <- as.integer(
      unlist(
        row[
          1,
          paste0(
            "q",
            seq_along(
              x_vars
            )
          ),
          drop = FALSE
        ],
        use.names = FALSE
      )
    )
    
    # Aproximação do nº de parâmetros
    k_aprox <- 1 +
      p +
      sum(
        q + 1
      )
    
    if (
      n_comum /
      k_aprox <
      min_obs_por_coef
    ) {
      next
    }
    
    ordem <- c(
      p,
      q
    )
    
    mod <- tryCatch(
      ARDL::ardl(
        formula = form,
        data = ts_data,
        order = ordem,
        start = start_ts,
        end = end_ts
      ),
      error = function(e) NULL
    )
    
    if (
      is.null(mod)
    ) {
      next
    }
    
    ll <- tryCatch(
      logLik(mod),
      error = function(e) NULL
    )
    
    if (
      is.null(ll)
    ) {
      next
    }
    
    out <- tibble(
      p = p,
      AIC = AIC(mod),
      AICc = aicc_modelo(
        mod
      ),
      BIC = BIC(mod),
      HQIC = hqic_modelo(
        mod
      ),
      n_parametros = attr(
        ll,
        "df"
      ),
      n_observacoes_efetivas = nobs(
        mod
      )
    )
    
    for (
      j in seq_along(
        x_vars
      )
    ) {
      
      out[
        [
          paste0(
            "q_",
            x_vars[j]
          )
        ]
      ] <- q[j]
    }
    
    resultados[[i]] <- out
    
    if (
      i %% 500 ==
      0
    ) {
      
      message(
        "  candidatos processados: ",
        i,
        " / ",
        nrow(grid)
      )
    }
  }
  
  tab <- bind_rows(
    resultados
  )
  
  if (
    nrow(tab) ==
    0
  ) {
    
    stop(
      "Nenhum modelo ARDL admissível foi estimado em ",
      nome_modelo,
      "."
    )
  }
  
  tab <- tab %>%
    arrange(
      BIC,
      AICc,
      HQIC,
      AIC
    )
  
  attr(
    tab,
    "formula"
  ) <- form
  
  attr(
    tab,
    "x_vars"
  ) <- x_vars
  
  attr(
    tab,
    "start_ts"
  ) <- start_ts
  
  attr(
    tab,
    "end_ts"
  ) <- end_ts
  
  attr(
    tab,
    "ts_data"
  ) <- ts_data
  
  attr(
    tab,
    "inicio_comum"
  ) <- inicio_comum
  
  attr(
    tab,
    "nome_modelo"
  ) <- nome_modelo
  
  message(
    "Busca concluída. Modelos admissíveis: ",
    nrow(tab)
  )
  
  tab
}

ajustar_linha <- function(
    busca,
    linha
) {
  
  x_vars <- attr(
    busca,
    "x_vars"
  )
  
  q <- extrair_q(
    linha,
    x_vars
  )
  
  ARDL::ardl(
    formula = attr(
      busca,
      "formula"
    ),
    data = attr(
      busca,
      "ts_data"
    ),
    order = c(
      as.integer(
        linha$p
      ),
      q
    ),
    start = attr(
      busca,
      "start_ts"
    ),
    end = attr(
      busca,
      "end_ts"
    )
  )
}

# ============================================================================
# 10. SELEÇÃO FINAL COM DIAGNÓSTICO
# ============================================================================

autocor_diagnostico <- function(
    modelo,
    p_ardl
) {
  
  lm_m <- to_lm_safe(
    modelo
  )
  
  n <- nobs(
    lm_m
  )
  
  lag_use <- max(
    1,
    min(
      lag_diagnostico,
      floor(
        n /
          5
      )
    )
  )
  
  bg <- tryCatch(
    lmtest::bgtest(
      lm_m,
      order = lag_use,
      type = "Chisq"
    ),
    error = function(e) NULL
  )
  
  lb <- tryCatch(
    Box.test(
      residuals(
        lm_m
      ),
      lag = lag_use,
      type = "Ljung-Box",
      fitdf = min(
        p_ardl,
        lag_use - 1
      )
    ),
    error = function(e) NULL
  )
  
  list(
    bg_p = if (
      is.null(bg)
    ) {
      NA_real_
    } else {
      as.numeric(
        bg$p.value
      )
    },
    
    ljung_p = if (
      is.null(lb)
    ) {
      NA_real_
    } else {
      as.numeric(
        lb$p.value
      )
    }
  )
}

selecionar_modelo_final <- function(
    busca,
    max_candidatos = 150
) {
  
  candidatos <- busca %>%
    arrange(
      BIC,
      AICc,
      HQIC,
      AIC
    ) %>%
    slice_head(
      n = min(
        max_candidatos,
        nrow(
          busca
        )
      )
    )
  
  for (
    i in seq_len(
      nrow(
        candidatos
      )
    )
  ) {
    
    linha <- candidatos[
      i,
      ,
      drop = FALSE
    ]
    
    mod <- tryCatch(
      ajustar_linha(
        busca,
        linha
      ),
      error = function(e) NULL
    )
    
    if (
      is.null(mod)
    ) {
      next
    }
    
    d <- autocor_diagnostico(
      mod,
      linha$p
    )
    
    if (
      !is.na(
        d$bg_p
      ) &&
      !is.na(
        d$ljung_p
      ) &&
      d$bg_p >= 0.05 &&
      d$ljung_p >= 0.05
    ) {
      
      return(
        list(
          linha = linha,
          modelo = mod,
          bg_p = d$bg_p,
          ljung_p = d$ljung_p,
          passou = TRUE,
          rank_bic = i
        )
      )
    }
  }
  
  warning(
    "Nenhum dos melhores candidatos passou simultaneamente ",
    "Breusch-Godfrey e Ljung-Box a 5%. ",
    "O menor BIC será mantido apenas como referência."
  )
  
  linha <- candidatos[
    1,
    ,
    drop = FALSE
  ]
  
  mod <- ajustar_linha(
    busca,
    linha
  )
  
  d <- autocor_diagnostico(
    mod,
    linha$p
  )
  
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
# 11. DEFINIÇÃO DOS DOIS MODELOS
# ============================================================================

x_sem_bets <- c(
  "selic_L1",
  "spread_livre_pf_L1",
  "ipca_L1",
  "desemprego_L1",
  "renda_L1"
)

x_com_bets <- c(
  "selic_L1",
  "spread_livre_pf_L1",
  "ipca_L1",
  "desemprego_L1",
  "renda_L1",
  "bets_L1"
)

if (
  incluir_inad_geral_no_modelo
) {
  
  x_sem_bets <- c(
    x_sem_bets,
    "inad_pf_geral_L1"
  )
  
  x_com_bets <- c(
    x_com_bets,
    "inad_pf_geral_L1"
  )
}

# ============================================================================
# 12. MODELO 1 – SEM BETS
# ============================================================================

message("\n============================================================")
message("6. ESTIMANDO MODELO 1 – SEM BETS")
message("============================================================")

ts_sem_bets <- criar_ts(
  base_ardl,
  c(
    "inad_pf_10sm",
    x_sem_bets
  )
)

busca_sem_bets <- buscar_ardl(
  ts_data = ts_sem_bets,
  y_var = "inad_pf_10sm",
  x_vars = x_sem_bets,
  max_p = max_p,
  max_q = max_q,
  causal = TRUE,
  nome_modelo = "MODELO 1 – SEM BETS"
)

melhor_sem_AIC <- busca_sem_bets %>%
  arrange(AIC) %>%
  slice(1)

melhor_sem_AICc <- busca_sem_bets %>%
  arrange(AICc) %>%
  slice(1)

melhor_sem_BIC <- busca_sem_bets %>%
  arrange(BIC) %>%
  slice(1)

melhor_sem_HQIC <- busca_sem_bets %>%
  arrange(HQIC) %>%
  slice(1)

selecao_sem <- selecionar_modelo_final(
  busca_sem_bets
)

modelo_sem_bets <- selecao_sem$modelo
linha_sem_bets <- selecao_sem$linha
lm_sem_bets <- to_lm_safe(
  modelo_sem_bets
)

# ============================================================================
# 13. MODELO 2 – COM BETS
# ============================================================================

message("\n============================================================")
message("7. ESTIMANDO MODELO 2 – COM BETS")
message("============================================================")

ts_com_bets <- criar_ts(
  base_ardl,
  c(
    "inad_pf_10sm",
    x_com_bets
  )
)

busca_com_bets <- buscar_ardl(
  ts_data = ts_com_bets,
  y_var = "inad_pf_10sm",
  x_vars = x_com_bets,
  max_p = max_p,
  max_q = max_q,
  causal = TRUE,
  nome_modelo = "MODELO 2 – COM BETS"
)

melhor_com_AIC <- busca_com_bets %>%
  arrange(AIC) %>%
  slice(1)

melhor_com_AICc <- busca_com_bets %>%
  arrange(AICc) %>%
  slice(1)

melhor_com_BIC <- busca_com_bets %>%
  arrange(BIC) %>%
  slice(1)

melhor_com_HQIC <- busca_com_bets %>%
  arrange(HQIC) %>%
  slice(1)

selecao_com <- selecionar_modelo_final(
  busca_com_bets
)

modelo_com_bets <- selecao_com$modelo
linha_com_bets <- selecao_com$linha
lm_com_bets <- to_lm_safe(
  modelo_com_bets
)

# ============================================================================
# 14. DIAGNÓSTICOS
# ============================================================================

diagnosticar_modelo <- function(
    modelo_ardl,
    p_ardl
) {
  
  lm_m <- to_lm_safe(
    modelo_ardl
  )
  
  n <- nobs(
    lm_m
  )
  
  lag_use <- max(
    1,
    min(
      lag_diagnostico,
      floor(
        n /
          5
      )
    )
  )
  
  bg <- tryCatch(
    lmtest::bgtest(
      lm_m,
      order = lag_use,
      type = "Chisq"
    ),
    error = function(e) NULL
  )
  
  lj <- tryCatch(
    Box.test(
      residuals(
        lm_m
      ),
      lag = lag_use,
      type = "Ljung-Box",
      fitdf = min(
        p_ardl,
        lag_use - 1
      )
    ),
    error = function(e) NULL
  )
  
  bp <- tryCatch(
    lmtest::bptest(
      lm_m
    ),
    error = function(e) NULL
  )
  
  jb <- tryCatch(
    tseries::jarque.bera.test(
      residuals(
        lm_m
      )
    ),
    error = function(e) NULL
  )
  
  reset <- tryCatch(
    lmtest::resettest(
      lm_m,
      power = 2:3,
      type = "fitted"
    ),
    error = function(e) NULL
  )
  
  tabela <- bind_rows(
    
    tibble(
      teste = "Breusch-Godfrey",
      H0 = "Ausência de autocorrelação serial",
      H1 = "Há autocorrelação serial",
      estatistica = if (
        is.null(bg)
      ) {
        NA_real_
      } else {
        as.numeric(
          bg$statistic
        )
      },
      p_valor = if (
        is.null(bg)
      ) {
        NA_real_
      } else {
        as.numeric(
          bg$p.value
        )
      }
    ),
    
    tibble(
      teste = "Ljung-Box",
      H0 = "Resíduos sem autocorrelação conjunta",
      H1 = "Há autocorrelação residual",
      estatistica = if (
        is.null(lj)
      ) {
        NA_real_
      } else {
        as.numeric(
          lj$statistic
        )
      },
      p_valor = if (
        is.null(lj)
      ) {
        NA_real_
      } else {
        as.numeric(
          lj$p.value
        )
      }
    ),
    
    tibble(
      teste = "Breusch-Pagan",
      H0 = "Homoscedasticidade",
      H1 = "Heterocedasticidade",
      estatistica = if (
        is.null(bp)
      ) {
        NA_real_
      } else {
        as.numeric(
          bp$statistic
        )
      },
      p_valor = if (
        is.null(bp)
      ) {
        NA_real_
      } else {
        as.numeric(
          bp$p.value
        )
      }
    ),
    
    tibble(
      teste = "Jarque-Bera",
      H0 = "Normalidade dos resíduos",
      H1 = "Não normalidade",
      estatistica = if (
        is.null(jb)
      ) {
        NA_real_
      } else {
        as.numeric(
          jb$statistic
        )
      },
      p_valor = if (
        is.null(jb)
      ) {
        NA_real_
      } else {
        as.numeric(
          jb$p.value
        )
      }
    ),
    
    tibble(
      teste = "Ramsey RESET",
      H0 = "Forma funcional adequada",
      H1 = "Possível erro de especificação",
      estatistica = if (
        is.null(reset)
      ) {
        NA_real_
      } else {
        as.numeric(
          reset$statistic
        )
      },
      p_valor = if (
        is.null(reset)
      ) {
        NA_real_
      } else {
        as.numeric(
          reset$p.value
        )
      }
    )
  ) %>%
    mutate(
      conclusao_5pct = case_when(
        is.na(p_valor) ~
          "Teste indisponível",
        p_valor < 0.05 ~
          "Rejeita H0 a 5%",
        TRUE ~
          "Não rejeita H0 a 5%"
      )
    )
  
  list(
    lm = lm_m,
    tabela = tabela
  )
}

diag_sem <- diagnosticar_modelo(
  modelo_sem_bets,
  linha_sem_bets$p
)

diag_com <- diagnosticar_modelo(
  modelo_com_bets,
  linha_com_bets$p
)

# ============================================================================
# 15. COEFICIENTES HAC
# ============================================================================

coef_hac <- function(
    modelo_lm
) {
  
  lag_hac <- max(
    1,
    min(
      12,
      floor(
        nobs(
          modelo_lm
        )^(1 / 4)
      )
    )
  )
  
  V <- sandwich::NeweyWest(
    modelo_lm,
    lag = lag_hac,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  tab <- lmtest::coeftest(
    modelo_lm,
    vcov. = V
  )
  
  tibble(
    termo = rownames(
      tab
    ),
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

coef_sem <- coef_hac(
  lm_sem_bets
)

coef_com <- coef_hac(
  lm_com_bets
)

# ============================================================================
# 16. VIF / MULTICOLINEARIDADE
# ============================================================================

vif_manual <- function(
    modelo_lm
) {
  
  X <- model.matrix(
    modelo_lm
  )
  
  if (
    "(Intercept)" %in%
    colnames(X)
  ) {
    
    X <- X[
      ,
      colnames(X) !=
        "(Intercept)",
      drop = FALSE
    ]
  }
  
  sds <- apply(
    X,
    2,
    sd,
    na.rm = TRUE
  )
  
  X <- X[
    ,
    is.finite(sds) &
      sds > 0,
    drop = FALSE
  ]
  
  if (
    ncol(X) <
    2
  ) {
    
    return(
      list(
        tabela = tibble(
          termo = colnames(X),
          VIF = NA_real_,
          tolerance = NA_real_
        ),
        condition_number = NA_real_,
        correlacao = cor(X)
      )
    )
  }
  
  vifs <- sapply(
    seq_len(
      ncol(X)
    ),
    function(j) {
      
      y <- X[, j]
      z <- X[, -j, drop = FALSE]
      
      fit <- tryCatch(
        lm(
          y ~ z
        ),
        error = function(e) NULL
      )
      
      if (
        is.null(fit)
      ) {
        return(
          NA_real_
        )
      }
      
      r2 <- summary(
        fit
      )$r.squared
      
      if (
        !is.finite(r2)
      ) {
        return(
          NA_real_
        )
      }
      
      if (
        r2 >= 1
      ) {
        return(
          Inf
        )
      }
      
      1 /
        (
          1 -
            r2
        )
    }
  )
  
  cn <- tryCatch(
    kappa(
      scale(X),
      exact = TRUE
    ),
    error = function(e) NA_real_
  )
  
  list(
    tabela = tibble(
      termo = colnames(X),
      VIF = as.numeric(
        vifs
      ),
      tolerance = ifelse(
        is.finite(
          vifs
        ),
        1 /
          vifs,
        0
      )
    ),
    condition_number = cn,
    correlacao = cor(
      X,
      use = "complete.obs"
    )
  )
}

multi_sem <- vif_manual(
  lm_sem_bets
)

multi_com <- vif_manual(
  lm_com_bets
)

# ============================================================================
# 17. EFEITO ACUMULADO DOS LAGS
# ============================================================================

efeito_acumulado <- function(
    modelo_lm,
    padrao
) {
  
  b <- coef(
    modelo_lm
  )
  
  idx <- grep(
    padrao,
    names(b),
    fixed = TRUE
  )
  
  if (
    length(idx) ==
    0
  ) {
    
    return(
      tibble(
        variavel = padrao,
        efeito_acumulado = NA_real_,
        erro_padrao = NA_real_,
        estatistica = NA_real_,
        p_valor = NA_real_
      )
    )
  }
  
  lag_hac <- max(
    1,
    min(
      12,
      floor(
        nobs(
          modelo_lm
        )^(1 / 4)
      )
    )
  )
  
  V <- sandwich::NeweyWest(
    modelo_lm,
    lag = lag_hac,
    prewhite = FALSE,
    adjust = TRUE
  )
  
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
  
  tt <- est /
    se
  
  pv <- 2 *
    pt(
      abs(tt),
      df = df.residual(
        modelo_lm
      ),
      lower.tail = FALSE
    )
  
  tibble(
    variavel = padrao,
    efeito_acumulado = est,
    erro_padrao = se,
    estatistica = tt,
    p_valor = pv
  )
}

efeitos_sem <- bind_rows(
  efeito_acumulado(
    lm_sem_bets,
    "selic_L1"
  ),
  efeito_acumulado(
    lm_sem_bets,
    "spread_livre_pf_L1"
  ),
  efeito_acumulado(
    lm_sem_bets,
    "ipca_L1"
  ),
  efeito_acumulado(
    lm_sem_bets,
    "desemprego_L1"
  ),
  efeito_acumulado(
    lm_sem_bets,
    "renda_L1"
  )
)

efeitos_com <- bind_rows(
  efeito_acumulado(
    lm_com_bets,
    "selic_L1"
  ),
  efeito_acumulado(
    lm_com_bets,
    "spread_livre_pf_L1"
  ),
  efeito_acumulado(
    lm_com_bets,
    "ipca_L1"
  ),
  efeito_acumulado(
    lm_com_bets,
    "desemprego_L1"
  ),
  efeito_acumulado(
    lm_com_bets,
    "renda_L1"
  ),
  efeito_acumulado(
    lm_com_bets,
    "bets_L1"
  )
)

efeito_bets <- efeito_acumulado(
  lm_com_bets,
  "bets_L1"
)

# ============================================================================
# 18. BOUNDS TEST / ECM
# ============================================================================

rodar_bounds <- function(
    modelo_ardl
) {
  
  if (
    ha_I2
  ) {
    
    return(
      list(
        conclusao =
          "NÃO INTERPRETADO – possível I(2)",
        bounds = NULL,
        recm = NULL,
        longo_prazo = NULL
      )
    )
  }
  
  b <- tryCatch(
    {
      if (
        usar_bounds_exato
      ) {
        
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
    },
    error = function(e) NULL
  )
  
  if (
    is.null(b)
  ) {
    
    return(
      list(
        conclusao =
          "Bounds Test não disponível",
        bounds = NULL,
        recm = NULL,
        longo_prazo = NULL
      )
    )
  }
  
  f <- as.numeric(
    b$statistic[1]
  )
  
  lim <- as.numeric(
    b$parameters
  )
  
  conclusao <- "INCONCLUSIVO"
  
  if (
    length(lim) >=
    2
  ) {
    
    I0 <- min(
      lim[1:2],
      na.rm = TRUE
    )
    
    I1 <- max(
      lim[1:2],
      na.rm = TRUE
    )
    
    conclusao <- if (
      f > I1
    ) {
      
      "SIM – evidência de cointegração"
      
    } else if (
      f < I0
    ) {
      
      "NÃO – sem evidência de cointegração"
      
    } else {
      
      "INCONCLUSIVO"
    }
  }
  
  recm_obj <- NULL
  lr_obj <- NULL
  
  if (
    grepl(
      "^SIM",
      conclusao
    )
  ) {
    
    recm_obj <- tryCatch(
      ARDL::recm(
        modelo_ardl,
        case = 3
      ),
      error = function(e) NULL
    )
    
    lr_obj <- tryCatch(
      ARDL::multipliers(
        modelo_ardl,
        type = "lr",
        se = TRUE
      ),
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

bounds_sem <- rodar_bounds(
  modelo_sem_bets
)

bounds_com <- rodar_bounds(
  modelo_com_bets
)

# ============================================================================
# 19. COMPARAÇÃO ENTRE MODELOS
# ============================================================================

metricas_modelo <- function(
    nome,
    modelo_ardl,
    diag
) {
  
  lm_m <- diag$lm
  
  tibble(
    modelo = nome,
    n = nobs(
      lm_m
    ),
    AIC = AIC(
      modelo_ardl
    ),
    AICc = aicc_modelo(
      modelo_ardl
    ),
    BIC = BIC(
      modelo_ardl
    ),
    HQIC = hqic_modelo(
      modelo_ardl
    ),
    R2_ajustado = summary(
      lm_m
    )$adj.r.squared,
    RMSE = sqrt(
      mean(
        residuals(
          lm_m
        )^2,
        na.rm = TRUE
      )
    ),
    BG_p = diag$tabela %>%
      filter(
        teste ==
          "Breusch-Godfrey"
      ) %>%
      pull(
        p_valor
      ),
    LjungBox_p = diag$tabela %>%
      filter(
        teste ==
          "Ljung-Box"
      ) %>%
      pull(
        p_valor
      ),
    BP_p = diag$tabela %>%
      filter(
        teste ==
          "Breusch-Pagan"
      ) %>%
      pull(
        p_valor
      ),
    RESET_p = diag$tabela %>%
      filter(
        teste ==
          "Ramsey RESET"
      ) %>%
      pull(
        p_valor
      ),
    JB_p = diag$tabela %>%
      filter(
        teste ==
          "Jarque-Bera"
      ) %>%
      pull(
        p_valor
      )
  )
}

comparacao_modelos <- bind_rows(
  
  metricas_modelo(
    "MODELO 1 – SEM BETS",
    modelo_sem_bets,
    diag_sem
  ),
  
  metricas_modelo(
    "MODELO 2 – COM BETS",
    modelo_com_bets,
    diag_com
  )
)

# ============================================================================
# 20. ORDENS / LAGS
# ============================================================================

fmt_ordem <- function(
    linha,
    x_vars
) {
  
  q <- extrair_q(
    linha,
    x_vars
  )
  
  paste0(
    "ARDL(",
    paste(
      c(
        as.integer(
          linha$p
        ),
        q
      ),
      collapse = ","
    ),
    ")"
  )
}

mostrar_lags <- function(
    linha,
    x_vars,
    nomes
) {
  
  q <- extrair_q(
    linha,
    x_vars
  )
  
  tibble(
    variavel = nomes,
    q_ARDL_sobre_X_L1 = q,
    primeiro_lag_original = 1,
    ultimo_lag_original =
      q +
      1
  )
}

nomes_sem <- c(
  "Selic",
  "Spread Livre PF",
  "IPCA",
  "Desemprego",
  "Renda"
)

nomes_com <- c(
  "Selic",
  "Spread Livre PF",
  "IPCA",
  "Desemprego",
  "Renda",
  "Bets"
)

if (
  incluir_inad_geral_no_modelo
) {
  
  nomes_sem <- c(
    nomes_sem,
    "Inadimplência PF geral"
  )
  
  nomes_com <- c(
    nomes_com,
    "Inadimplência PF geral"
  )
}

lags_sem <- mostrar_lags(
  linha_sem_bets,
  x_sem_bets,
  nomes_sem
)

lags_com <- mostrar_lags(
  linha_com_bets,
  x_com_bets,
  nomes_com
)

# ============================================================================
# 21. OBSERVADO X AJUSTADO
# ============================================================================

criar_fit_df <- function(
    modelo_lm,
    busca
) {
  
  n <- nobs(
    modelo_lm
  )
  
  inicio <- attr(
    busca,
    "inicio_comum"
  )
  
  datas <- seq.Date(
    inicio,
    by = "month",
    length.out = n
  )
  
  tibble(
    data = datas,
    observado = as.numeric(
      model.response(
        model.frame(
          modelo_lm
        )
      )
    ),
    ajustado = as.numeric(
      fitted(
        modelo_lm
      )
    ),
    residuo = as.numeric(
      residuals(
        modelo_lm
      )
    )
  )
}

fit_sem <- criar_fit_df(
  lm_sem_bets,
  busca_sem_bets
)

fit_com <- criar_fit_df(
  lm_com_bets,
  busca_com_bets
)

plot_fit <- function(
    df,
    titulo,
    arquivo
) {
  
  g <- df %>%
    select(
      data,
      observado,
      ajustado
    ) %>%
    pivot_longer(
      -data,
      names_to = "serie",
      values_to = "valor"
    ) %>%
    ggplot(
      aes(
        data,
        valor,
        linetype = serie
      )
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    labs(
      title = titulo,
      x = NULL,
      y = NULL,
      linetype = NULL
    ) +
    theme_minimal(
      base_size = 12
    )
  
  ggsave(
    file.path(
      dir_saida,
      arquivo
    ),
    g,
    width = 8,
    height = 4.5,
    dpi = 150
  )
}

plot_fit(
  fit_sem,
  "Observado × Ajustado – Modelo sem Bets",
  "observado_ajustado_sem_bets.png"
)

plot_fit(
  fit_com,
  "Observado × Ajustado – Modelo com Bets",
  "observado_ajustado_com_bets.png"
)

# ============================================================================
# 22. ACF / PACF
# ============================================================================

png(
  file.path(
    dir_saida,
    "acf_sem_bets.png"
  ),
  width = 1000,
  height = 650
)

acf(
  residuals(
    lm_sem_bets
  ),
  main =
    "ACF – resíduos sem Bets"
)

dev.off()

png(
  file.path(
    dir_saida,
    "pacf_sem_bets.png"
  ),
  width = 1000,
  height = 650
)

pacf(
  residuals(
    lm_sem_bets
  ),
  main =
    "PACF – resíduos sem Bets"
)

dev.off()

png(
  file.path(
    dir_saida,
    "acf_com_bets.png"
  ),
  width = 1000,
  height = 650
)

acf(
  residuals(
    lm_com_bets
  ),
  main =
    "ACF – resíduos com Bets"
)

dev.off()

png(
  file.path(
    dir_saida,
    "pacf_com_bets.png"
  ),
  width = 1000,
  height = 650
)

pacf(
  residuals(
    lm_com_bets
  ),
  main =
    "PACF – resíduos com Bets"
)

dev.off()

# ============================================================================
# 23. ROBUSTEZ CONTEMPORÂNEA
# ============================================================================

robustez_contemporanea <- NULL

if (
  rodar_robustez_contemporanea
) {
  
  message("\n============================================================")
  message("8. ROBUSTEZ – REGRESSORAS CONTEMPORÂNEAS")
  message("============================================================")
  
  x_sem_cont <- c(
    "selic",
    "spread_livre_pf",
    "ipca",
    "desemprego",
    "renda"
  )
  
  x_com_cont <- c(
    "selic",
    "spread_livre_pf",
    "ipca",
    "desemprego",
    "renda",
    "bets"
  )
  
  if (
    incluir_inad_geral_no_modelo
  ) {
    
    x_sem_cont <- c(
      x_sem_cont,
      "inad_pf_geral"
    )
    
    x_com_cont <- c(
      x_com_cont,
      "inad_pf_geral"
    )
  }
  
  ts_sem_cont <- criar_ts(
    base_ardl,
    c(
      "inad_pf_10sm",
      x_sem_cont
    )
  )
  
  busca_sem_cont <- buscar_ardl(
    ts_data = ts_sem_cont,
    y_var = "inad_pf_10sm",
    x_vars = x_sem_cont,
    max_p = max_p,
    max_q = max_q,
    causal = FALSE,
    nome_modelo =
      "ROBUSTEZ SEM BETS – contemporâneo"
  )
  
  sel_sem_cont <- selecionar_modelo_final(
    busca_sem_cont
  )
  
  diag_sem_cont <- diagnosticar_modelo(
    sel_sem_cont$modelo,
    sel_sem_cont$linha$p
  )
  
  ts_com_cont <- criar_ts(
    base_ardl,
    c(
      "inad_pf_10sm",
      x_com_cont
    )
  )
  
  busca_com_cont <- buscar_ardl(
    ts_data = ts_com_cont,
    y_var = "inad_pf_10sm",
    x_vars = x_com_cont,
    max_p = max_p,
    max_q = max_q,
    causal = FALSE,
    nome_modelo =
      "ROBUSTEZ COM BETS – contemporâneo"
  )
  
  sel_com_cont <- selecionar_modelo_final(
    busca_com_cont
  )
  
  diag_com_cont <- diagnosticar_modelo(
    sel_com_cont$modelo,
    sel_com_cont$linha$p
  )
  
  robustez_contemporanea <- bind_rows(
    
    metricas_modelo(
      "SEM BETS – contemporâneo",
      sel_sem_cont$modelo,
      diag_sem_cont
    ),
    
    metricas_modelo(
      "COM BETS – contemporâneo",
      sel_com_cont$modelo,
      diag_com_cont
    )
  )
}

# ============================================================================
# 24. EXPORTAÇÃO DOS RESULTADOS
# ============================================================================

bounds_sem_export <- if (
  !is.null(
    bounds_sem$bounds
  )
) {
  
  as.data.frame(
    bounds_sem$bounds$tab
  )
  
} else {
  
  data.frame(
    resultado =
      bounds_sem$conclusao
  )
}

bounds_com_export <- if (
  !is.null(
    bounds_com$bounds
  )
) {
  
  as.data.frame(
    bounds_com$bounds$tab
  )
  
} else {
  
  data.frame(
    resultado =
      bounds_com$conclusao
  )
}

lr_sem_export <- if (
  !is.null(
    bounds_sem$longo_prazo
  )
) {
  
  as.data.frame(
    bounds_sem$longo_prazo
  )
  
} else {
  
  data.frame(
    resultado =
      "Não calculado"
  )
}

lr_com_export <- if (
  !is.null(
    bounds_com$longo_prazo
  )
) {
  
  as.data.frame(
    bounds_com$longo_prazo
  )
  
} else {
  
  data.frame(
    resultado =
      "Não calculado"
  )
}

ecm_sem_export <- if (
  !is.null(
    bounds_sem$recm
  )
) {
  
  as.data.frame(
    summary(
      bounds_sem$recm
    )$coefficients
  )
  
} else {
  
  data.frame(
    resultado =
      "Não calculado"
  )
}

ecm_com_export <- if (
  !is.null(
    bounds_com$recm
  )
) {
  
  as.data.frame(
    summary(
      bounds_com$recm
    )$coefficients
  )
  
} else {
  
  data.frame(
    resultado =
      "Não calculado"
  )
}

abas <- list(
  
  base_final =
    base_final,
  
  descritivas =
    descritivas,
  
  correlacoes =
    as.data.frame(
      mat_cor
    ),
  
  estacionariedade =
    tab_estacionariedade,
  
  ranking_sem_bets =
    head(
      busca_sem_bets,
      250
    ),
  
  ranking_com_bets =
    head(
      busca_com_bets,
      250
    ),
  
  lags_sem_bets =
    lags_sem,
  
  lags_com_bets =
    lags_com,
  
  coef_sem_bets_HAC =
    coef_sem,
  
  coef_com_bets_HAC =
    coef_com,
  
  efeitos_sem_bets =
    efeitos_sem,
  
  efeitos_com_bets =
    efeitos_com,
  
  diagnosticos_sem_bets =
    diag_sem$tabela,
  
  diagnosticos_com_bets =
    diag_com$tabela,
  
  VIF_sem_bets =
    multi_sem$tabela,
  
  VIF_com_bets =
    multi_com$tabela,
  
  comparacao_modelos =
    comparacao_modelos,
  
  bounds_sem_bets =
    bounds_sem_export,
  
  bounds_com_bets =
    bounds_com_export,
  
  longo_prazo_sem_bets =
    lr_sem_export,
  
  longo_prazo_com_bets =
    lr_com_export,
  
  ECM_sem_bets =
    ecm_sem_export,
  
  ECM_com_bets =
    ecm_com_export
)

if (
  !is.null(
    robustez_contemporanea
  )
) {
  
  abas$robustez_contemporanea <-
    robustez_contemporanea
}

openxlsx::write.xlsx(
  abas,
  file = file.path(
    dir_saida,
    "resultados_SFN_modelos_com_sem_bets.xlsx"
  ),
  overwrite = TRUE
)

readr::write_csv(
  busca_sem_bets,
  file.path(
    dir_saida,
    "ranking_ARDL_sem_bets.csv"
  )
)

readr::write_csv(
  busca_com_bets,
  file.path(
    dir_saida,
    "ranking_ARDL_com_bets.csv"
  )
)

# ============================================================================
# 25. SÍNTESE FINAL
# ============================================================================

cat("\n\n")
cat("==================================================================\n")
cat("SFN – INADIMPLÊNCIA PF ATÉ 10 SM\n")
cat("Período: 2020M01–2026M07\n")
cat("==================================================================\n\n")

cat(
  "Observações disponíveis: ",
  nrow(base_final),
  "\n\n",
  sep = ""
)

cat("--------------------------------------------------\n")
cat("MODELO 1 – SEM BETS\n")
cat("--------------------------------------------------\n")

cat(
  "Melhor AIC: ",
  fmt_ordem(
    melhor_sem_AIC,
    x_sem_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Melhor AICc: ",
  fmt_ordem(
    melhor_sem_AICc,
    x_sem_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Melhor BIC: ",
  fmt_ordem(
    melhor_sem_BIC,
    x_sem_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Melhor HQIC: ",
  fmt_ordem(
    melhor_sem_HQIC,
    x_sem_bets
  ),
  "\n\n",
  sep = ""
)

cat(
  "MODELO FINAL: ",
  fmt_ordem(
    linha_sem_bets,
    x_sem_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Observações usadas: ",
  nobs(
    lm_sem_bets
  ),
  "\n",
  sep = ""
)

cat(
  "BIC: ",
  round(
    linha_sem_bets$BIC,
    4
  ),
  "\n",
  sep = ""
)

cat(
  "AICc: ",
  round(
    linha_sem_bets$AICc,
    4
  ),
  "\n\n",
  sep = ""
)

print(
  lags_sem
)

cat(
  "\nBounds Test: ",
  bounds_sem$conclusao,
  "\n",
  sep = ""
)

cat(
  "Condition Number: ",
  round(
    multi_sem$condition_number,
    3
  ),
  "\n\n",
  sep = ""
)

cat("--------------------------------------------------\n")
cat("MODELO 2 – COM BETS\n")
cat("--------------------------------------------------\n")

cat(
  "Melhor AIC: ",
  fmt_ordem(
    melhor_com_AIC,
    x_com_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Melhor AICc: ",
  fmt_ordem(
    melhor_com_AICc,
    x_com_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Melhor BIC: ",
  fmt_ordem(
    melhor_com_BIC,
    x_com_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Melhor HQIC: ",
  fmt_ordem(
    melhor_com_HQIC,
    x_com_bets
  ),
  "\n\n",
  sep = ""
)

cat(
  "MODELO FINAL: ",
  fmt_ordem(
    linha_com_bets,
    x_com_bets
  ),
  "\n",
  sep = ""
)

cat(
  "Observações usadas: ",
  nobs(
    lm_com_bets
  ),
  "\n",
  sep = ""
)

cat(
  "BIC: ",
  round(
    linha_com_bets$BIC,
    4
  ),
  "\n",
  sep = ""
)

cat(
  "AICc: ",
  round(
    linha_com_bets$AICc,
    4
  ),
  "\n\n",
  sep = ""
)

print(
  lags_com
)

cat(
  "\nBounds Test: ",
  bounds_com$conclusao,
  "\n",
  sep = ""
)

cat(
  "Condition Number: ",
  round(
    multi_com$condition_number,
    3
  ),
  "\n\n",
  sep = ""
)

cat("--------------------------------------------------\n")
cat("EFEITO DAS BETS\n")
cat("--------------------------------------------------\n")

print(
  efeito_bets
)

cat("\n--------------------------------------------------\n")
cat("COMPARAÇÃO ENTRE OS DOIS MODELOS\n")
cat("--------------------------------------------------\n")

print(
  comparacao_modelos
)

bic_sem <- comparacao_modelos %>%
  filter(
    modelo ==
      "MODELO 1 – SEM BETS"
  ) %>%
  pull(
    BIC
  )

bic_com <- comparacao_modelos %>%
  filter(
    modelo ==
      "MODELO 2 – COM BETS"
  ) %>%
  pull(
    BIC
  )

cat("\n")

if (
  bic_com <
  bic_sem
) {
  
  cat(
    "O modelo COM Bets apresentou BIC menor.\n"
  )
  
} else {
  
  cat(
    "O modelo SEM Bets apresentou BIC menor ou igual.\n"
  )
}

if (
  !is.na(
    efeito_bets$p_valor
  ) &&
  efeito_bets$p_valor <
  0.05
) {
  
  cat(
    "O efeito acumulado das Bets é estatisticamente significativo a 5%.\n"
  )
  
} else {
  
  cat(
    "O efeito acumulado das Bets não é estatisticamente significativo a 5%.\n"
  )
}

cat(
  "\nIMPORTANTE:\n",
  "- A inadimplência PF geral foi importada de arquivo próprio.\n",
  "- Por padrão ela é benchmark, não regressora.\n",
  "- Para incluí-la nos dois modelos, altere:\n",
  "  incluir_inad_geral_no_modelo <- TRUE\n",
  sep = ""
)

cat(
  "\nArquivos exportados para:\n",
  normalizePath(
    dir_saida
  ),
  "\n"
)

cat("\nFim.\n")
