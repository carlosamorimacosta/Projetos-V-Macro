# ============================================================================
# SFN – INADIMPLÊNCIA PF ATÉ 10 SALÁRIOS MÍNIMOS
# ARDL COM E SEM BETS – COMPARAÇÃO DE 6 MODALIDADES DE JUROS
#
# ADAPTAÇÃO DO CÓDIGO ORIGINAL:
# - remove o spread livre PF das regressões;
# - mantém a Selic como variável de política monetária;
# - importa do CSV BCB seis modalidades de juros:
#     1) crédito pessoal não consignado;
#     2) cartão de crédito total;
#     3) cartão de crédito rotativo;
#     4) cheque especial;
#     5) crédito pessoal consignado;
#     6) aquisição de veículos;
# - converte as taxas do CSV de % a.m. para equivalente anual % a.a.;
# - estima cada modalidade SEPARADAMENTE, sempre junto com a Selic;
# - para cada modalidade estima um modelo sem Bets e outro com Bets;
# - preserva seleção ARDL, diagnósticos, HAC, VIF, Bounds e ECM.
# ============================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(2026)

# ============================================================================
# 0. CONFIGURAÇÕES
# ============================================================================

data_inicio_desejado <- as.Date("2020-01-01")
data_fim_desejado    <- as.Date("2026-07-31")

# ============================================================================
# >>> CAMINHOS DOS ARQUIVOS <<<
# ============================================================================

arquivo_inad_10sm <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplência 10sm/Base_Final_Inadimplencia_PF.xlsx"
arquivo_inad_geral <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplencia de crédito - pesssoa física - SGS.csv"

# O antigo spread NÃO entra mais como regressora.
# Este arquivo é mantido APENAS como fonte da Selic média mensal já pronta.
arquivo_selic <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Spread_Mensal_Credito_Livre_PF.csv"

# NOVA FONTE DAS TAXAS POR MODALIDADE
arquivo_juros_modalidades <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros/Taxas médias das op de crédito livre - modalidades - completa.csv"

arquivo_ipca <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/ipca_202606SerieHist.xls"
arquivo_desemprego <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Desemprego Pnad.csv"
arquivo_renda <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Renda/rendimento médio pnad.csv"
arquivo_bets <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Bets/Bets_GGR_Mensal_2021_2025_estimado.xlsx"

aba_inad_10sm <- "Base Mensal"
aba_ipca <- 1
aba_bets <- "Base_mensal"

incluir_inad_geral_no_modelo <- FALSE

# p continua até 6. Para q, uso 4 por padrão porque agora serão rodadas
# 6 especificações de juros × 2 modelos (com/sem Bets).
# Como as regressoras entram como X_L1, q=4 representa lags originais t-1...t-5.
# Se quiser reproduzir exatamente o teto antigo, altere max_q para 6.
max_p <- 6
max_q <- 4
min_obs_por_coef <- 4
lag_diagnostico <- 12

# IMPORTANTE:
# As 6 taxas serão avaliadas em especificações SEPARADAS.
# Isso evita explosão combinatória na busca ARDL e reduz multicolinearidade.
rodar_robustez_contemporanea <- TRUE

usar_bounds_exato <- FALSE
R_bounds_exato <- 40000

dir_saida <- "C:/Users/carlo/Downloads/output_SFN_modelos_modalidades_juros"
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
  "openxlsx",
  "readxl"
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
    return(floor_date(x, "month"))
  }
  
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(floor_date(as.Date(x), "month"))
  }
  
  if (is.numeric(x)) {
    
    xx <- as.numeric(x)
    
    med <- suppressWarnings(
      median(xx, na.rm = TRUE)
    )
    
    # Serial Excel
    if (
      is.finite(med) &&
      med > 20000 &&
      med < 80000
    ) {
      
      return(
        floor_date(
          as.Date(xx, origin = "1899-12-30"),
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
      
      s <- sprintf("%06d", as.integer(xx))
      
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
  
  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "NaN")] <- NA_character_
  
  out <- rep(as.Date(NA), length(s))
  
  # YYYY-MM
  idx1 <- grepl("^\\d{4}[-/]\\d{1,2}$", s)
  
  if (any(idx1, na.rm = TRUE)) {
    ss <- gsub("/", "-", s[idx1])
    out[idx1] <- as.Date(paste0(ss, "-01"))
  }
  
  # MM/YYYY
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
  
  # YYYYMM
  idx3 <- is.na(out) & grepl("^\\d{6}$", s)
  
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
  idx4 <- is.na(out) & !is.na(s)
  
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
  
  floor_date(out, "month")
}

mes_pt_numero <- function(x) {
  
  s <- iconv(
    tolower(trimws(as.character(x))),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  # Caso já seja número 1-12
  num <- suppressWarnings(as.integer(s))
  out <- ifelse(
    !is.na(num) & num >= 1 & num <= 12,
    num,
    NA_integer_
  )
  
  chave <- substr(s, 1, 3)
  
  mapa <- c(
    jan = 1,
    fev = 2,
    mar = 3,
    abr = 4,
    mai = 5,
    jun = 6,
    jul = 7,
    ago = 8,
    set = 9,
    out = 10,
    nov = 11,
    dez = 12
  )
  
  idx <- is.na(out) & chave %in% names(mapa)
  
  out[idx] <- unname(
    mapa[
      chave[idx]
    ]
  )
  
  as.integer(out)
}

validar_serie_mensal <- function(df, nome) {
  
  if (!all(c("data", nome) %in% names(df))) {
    stop(
      "Estrutura inválida para a série ",
      nome,
      "."
    )
  }
  
  df <- df %>%
    arrange(data)
  
  if (anyDuplicated(df$data)) {
    
    dup <- df %>%
      count(data) %>%
      filter(n > 1)
    
    print(dup)
    
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

# Detecta a codificação real dos CSVs.
# Arquivos do SGS/BCB frequentemente vêm em Windows-1252/ISO-8859-1,
# enquanto outras bases podem estar em UTF-8.
detectar_encoding_csv <- function(caminho) {
  
  enc <- tryCatch(
    readr::guess_encoding(
      caminho,
      n_max = 1000
    ),
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
      locale = readr::locale(
        encoding = encoding_usar
      ),
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
    
    n_pv <- contar(";")
    n_vg <- contar(",")
    n_tab <- contar("\t")
    
    delim <- if (
      n_tab >= max(n_pv, n_vg) && n_tab > 0
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
  
  nomes <- names(df)
  
  if (
    is.null(nomes) ||
    length(nomes) == 0 ||
    all(is.na(nomes) | nomes == "")
  ) {
    stop(
      "\nNão foi possível ler corretamente o cabeçalho de: ",
      basename(caminho),
      "\nVerifique a codificação e o delimitador do arquivo."
    )
  }
  
  names(df) <- normalizar_nome(nomes)
  
  as.data.frame(df)
}

encontrar_coluna <- function(
    df,
    alternativas,
    nome_logico
) {
  
  alternativas <- normalizar_nome(alternativas)
  
  # 1) Tentativa por nome exato
  achou <- intersect(
    alternativas,
    names(df)
  )
  
  if (length(achou) > 0) {
    return(achou[1])
  }
  
  # 2) Tentativa ignorando código numérico no início do cabeçalho SGS.
  # Ex.: 21084_inadimplencia_da_carteira... -> inadimplencia_da_carteira...
  nomes_sem_codigo <- sub(
    "^[0-9]+_",
    "",
    names(df)
  )
  
  for (alt in alternativas) {
    idx <- which(nomes_sem_codigo == alt)
    if (length(idx) == 1) {
      return(names(df)[idx])
    }
  }
  
  # 3) Fallback por conteúdo do nome.
  # Útil para cabeçalhos longos que incluem código, unidade ou descrição adicional.
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
        data_inicio_desejado,
        "month"
      ),
      data <= floor_date(
        data_fim_desejado,
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


# ----------------------------------------------------------------------------
# TAXAS DE JUROS POR MODALIDADE – arquivo BCB/SGS em % a.m.
#
# O CSV fornecido contém:
#   25463 = Cheque especial
#   25464 = Crédito pessoal não consignado total
#   25469 = Crédito pessoal consignado total
#   25471 = Aquisição de veículos
#   25477 = Cartão de crédito rotativo
#   25479 = Cartão de crédito total
#
# Como a Selic está em % a.a., as taxas mensais são convertidas para
# equivalente anual por composição:
#   i_aa = 100 * ((1 + i_am/100)^12 - 1)
#
# IMPORTANTE:
# 25464 é a versão mensal (% a.m.) da modalidade "crédito pessoal não
# consignado total". O resultado anualizado abaixo é um equivalente anual
# calculado a partir do CSV; não é renomeado como a série oficial SGS 20742.
# ----------------------------------------------------------------------------

parse_data_bcb_mmm_aa <- function(x) {
  
  s <- iconv(
    tolower(trimws(as.character(x))),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  out <- rep(as.Date(NA), length(s))
  
  ok <- grepl(
    "^[a-z]{3}/[0-9]{2}$",
    s
  )
  
  if (any(ok)) {
    
    partes <- strsplit(
      s[ok],
      "/",
      fixed = TRUE
    )
    
    mes_txt <- vapply(
      partes,
      `[`,
      character(1),
      1
    )
    
    ano_2d <- suppressWarnings(
      as.integer(
        vapply(
          partes,
          `[`,
          character(1),
          2
        )
      )
    )
    
    mes_num <- mes_pt_numero(
      mes_txt
    )
    
    # Os arquivos BCB desta família usam anos 2000+ no período relevante.
    ano <- 2000 + ano_2d
    
    validos <- !is.na(mes_num) &
      !is.na(ano)
    
    tmp <- rep(
      as.Date(NA),
      length(mes_num)
    )
    
    tmp[validos] <- as.Date(
      sprintf(
        "%04d-%02d-01",
        ano[validos],
        mes_num[validos]
      )
    )
    
    out[ok] <- tmp
  }
  
  # Fallback para qualquer data que já venha em outro formato reconhecível
  faltou <- is.na(out) & !is.na(s) & s != ""
  
  if (any(faltou)) {
    out[faltou] <- parse_data_mensal(
      s[faltou]
    )
  }
  
  out
}

anualizar_taxa_mensal <- function(x) {
  100 * (
    (
      1 + x / 100
    )^12 -
      1
  )
}

encontrar_coluna_por_codigo_sgs <- function(
    df,
    codigo
) {
  
  padrao <- paste0(
    "^",
    codigo,
    "_"
  )
  
  idx <- grep(
    padrao,
    names(df)
  )
  
  if (length(idx) != 1) {
    stop(
      "\nNão encontrei exatamente uma coluna para o SGS ",
      codigo,
      ".\nColunas encontradas: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  names(df)[idx]
}

ler_juros_modalidades <- function(
    caminho
) {
  
  raw <- ler_arquivo_generico(
    caminho,
    1
  )
  
  col_data <- encontrar_coluna(
    raw,
    c(
      "data",
      "date",
      "mes",
      "mes_ano",
      "competencia",
      "periodo"
    ),
    "data das taxas de juros"
  )
  
  col_cheque <- encontrar_coluna_por_codigo_sgs(
    raw,
    25463
  )
  
  col_nao_consignado <- encontrar_coluna_por_codigo_sgs(
    raw,
    25464
  )
  
  col_consignado <- encontrar_coluna_por_codigo_sgs(
    raw,
    25469
  )
  
  col_veiculos <- encontrar_coluna_por_codigo_sgs(
    raw,
    25471
  )
  
  col_rotativo <- encontrar_coluna_por_codigo_sgs(
    raw,
    25477
  )
  
  col_cartao_total <- encontrar_coluna_por_codigo_sgs(
    raw,
    25479
  )
  
  out <- raw %>%
    transmute(
      data = parse_data_bcb_mmm_aa(
        .data[[col_data]]
      ),
      
      cheque_especial_am = parse_numero(
        .data[[col_cheque]]
      ),
      
      juros_nao_consignado_am = parse_numero(
        .data[[col_nao_consignado]]
      ),
      
      juros_consignado_am = parse_numero(
        .data[[col_consignado]]
      ),
      
      juros_veiculos_am = parse_numero(
        .data[[col_veiculos]]
      ),
      
      juros_cartao_rotativo_am = parse_numero(
        .data[[col_rotativo]]
      ),
      
      juros_cartao_total_am = parse_numero(
        .data[[col_cartao_total]]
      )
    ) %>%
    filter(
      !is.na(data),
      data >= floor_date(
        data_inicio_desejado,
        "month"
      ),
      data <= floor_date(
        data_fim_desejado,
        "month"
      )
    ) %>%
    mutate(
      cheque_especial =
        anualizar_taxa_mensal(
          cheque_especial_am
        ),
      
      juros_nao_consignado =
        anualizar_taxa_mensal(
          juros_nao_consignado_am
        ),
      
      juros_consignado =
        anualizar_taxa_mensal(
          juros_consignado_am
        ),
      
      juros_veiculos =
        anualizar_taxa_mensal(
          juros_veiculos_am
        ),
      
      juros_cartao_rotativo =
        anualizar_taxa_mensal(
          juros_cartao_rotativo_am
        ),
      
      juros_cartao_total =
        anualizar_taxa_mensal(
          juros_cartao_total_am
        )
    ) %>%
    select(
      data,
      cheque_especial,
      juros_nao_consignado,
      juros_consignado,
      juros_veiculos,
      juros_cartao_rotativo,
      juros_cartao_total
    ) %>%
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    arrange(data)
  
  if (nrow(out) == 0) {
    stop(
      "\nNenhuma observação válida das taxas por modalidade foi encontrada."
    )
  }
  
  if (anyDuplicated(out$data)) {
    stop(
      "\nHá mais de uma observação por mês na base de modalidades de juros."
    )
  }
  
  cols_taxas <- setdiff(
    names(out),
    "data"
  )
  
  if (
    any(
      vapply(
        out[cols_taxas],
        function(x) all(is.na(x)),
        logical(1)
      )
    )
  ) {
    stop(
      "\nPelo menos uma das seis taxas foi importada somente com NA."
    )
  }
  
  message(
    "Taxas por modalidade importadas corretamente: ",
    nrow(out),
    " observações de ",
    format(min(out$data), "%Y-%m"),
    " a ",
    format(max(out$data), "%Y-%m"),
    "."
  )
  
  message(
    "As taxas do CSV (% a.m.) foram convertidas para equivalente anual (% a.a.)."
  )
  
  out
}

# ----------------------------------------------------------------------------
# IPCA – arquivo histórico IBGE .xls
# Extrai a coluna "NO MÊS" da tabela de série histórica.
# ----------------------------------------------------------------------------

ler_ipca_ibge <- function(
    caminho,
    aba = 1
) {
  
  if (!file.exists(caminho)) {
    stop(
      "Arquivo IPCA não encontrado: ",
      caminho
    )
  }
  
  # --------------------------------------------------------------------------
  # Estrutura REAL do arquivo IBGE "Série Histórica IPCA":
  #
  #   Coluna A = ANO
  #   Coluna B = MÊS
  #   Coluna C = NÚMERO ÍNDICE
  #   Coluna D = VARIAÇÃO (%) NO MÊS  <-- variável utilizada no modelo
  #
  # O cabeçalho "NO MÊS" é dividido em duas linhas:
  #   linha 6: "NO"
  #   linha 7: "MÊS"
  #
  # Além disso, o ano aparece apenas em JAN de cada ano e precisa ser
  # propagado para FEV...DEZ.
  # --------------------------------------------------------------------------
  
  raw <- readxl::read_excel(
    caminho,
    sheet = aba,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  
  raw <- as.data.frame(
    raw,
    check.names = FALSE
  )
  
  if (
    ncol(raw) < 4
  ) {
    stop(
      "
O arquivo histórico do IPCA possui menos de 4 colunas.
",
      "Era esperado: A=ANO, B=MÊS, C=NÚMERO ÍNDICE e D=NO MÊS."
    )
  }
  
  # --------------------------------------------------------------------------
  # Validação simples da estrutura do arquivo.
  # Procura ANO e MÊS nas primeiras linhas para evitar extrair uma aba errada.
  # --------------------------------------------------------------------------
  
  primeiras_linhas <- raw[
    seq_len(
      min(
        15,
        nrow(raw)
      )
    ),
    seq_len(
      min(
        8,
        ncol(raw)
      )
    ),
    drop = FALSE
  ]
  
  cab_texto <- normalizar_nome(
    as.character(
      unlist(
        primeiras_linhas,
        use.names = FALSE
      )
    )
  )
  
  tem_ano <- any(
    cab_texto == "ano",
    na.rm = TRUE
  )
  
  tem_mes <- any(
    cab_texto == "mes",
    na.rm = TRUE
  )
  
  if (
    !tem_ano ||
    !tem_mes
  ) {
    stop(
      "
A aba selecionada não parece ser a Série Histórica do IPCA.
",
      "Não encontrei os cabeçalhos ANO e MÊS nas primeiras linhas.
",
      "Aba utilizada: ",
      aba
    )
  }
  
  # --------------------------------------------------------------------------
  # ANO
  # No arquivo do IBGE o ano só aparece na primeira observação de cada ano.
  # Ex.:
  #   2025 | JAN
  #        | FEV
  #        | MAR
  # Portanto, fazemos preenchimento para baixo.
  # --------------------------------------------------------------------------
  
  ano_raw <- suppressWarnings(
    parse_numero(
      raw[[1]]
    )
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
  
  # --------------------------------------------------------------------------
  # MÊS
  # JAN, FEV, MAR, ABR, MAI, JUN, JUL, AGO, SET, OUT, NOV, DEZ
  # --------------------------------------------------------------------------
  
  mes <- mes_pt_numero(
    raw[[2]]
  )
  
  # --------------------------------------------------------------------------
  # IPCA
  # Coluna D = variação percentual "NO MÊS".
  # NÃO usamos número índice, acumulado no ano, 3, 6 ou 12 meses.
  # --------------------------------------------------------------------------
  
  ipca <- suppressWarnings(
    parse_numero(
      raw[[4]]
    )
  )
  
  # --------------------------------------------------------------------------
  # Cria a data mensal somente para linhas que são observações válidas.
  # Linhas de cabeçalho, rodapé e separadores são descartadas.
  # --------------------------------------------------------------------------
  
  valido <- !is.na(ano) &
    !is.na(mes) &
    mes >= 1 &
    mes <= 12 &
    !is.na(ipca)
  
  if (
    sum(valido) == 0
  ) {
    stop(
      "
Nenhuma observação mensal válida do IPCA foi encontrada.
",
      "Esperava encontrar ano na coluna A, mês na coluna B e ",
      "variação mensal na coluna D."
    )
  }
  
  datas <- as.Date(
    sprintf(
      "%04d-%02d-01",
      ano[valido],
      mes[valido]
    )
  )
  
  out <- tibble(
    data = datas,
    ipca = as.numeric(
      ipca[valido]
    )
  ) %>%
    filter(
      data >= floor_date(
        data_inicio_desejado,
        "month"
      ),
      data <= floor_date(
        data_fim_desejado,
        "month"
      )
    ) %>%
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    arrange(data)
  
  if (
    nrow(out) == 0
  ) {
    stop(
      "
O IPCA foi identificado, mas não há observações dentro do período ",
      format(data_inicio_desejado, "%Y-%m"),
      " a ",
      format(data_fim_desejado, "%Y-%m"),
      "."
    )
  }
  
  message(
    "IPCA importado corretamente: ",
    nrow(out),
    " observações de ",
    format(min(out$data), "%Y-%m"),
    " a ",
    format(max(out$data), "%Y-%m"),
    "."
  )
  
  message(
    "Primeiro IPCA: ",
    format(min(out$data), "%Y-%m"),
    " = ",
    out$ipca[which.min(out$data)],
    "%"
  )
  
  message(
    "Último IPCA: ",
    format(max(out$data), "%Y-%m"),
    " = ",
    out$ipca[which.max(out$data)],
    "%"
  )
  
  validar_serie_mensal(
    out,
    "ipca"
  )
}

# ----------------------------------------------------------------------------
# PNAD Contínua – taxa de desocupação
#
# O CSV fornecido é horizontal:
# linha com períodos: jan-fev-mar 2012; fev-mar-abr 2012; ...
# linha "Brasil":      8; 7.8; ...
#
# Cada trimestre móvel é datado pelo ÚLTIMO mês do trimestre.
# Ex.: jan-fev-mar 2020 -> 2020-03-01.
# ----------------------------------------------------------------------------

parse_periodo_pnad <- function(periodo) {
  
  s <- iconv(
    tolower(trimws(as.character(periodo))),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  ano <- suppressWarnings(
    as.integer(
      sub(
        ".*\\s([0-9]{4})$",
        "\\1",
        s
      )
    )
  )
  
  ultimo_mes <- sub(
    ".*-([a-z]+)\\s[0-9]{4}$",
    "\\1",
    s
  )
  
  mes <- mes_pt_numero(
    ultimo_mes
  )
  
  ok <- !is.na(ano) &
    !is.na(mes)
  
  out <- rep(
    as.Date(NA),
    length(s)
  )
  
  out[ok] <- as.Date(
    sprintf(
      "%04d-%02d-01",
      ano[ok],
      mes[ok]
    )
  )
  
  out
}

ler_desemprego_pnad <- function(
    caminho
) {
  
  if (!file.exists(caminho)) {
    stop("Arquivo de desemprego não encontrado: ", caminho)
  }
  
  linhas <- readr::read_lines(
    caminho,
    progress = FALSE
  )
  
  i_brasil <- which(
    grepl(
      "^\\s*Brasil\\s*;",
      linhas,
      ignore.case = TRUE
    )
  )[1]
  
  if (is.na(i_brasil) || i_brasil <= 1) {
    stop(
      "Não encontrei a linha Brasil no CSV da PNAD de desemprego."
    )
  }
  
  periodo_raw <- strsplit(
    linhas[i_brasil - 1],
    ";",
    fixed = TRUE
  )[[1]]
  
  valor_raw <- strsplit(
    linhas[i_brasil],
    ";",
    fixed = TRUE
  )[[1]]
  
  n <- min(
    length(periodo_raw),
    length(valor_raw)
  )
  
  periodo_raw <- periodo_raw[
    2:n
  ]
  
  valor_raw <- valor_raw[
    2:n
  ]
  
  out <- tibble(
    data = parse_periodo_pnad(
      periodo_raw
    ),
    desemprego = parse_numero(
      valor_raw
    )
  ) %>%
    filter(
      !is.na(data),
      data >= floor_date(
        data_inicio_desejado,
        "month"
      ),
      data <= floor_date(
        data_fim_desejado,
        "month"
      )
    ) %>%
    arrange(data)
  
  validar_serie_mensal(
    out,
    "desemprego"
  )
}

# ----------------------------------------------------------------------------
# PNAD Contínua – rendimento médio em trimestre móvel
#
# Arquivo:
#   rendimento médio pnad.csv
#
# ESTRUTURA REAL DO CSV FORNECIDO:
#   linha 1 = título ("Rendimento médio")
#   linha 2 = períodos, separados por ";"
#             ex.: jan-fev-mar 2012; fev-mar-abr 2012; ...
#   linha 3 = "Brasil" + valores, separados por ";"
#   linha 4 = vazia
#   linha 5 = fonte
#
# CONVENÇÃO TEMPORAL:
# Cada trimestre móvel é atribuído ao ÚLTIMO mês da janela.
#
# Exemplos:
#   nov-dez-jan 2021 -> 2021-01
#   dez-jan-fev 2021 -> 2021-02
#   jan-fev-mar 2021 -> 2021-03
#   fev-mar-abr 2021 -> 2021-04
#
# Essa convenção evita look-ahead bias:
# NÃO atribuímos "jan-fev-mar 2021" a janeiro, pois isso colocaria
# em janeiro informações de fevereiro e março.
#
# A série é um trimestre móvel sobreposto, não uma observação mensal "pura",
# mas é atualizada mensalmente e é muito superior à repetição de um único
# valor anual nos 12 meses.
# ----------------------------------------------------------------------------

ler_renda_pnad_movel <- function(
    caminho
) {
  
  if (!file.exists(caminho)) {
    stop(
      "Arquivo de rendimento médio PNAD não encontrado: ",
      caminho
    )
  }
  
  encoding_usar <- detectar_encoding_csv(
    caminho
  )
  
  # --------------------------------------------------------------------------
  # IMPORTANTE:
  # Não usamos read_delim() diretamente porque a primeira linha do arquivo
  # contém apenas o título "Rendimento médio", enquanto a linha seguinte
  # contém mais de 170 campos. Isso faz leitores tabulares tentarem inferir
  # uma largura errada para o arquivo.
  #
  # Por isso, a leitura é feita linha a linha, exatamente como no leitor
  # de desemprego da PNAD.
  # --------------------------------------------------------------------------
  
  linhas <- readr::read_lines(
    caminho,
    locale = readr::locale(
      encoding = encoding_usar
    ),
    progress = FALSE
  )
  
  # Localiza a linha do Brasil de forma robusta, em vez de assumir
  # rigidamente que será sempre a linha 3.
  i_brasil <- which(
    grepl(
      "^\\s*Brasil\\s*;",
      linhas,
      ignore.case = TRUE
    )
  )[1]
  
  if (
    is.na(i_brasil) ||
    i_brasil <= 1
  ) {
    stop(
      "\nNão encontrei a linha 'Brasil' no arquivo de rendimento médio PNAD.\n",
      "Estrutura esperada: uma linha de períodos imediatamente antes ",
      "da linha Brasil."
    )
  }
  
  # A linha imediatamente anterior à linha Brasil contém os períodos.
  linha_periodos <- linhas[
    i_brasil - 1
  ]
  
  linha_valores <- linhas[
    i_brasil
  ]
  
  periodo_raw <- strsplit(
    linha_periodos,
    ";",
    fixed = TRUE
  )[[1]]
  
  valor_raw <- strsplit(
    linha_valores,
    ";",
    fixed = TRUE
  )[[1]]
  
  # Remove a primeira célula:
  #   períodos -> primeira célula vazia
  #   valores  -> primeira célula = "Brasil"
  if (length(periodo_raw) < 2 || length(valor_raw) < 2) {
    stop(
      "\nNão foi possível separar os períodos e valores da renda PNAD."
    )
  }
  
  periodo_raw <- periodo_raw[-1]
  valor_raw <- valor_raw[-1]
  
  # Remove eventuais espaços extras
  periodo_raw <- trimws(
    periodo_raw
  )
  
  valor_raw <- trimws(
    valor_raw
  )
  
  # As duas linhas precisam ter exatamente o mesmo número de observações.
  if (
    length(periodo_raw) != length(valor_raw)
  ) {
    stop(
      "\nNúmero de períodos diferente do número de valores na renda PNAD.\n",
      "Períodos encontrados: ",
      length(periodo_raw),
      "\nValores encontrados: ",
      length(valor_raw)
    )
  }
  
  if (length(periodo_raw) == 0) {
    stop(
      "\nNenhuma observação de rendimento médio foi encontrada."
    )
  }
  
  # --------------------------------------------------------------------------
  # parse_periodo_pnad() usa o ÚLTIMO mês informado no trimestre móvel:
  #
  #   jan-fev-mar 2021 -> mês = mar -> 2021-03-01
  #   fev-mar-abr 2021 -> mês = abr -> 2021-04-01
  #
  # Portanto não há uso de informação futura.
  # --------------------------------------------------------------------------
  
  out_com_periodo <- tibble(
    periodo_original = periodo_raw,
    data = parse_periodo_pnad(
      periodo_raw
    ),
    renda = parse_numero(
      valor_raw
    )
  )
  
  # Diagnóstico explícito caso algum período não seja reconhecido.
  periodos_nao_lidos <- out_com_periodo %>%
    filter(
      is.na(data) &
        !is.na(periodo_original) &
        periodo_original != ""
    )
  
  if (nrow(periodos_nao_lidos) > 0) {
    warning(
      "\nAlguns períodos da renda PNAD não puderam ser convertidos em data:\n",
      paste(
        head(
          periodos_nao_lidos$periodo_original,
          10
        ),
        collapse = ", "
      )
    )
  }
  
  out <- out_com_periodo %>%
    filter(
      !is.na(data),
      !is.na(renda),
      data >= floor_date(
        data_inicio_desejado,
        "month"
      ),
      data <= floor_date(
        data_fim_desejado,
        "month"
      )
    ) %>%
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    arrange(data) %>%
    select(
      data,
      renda
    )
  
  out <- validar_serie_mensal(
    out,
    "renda"
  )
  
  if (nrow(out) == 0) {
    stop(
      "\nA série de rendimento médio PNAD foi identificada, ",
      "mas não há observações dentro do período desejado."
    )
  }
  
  # --------------------------------------------------------------------------
  # Verifica continuidade mensal dentro da cobertura observada.
  # --------------------------------------------------------------------------
  
  calendario_renda <- seq.Date(
    min(out$data),
    max(out$data),
    by = "month"
  )
  
  meses_faltantes <- setdiff(
    calendario_renda,
    out$data
  )
  
  if (length(meses_faltantes) > 0) {
    warning(
      "\nA série de rendimento médio PNAD possui meses faltantes entre ",
      format(min(out$data), "%Y-%m"),
      " e ",
      format(max(out$data), "%Y-%m"),
      ".\nMeses ausentes: ",
      paste(
        format(
          as.Date(
            meses_faltantes,
            origin = "1970-01-01"
          ),
          "%Y-%m"
        ),
        collapse = ", "
      )
    )
  }
  
  message(
    "Renda PNAD importada corretamente: ",
    nrow(out),
    " observações mensais/trimestres móveis de ",
    format(min(out$data), "%Y-%m"),
    " a ",
    format(max(out$data), "%Y-%m"),
    "."
  )
  
  message(
    "Convenção temporal da renda: trimestre móvel atribuído ao ÚLTIMO mês."
  )
  
  message(
    "Exemplos: nov-dez-jan 2021 -> 2021-01; ",
    "dez-jan-fev 2021 -> 2021-02; ",
    "jan-fev-mar 2021 -> 2021-03."
  )
  
  message(
    "Total de períodos brutos encontrados no CSV: ",
    length(periodo_raw),
    "."
  )
  
  out
}

# ----------------------------------------------------------------------------
# Bets – GGR mensal estimado Brasil
#
# Arquivo:
#   Bets_GGR_Mensal_2021_2025_estimado.xlsx
#
# Aba:
#   Base_mensal
#
# Estrutura da aba:
#   linha 1 = título
#   linha 2 = vazia
#   linha 3 = cabeçalho
#
# Colunas relevantes:
#   Data
#   GGR_bets_R$bi
#
# A função abaixo lê DIRETAMENTE os valores mensais da planilha.
# Portanto:
#   - NÃO divide GGR anual por 12;
#   - NÃO repete um mesmo valor dentro do ano;
#   - NÃO altera a especificação do ARDL;
#   - a variável final continua se chamando "bets".
#
# Observação metodológica da própria base:
#   2021–2022 = estimados por sazonalização;
#   2023      = estimado com indicador mensal BCB;
#   2024–2025 = valores mensais publicados pela H2.
# ----------------------------------------------------------------------------

ler_bets_ggr_mensal <- function(
    caminho,
    aba = "Base_mensal"
) {
  
  if (!file.exists(caminho)) {
    stop(
      "Arquivo mensal de bets não encontrado: ",
      caminho
    )
  }
  
  # A linha 3 contém o cabeçalho; por isso pulamos as duas primeiras linhas.
  raw <- readxl::read_excel(
    caminho,
    sheet = aba,
    skip = 2,
    .name_repair = "unique"
  )
  
  raw <- as.data.frame(
    raw,
    check.names = FALSE
  )
  
  names(raw) <- normalizar_nome(
    names(raw)
  )
  
  col_data <- encontrar_coluna(
    raw,
    c(
      "data",
      "date",
      "mes_ano",
      "competencia",
      "periodo"
    ),
    "data da série mensal de Bets"
  )
  
  # "GGR_bets_R$bi" é normalizado para "ggr_bets_r_bi".
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
        data_inicio_desejado,
        "month"
      ),
      data <= floor_date(
        data_fim_desejado,
        "month"
      )
    ) %>%
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    arrange(data)
  
  out <- validar_serie_mensal(
    out,
    "bets"
  )
  
  # A base fornecida deve cobrir 2021M01–2025M12.
  inicio_esperado <- max(
    floor_date(
      data_inicio_desejado,
      "month"
    ),
    as.Date("2021-01-01")
  )
  
  fim_esperado <- min(
    floor_date(
      data_fim_desejado,
      "month"
    ),
    as.Date("2025-12-01")
  )
  
  datas_esperadas <- seq.Date(
    inicio_esperado,
    fim_esperado,
    by = "month"
  )
  
  datas_faltantes <- setdiff(
    datas_esperadas,
    out$data
  )
  
  if (length(datas_faltantes) > 0) {
    stop(
      "\nA base mensal de Bets possui meses faltantes.\n",
      "Meses ausentes: ",
      paste(
        format(
          as.Date(datas_faltantes, origin = "1970-01-01"),
          "%Y-%m"
        ),
        collapse = ", "
      )
    )
  }
  
  message(
    "Bets importadas corretamente: ",
    nrow(out),
    " observações mensais de ",
    format(min(out$data), "%Y-%m"),
    " a ",
    format(max(out$data), "%Y-%m"),
    "."
  )
  
  message(
    "Unidade da variável Bets: GGR mensal estimado, em R$ bilhões."
  )
  
  out
}


# ============================================================================
# 3. IMPORTAÇÃO DAS SÉRIES
# ============================================================================

message("\n============================================================")
message("1. IMPORTANDO AS SÉRIES")
message("============================================================")

# 1) Inadimplência PF até 10 SM
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

# 2) Inadimplência PF geral – SGS
inad_geral <- ler_serie_unica(
  arquivo_inad_geral,
  1,
  "inad_pf_geral",
  c(
    "inadimplencia_da_carteira_de_credito_pessoas_fisicas_total",
    "inadimplencia_pf_total",
    "inadimplencia",
    "valor"
  )
)

# 3) Selic mensal média – mantida do arquivo que já estava funcionando
selic_df <- ler_serie_unica(
  arquivo_selic,
  1,
  "selic",
  c(
    "selic_media_mensal_pct_aa",
    "selic_media_mensal",
    "selic_meta",
    "selic"
  )
)

# 4) NOVO: seis taxas de juros por modalidade
juros_modalidades_df <- ler_juros_modalidades(
  arquivo_juros_modalidades
)

# 5) IPCA
ipca_df <- ler_ipca_ibge(
  arquivo_ipca,
  aba_ipca
)

# 6) Desemprego
desemprego_df <- ler_desemprego_pnad(
  arquivo_desemprego
)

# 7) Renda
renda_df <- ler_renda_pnad_movel(
  arquivo_renda
)

# 8) Bets
bets_df <- ler_bets_ggr_mensal(
  arquivo_bets,
  aba_bets
)

# ============================================================================
# 4. DEFINIÇÃO DA AMOSTRA COMUM E CONSTRUÇÃO DA BASE FINAL
# ============================================================================

message("\n============================================================")
message("2. DEFININDO AMOSTRA COMUM")
message("============================================================")

cobertura_series <- bind_rows(
  tibble(
    variavel = "inad_pf_10sm",
    inicio = min(inad_10sm$data, na.rm = TRUE),
    fim = max(inad_10sm$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "inad_pf_geral",
    inicio = min(inad_geral$data, na.rm = TRUE),
    fim = max(inad_geral$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "selic",
    inicio = min(selic_df$data, na.rm = TRUE),
    fim = max(selic_df$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "juros_modalidades",
    inicio = min(juros_modalidades_df$data, na.rm = TRUE),
    fim = max(juros_modalidades_df$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "ipca",
    inicio = min(ipca_df$data, na.rm = TRUE),
    fim = max(ipca_df$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "desemprego",
    inicio = min(desemprego_df$data, na.rm = TRUE),
    fim = max(desemprego_df$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "renda",
    inicio = min(renda_df$data, na.rm = TRUE),
    fim = max(renda_df$data, na.rm = TRUE)
  ),
  tibble(
    variavel = "bets",
    inicio = min(bets_df$data, na.rm = TRUE),
    fim = max(bets_df$data, na.rm = TRUE)
  )
)

print(cobertura_series)

data_inicio <- max(
  c(
    floor_date(
      data_inicio_desejado,
      "month"
    ),
    cobertura_series$inicio
  )
)

data_fim <- min(
  c(
    floor_date(
      data_fim_desejado,
      "month"
    ),
    cobertura_series$fim
  )
)

if (
  !is.finite(as.numeric(data_inicio)) ||
  !is.finite(as.numeric(data_fim)) ||
  data_inicio > data_fim
) {
  stop(
    "Não existe interseção temporal válida entre todas as séries."
  )
}

message(
  "\nAmostra comum efetiva: ",
  format(data_inicio, "%Y-%m"),
  " até ",
  format(data_fim, "%Y-%m")
)

calendario <- tibble(
  data = seq.Date(
    data_inicio,
    data_fim,
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
    juros_modalidades_df,
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
  arrange(data) %>%
  mutate(
    gap_inad_10sm_geral =
      inad_pf_10sm -
      inad_pf_geral
  )

vars_taxas <- c(
  "juros_nao_consignado",
  "juros_cartao_total",
  "juros_cartao_rotativo",
  "cheque_especial",
  "juros_consignado",
  "juros_veiculos"
)

vars_essenciais <- c(
  "inad_pf_10sm",
  "inad_pf_geral",
  "selic",
  vars_taxas,
  "ipca",
  "desemprego",
  "renda",
  "bets"
)

faltantes <- base_final %>%
  filter(
    if_any(
      all_of(vars_essenciais),
      is.na
    )
  )

if (nrow(faltantes) > 0) {
  
  message(
    "\nForam encontrados meses com dados ausentes dentro da amostra comum:"
  )
  
  print(
    faltantes %>%
      select(
        data,
        all_of(vars_essenciais)
      )
  )
  
  stop(
    "\nHá lacunas internas nas séries dentro da amostra comum.\n",
    "O script não interpola, não preenche com zero e não usa informação futura."
  )
}

message("\nPrimeiras observações:")
print(head(base_final))

message("\nÚltimas observações:")
print(tail(base_final))

message("\nResumo:")
print(summary(base_final))

periodo_tag <- paste0(
  format(data_inicio, "%YM%m"),
  "_",
  format(data_fim, "%YM%m")
)

readr::write_csv(
  base_final,
  file.path(
    dir_saida,
    paste0(
      "base_final_SFN_modalidades_juros_",
      periodo_tag,
      ".csv"
    )
  )
)

openxlsx::write.xlsx(
  base_final,
  file.path(
    dir_saida,
    paste0(
      "base_final_SFN_modalidades_juros_",
      periodo_tag,
      ".xlsx"
    )
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
      media = mean(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE),
      minimo = min(x, na.rm = TRUE),
      maximo = max(x, na.rm = TRUE),
      variancia = var(x, na.rm = TRUE),
      desvio_padrao = sd(x, na.rm = TRUE)
    )
  }
)

print(descritivas)

mat_cor <- cor(
  base_final %>%
    select(
      all_of(vars_numericas)
    ),
  use = "complete.obs"
)

message("\nMatriz de correlação:")
print(round(mat_cor, 4))

cor_taxas <- cor(
  base_final %>%
    select(
      selic,
      all_of(vars_taxas)
    ),
  use = "complete.obs"
)

message("\nCorrelação entre Selic e modalidades de crédito:")
print(round(cor_taxas, 4))

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
    selic_L1 = lag(selic, 1),
    
    juros_nao_consignado_L1 =
      lag(juros_nao_consignado, 1),
    
    juros_cartao_total_L1 =
      lag(juros_cartao_total, 1),
    
    juros_cartao_rotativo_L1 =
      lag(juros_cartao_rotativo, 1),
    
    cheque_especial_L1 =
      lag(cheque_especial, 1),
    
    juros_consignado_L1 =
      lag(juros_consignado, 1),
    
    juros_veiculos_L1 =
      lag(juros_veiculos, 1),
    
    ipca_L1 = lag(ipca, 1),
    desemprego_L1 = lag(desemprego, 1),
    renda_L1 = lag(renda, 1),
    bets_L1 = lag(bets, 1),
    inad_pf_geral_L1 = lag(inad_pf_geral, 1)
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
  
  ll <- logLik(modelo)
  k <- attr(ll, "df")
  n <- nobs(modelo)
  aic <- AIC(modelo)
  
  if (n - k - 1 <= 0) {
    return(Inf)
  }
  
  as.numeric(
    aic +
      (2 * k * (k + 1)) /
      (n - k - 1)
  )
}

hqic_modelo <- function(
    modelo
) {
  
  ll <- logLik(modelo)
  k <- attr(ll, "df")
  n <- nobs(modelo)
  
  as.numeric(
    -2 * as.numeric(ll) +
      2 * k * log(log(n))
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
      
      out[[
        paste0(
          "q_",
          x_vars[j]
        )
      ]] <- q[j]
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



# ============================================================================
# 22. ESPECIFICAÇÕES POR MODALIDADE DE JUROS
# ============================================================================

modalidades_modelo <- tribble(
  ~taxa_var,                  ~taxa_nome,
  "juros_nao_consignado",     "Crédito pessoal não consignado",
  "juros_cartao_total",       "Cartão de crédito total",
  "juros_cartao_rotativo",    "Cartão de crédito rotativo",
  "cheque_especial",          "Cheque especial",
  "juros_consignado",         "Crédito pessoal consignado",
  "juros_veiculos",           "Aquisição de veículos"
)

slug_modelo <- function(x) {
  x <- normalizar_nome(x)
  gsub("_+", "_", x)
}

salvar_acf_pacf <- function(
    residuos,
    prefixo,
    titulo
) {
  
  png(
    file.path(
      dir_saida,
      paste0(prefixo, "_acf.png")
    ),
    width = 1000,
    height = 650
  )
  
  acf(
    residuos,
    main = paste0(
      "ACF – ",
      titulo
    )
  )
  
  dev.off()
  
  png(
    file.path(
      dir_saida,
      paste0(prefixo, "_pacf.png")
    ),
    width = 1000,
    height = 650
  )
  
  pacf(
    residuos,
    main = paste0(
      "PACF – ",
      titulo
    )
  )
  
  dev.off()
}

rodar_par_modalidade <- function(
    taxa_var,
    taxa_nome
) {
  
  message("\n\n============================================================")
  message("MODALIDADE: ", taxa_nome)
  message("============================================================")
  
  taxa_L1 <- paste0(
    taxa_var,
    "_L1"
  )
  
  x_sem <- c(
    "selic_L1",
    taxa_L1,
    "ipca_L1",
    "desemprego_L1",
    "renda_L1"
  )
  
  x_com <- c(
    x_sem,
    "bets_L1"
  )
  
  if (
    incluir_inad_geral_no_modelo
  ) {
    
    x_sem <- c(
      x_sem,
      "inad_pf_geral_L1"
    )
    
    x_com <- c(
      x_com,
      "inad_pf_geral_L1"
    )
  }
  
  # --------------------------------------------------------------------------
  # MODELO SEM BETS
  # --------------------------------------------------------------------------
  
  ts_sem <- criar_ts(
    base_ardl,
    c(
      "inad_pf_10sm",
      x_sem
    )
  )
  
  busca_sem <- buscar_ardl(
    ts_data = ts_sem,
    y_var = "inad_pf_10sm",
    x_vars = x_sem,
    max_p = max_p,
    max_q = max_q,
    causal = TRUE,
    nome_modelo = paste0(
      taxa_nome,
      " – SEM BETS"
    )
  )
  
  melhor_sem_AIC <- busca_sem %>%
    arrange(AIC) %>%
    slice(1)
  
  melhor_sem_AICc <- busca_sem %>%
    arrange(AICc) %>%
    slice(1)
  
  melhor_sem_BIC <- busca_sem %>%
    arrange(BIC) %>%
    slice(1)
  
  melhor_sem_HQIC <- busca_sem %>%
    arrange(HQIC) %>%
    slice(1)
  
  selecao_sem <- selecionar_modelo_final(
    busca_sem
  )
  
  modelo_sem <- selecao_sem$modelo
  linha_sem <- selecao_sem$linha
  lm_sem <- to_lm_safe(
    modelo_sem
  )
  
  # --------------------------------------------------------------------------
  # MODELO COM BETS
  # --------------------------------------------------------------------------
  
  ts_com <- criar_ts(
    base_ardl,
    c(
      "inad_pf_10sm",
      x_com
    )
  )
  
  busca_com <- buscar_ardl(
    ts_data = ts_com,
    y_var = "inad_pf_10sm",
    x_vars = x_com,
    max_p = max_p,
    max_q = max_q,
    causal = TRUE,
    nome_modelo = paste0(
      taxa_nome,
      " – COM BETS"
    )
  )
  
  melhor_com_AIC <- busca_com %>%
    arrange(AIC) %>%
    slice(1)
  
  melhor_com_AICc <- busca_com %>%
    arrange(AICc) %>%
    slice(1)
  
  melhor_com_BIC <- busca_com %>%
    arrange(BIC) %>%
    slice(1)
  
  melhor_com_HQIC <- busca_com %>%
    arrange(HQIC) %>%
    slice(1)
  
  selecao_com <- selecionar_modelo_final(
    busca_com
  )
  
  modelo_com <- selecao_com$modelo
  linha_com <- selecao_com$linha
  lm_com <- to_lm_safe(
    modelo_com
  )
  
  # --------------------------------------------------------------------------
  # DIAGNÓSTICOS, HAC, VIF
  # --------------------------------------------------------------------------
  
  diag_sem <- diagnosticar_modelo(
    modelo_sem,
    linha_sem$p
  )
  
  diag_com <- diagnosticar_modelo(
    modelo_com,
    linha_com$p
  )
  
  coef_sem <- coef_hac(
    lm_sem
  )
  
  coef_com <- coef_hac(
    lm_com
  )
  
  multi_sem <- vif_manual(
    lm_sem
  )
  
  multi_com <- vif_manual(
    lm_com
  )
  
  # --------------------------------------------------------------------------
  # EFEITOS ACUMULADOS
  # --------------------------------------------------------------------------
  
  vars_efeitos_sem <- c(
    "selic_L1",
    taxa_L1,
    "ipca_L1",
    "desemprego_L1",
    "renda_L1"
  )
  
  vars_efeitos_com <- c(
    vars_efeitos_sem,
    "bets_L1"
  )
  
  if (
    incluir_inad_geral_no_modelo
  ) {
    
    vars_efeitos_sem <- c(
      vars_efeitos_sem,
      "inad_pf_geral_L1"
    )
    
    vars_efeitos_com <- c(
      vars_efeitos_com,
      "inad_pf_geral_L1"
    )
  }
  
  efeitos_sem <- map_dfr(
    vars_efeitos_sem,
    ~ efeito_acumulado(
      lm_sem,
      .x
    )
  )
  
  efeitos_com <- map_dfr(
    vars_efeitos_com,
    ~ efeito_acumulado(
      lm_com,
      .x
    )
  )
  
  efeito_taxa_sem <- efeito_acumulado(
    lm_sem,
    taxa_L1
  ) %>%
    mutate(
      modalidade = taxa_nome,
      modelo = "SEM BETS"
    )
  
  efeito_taxa_com <- efeito_acumulado(
    lm_com,
    taxa_L1
  ) %>%
    mutate(
      modalidade = taxa_nome,
      modelo = "COM BETS"
    )
  
  efeito_bets <- efeito_acumulado(
    lm_com,
    "bets_L1"
  ) %>%
    mutate(
      modalidade = taxa_nome
    )
  
  # --------------------------------------------------------------------------
  # BOUNDS / ECM
  # --------------------------------------------------------------------------
  
  bounds_sem <- rodar_bounds(
    modelo_sem
  )
  
  bounds_com <- rodar_bounds(
    modelo_com
  )
  
  # --------------------------------------------------------------------------
  # MÉTRICAS
  # --------------------------------------------------------------------------
  
  metricas <- bind_rows(
    metricas_modelo(
      "SEM BETS",
      modelo_sem,
      diag_sem
    ),
    metricas_modelo(
      "COM BETS",
      modelo_com,
      diag_com
    )
  ) %>%
    mutate(
      modalidade = taxa_nome,
      taxa_var = taxa_var,
      .before = 1
    )
  
  # --------------------------------------------------------------------------
  # LAGS
  # --------------------------------------------------------------------------
  
  nomes_sem <- c(
    "Selic",
    taxa_nome,
    "IPCA",
    "Desemprego",
    "Renda"
  )
  
  nomes_com <- c(
    nomes_sem,
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
    linha_sem,
    x_sem,
    nomes_sem
  ) %>%
    mutate(
      modalidade = taxa_nome,
      modelo = "SEM BETS",
      .before = 1
    )
  
  lags_com <- mostrar_lags(
    linha_com,
    x_com,
    nomes_com
  ) %>%
    mutate(
      modalidade = taxa_nome,
      modelo = "COM BETS",
      .before = 1
    )
  
  # --------------------------------------------------------------------------
  # OBSERVADO X AJUSTADO + ACF/PACF
  # --------------------------------------------------------------------------
  
  slug <- slug_modelo(
    taxa_nome
  )
  
  fit_sem <- criar_fit_df(
    lm_sem,
    busca_sem
  )
  
  fit_com <- criar_fit_df(
    lm_com,
    busca_com
  )
  
  plot_fit(
    fit_sem,
    paste0(
      "Observado × Ajustado – ",
      taxa_nome,
      " – sem Bets"
    ),
    paste0(
      "observado_ajustado_",
      slug,
      "_sem_bets.png"
    )
  )
  
  plot_fit(
    fit_com,
    paste0(
      "Observado × Ajustado – ",
      taxa_nome,
      " – com Bets"
    ),
    paste0(
      "observado_ajustado_",
      slug,
      "_com_bets.png"
    )
  )
  
  salvar_acf_pacf(
    residuals(
      lm_sem
    ),
    paste0(
      slug,
      "_sem_bets"
    ),
    paste0(
      taxa_nome,
      " – sem Bets"
    )
  )
  
  salvar_acf_pacf(
    residuals(
      lm_com
    ),
    paste0(
      slug,
      "_com_bets"
    ),
    paste0(
      taxa_nome,
      " – com Bets"
    )
  )
  
  # --------------------------------------------------------------------------
  # ROBUSTEZ CONTEMPORÂNEA
  # --------------------------------------------------------------------------
  
  robustez <- NULL
  
  if (
    rodar_robustez_contemporanea
  ) {
    
    x_sem_cont <- c(
      "selic",
      taxa_var,
      "ipca",
      "desemprego",
      "renda"
    )
    
    x_com_cont <- c(
      x_sem_cont,
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
      nome_modelo = paste0(
        taxa_nome,
        " – ROBUSTEZ SEM BETS"
      )
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
      nome_modelo = paste0(
        taxa_nome,
        " – ROBUSTEZ COM BETS"
      )
    )
    
    sel_com_cont <- selecionar_modelo_final(
      busca_com_cont
    )
    
    diag_com_cont <- diagnosticar_modelo(
      sel_com_cont$modelo,
      sel_com_cont$linha$p
    )
    
    robustez <- bind_rows(
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
    ) %>%
      mutate(
        modalidade = taxa_nome,
        taxa_var = taxa_var,
        .before = 1
      )
  }
  
  # --------------------------------------------------------------------------
  # EXPORTAÇÕES ESPECÍFICAS DA MODALIDADE
  # --------------------------------------------------------------------------
  
  readr::write_csv(
    head(
      busca_sem,
      250
    ),
    file.path(
      dir_saida,
      paste0(
        "ranking_",
        slug,
        "_sem_bets_top250.csv"
      )
    )
  )
  
  readr::write_csv(
    head(
      busca_com,
      250
    ),
    file.path(
      dir_saida,
      paste0(
        "ranking_",
        slug,
        "_com_bets_top250.csv"
      )
    )
  )
  
  list(
    taxa_var = taxa_var,
    taxa_nome = taxa_nome,
    x_sem = x_sem,
    x_com = x_com,
    
    busca_sem = busca_sem,
    busca_com = busca_com,
    
    melhor_sem_AIC = melhor_sem_AIC,
    melhor_sem_AICc = melhor_sem_AICc,
    melhor_sem_BIC = melhor_sem_BIC,
    melhor_sem_HQIC = melhor_sem_HQIC,
    
    melhor_com_AIC = melhor_com_AIC,
    melhor_com_AICc = melhor_com_AICc,
    melhor_com_BIC = melhor_com_BIC,
    melhor_com_HQIC = melhor_com_HQIC,
    
    selecao_sem = selecao_sem,
    selecao_com = selecao_com,
    
    modelo_sem = modelo_sem,
    modelo_com = modelo_com,
    
    lm_sem = lm_sem,
    lm_com = lm_com,
    
    diag_sem = diag_sem,
    diag_com = diag_com,
    
    coef_sem = coef_sem,
    coef_com = coef_com,
    
    multi_sem = multi_sem,
    multi_com = multi_com,
    
    efeitos_sem = efeitos_sem,
    efeitos_com = efeitos_com,
    efeito_taxa_sem = efeito_taxa_sem,
    efeito_taxa_com = efeito_taxa_com,
    efeito_bets = efeito_bets,
    
    bounds_sem = bounds_sem,
    bounds_com = bounds_com,
    
    metricas = metricas,
    lags_sem = lags_sem,
    lags_com = lags_com,
    
    fit_sem = fit_sem,
    fit_com = fit_com,
    
    robustez = robustez
  )
}

# ============================================================================
# 23. ESTIMAÇÃO DAS 6 MODALIDADES
# ============================================================================

resultados_modalidades <- vector(
  "list",
  nrow(
    modalidades_modelo
  )
)

names(
  resultados_modalidades
) <- modalidades_modelo$taxa_var

for (
  i in seq_len(
    nrow(
      modalidades_modelo
    )
  )
) {
  
  taxa_var_i <- modalidades_modelo$taxa_var[i]
  taxa_nome_i <- modalidades_modelo$taxa_nome[i]
  
  resultados_modalidades[[taxa_var_i]] <-
    rodar_par_modalidade(
      taxa_var = taxa_var_i,
      taxa_nome = taxa_nome_i
    )
}

# ============================================================================
# 24. CONSOLIDAÇÃO DOS RESULTADOS
# ============================================================================

comparacao_modelos <- map_dfr(
  resultados_modalidades,
  "metricas"
)

efeitos_taxas <- bind_rows(
  map_dfr(
    resultados_modalidades,
    "efeito_taxa_sem"
  ),
  map_dfr(
    resultados_modalidades,
    "efeito_taxa_com"
  )
)

efeitos_bets <- map_dfr(
  resultados_modalidades,
  "efeito_bets"
)

lags_todos <- bind_rows(
  map_dfr(
    resultados_modalidades,
    "lags_sem"
  ),
  map_dfr(
    resultados_modalidades,
    "lags_com"
  )
)

coeficientes_todos <- bind_rows(
  map_dfr(
    resultados_modalidades,
    function(x) {
      x$coef_sem %>%
        mutate(
          modalidade = x$taxa_nome,
          modelo = "SEM BETS",
          .before = 1
        )
    }
  ),
  map_dfr(
    resultados_modalidades,
    function(x) {
      x$coef_com %>%
        mutate(
          modalidade = x$taxa_nome,
          modelo = "COM BETS",
          .before = 1
        )
    }
  )
)

diagnosticos_todos <- bind_rows(
  map_dfr(
    resultados_modalidades,
    function(x) {
      x$diag_sem$tabela %>%
        mutate(
          modalidade = x$taxa_nome,
          modelo = "SEM BETS",
          .before = 1
        )
    }
  ),
  map_dfr(
    resultados_modalidades,
    function(x) {
      x$diag_com$tabela %>%
        mutate(
          modalidade = x$taxa_nome,
          modelo = "COM BETS",
          .before = 1
        )
    }
  )
)

vif_todos <- bind_rows(
  map_dfr(
    resultados_modalidades,
    function(x) {
      x$multi_sem$tabela %>%
        mutate(
          modalidade = x$taxa_nome,
          modelo = "SEM BETS",
          condition_number =
            x$multi_sem$condition_number,
          .before = 1
        )
    }
  ),
  map_dfr(
    resultados_modalidades,
    function(x) {
      x$multi_com$tabela %>%
        mutate(
          modalidade = x$taxa_nome,
          modelo = "COM BETS",
          condition_number =
            x$multi_com$condition_number,
          .before = 1
        )
    }
  )
)

robustez_todas <- if (
  rodar_robustez_contemporanea
) {
  
  bind_rows(
    map(
      resultados_modalidades,
      "robustez"
    )
  )
  
} else {
  
  NULL
}


bounds_resumo <- bind_rows(
  map_dfr(
    resultados_modalidades,
    function(x) {
      tibble(
        modalidade = x$taxa_nome,
        modelo = "SEM BETS",
        conclusao = x$bounds_sem$conclusao
      )
    }
  ),
  map_dfr(
    resultados_modalidades,
    function(x) {
      tibble(
        modalidade = x$taxa_nome,
        modelo = "COM BETS",
        conclusao = x$bounds_com$conclusao
      )
    }
  )
)

# Ranking principal: primeiro BIC, depois diagnóstico e parcimônia
ranking_modalidades <- comparacao_modelos %>%
  arrange(
    modelo,
    BIC,
    AICc,
    HQIC,
    RMSE
  ) %>%
  group_by(
    modelo
  ) %>%
  mutate(
    ranking_BIC = row_number()
  ) %>%
  ungroup()

# ============================================================================
# 25. EXPORTAÇÃO
# ============================================================================

abas <- list(
  base_final =
    base_final,
  
  descritivas =
    descritivas,
  
  correlacoes =
    as.data.frame(
      mat_cor
    ),
  
  correlacoes_selic_taxas =
    as.data.frame(
      cor_taxas
    ),
  
  estacionariedade =
    tab_estacionariedade,
  
  bounds_resumo =
    bounds_resumo,
  
  ranking_modalidades =
    ranking_modalidades,
  
  coeficientes_HAC =
    coeficientes_todos,
  
  efeitos_taxas =
    efeitos_taxas,
  
  efeitos_bets =
    efeitos_bets,
  
  lags =
    lags_todos,
  
  diagnosticos =
    diagnosticos_todos,
  
  VIF =
    vif_todos
)

if (
  !is.null(
    robustez_todas
  )
) {
  abas$robustez_contemporanea <-
    robustez_todas
}

# Adiciona as 250 melhores especificações de cada busca
for (
  taxa_var_i in names(
    resultados_modalidades
  )
) {
  
  obj <- resultados_modalidades[[taxa_var_i]]
  slug <- slug_modelo(
    obj$taxa_nome
  )
  
  abas[[
    paste0(
      "top_sem_",
      substr(slug, 1, 20)
    )
  ]] <- head(
    obj$busca_sem,
    250
  )
  
  abas[[
    paste0(
      "top_com_",
      substr(slug, 1, 20)
    )
  ]] <- head(
    obj$busca_com,
    250
  )
}

openxlsx::write.xlsx(
  abas,
  file = file.path(
    dir_saida,
    "resultados_SFN_modalidades_juros_com_sem_bets.xlsx"
  ),
  overwrite = TRUE
)

readr::write_csv(
  ranking_modalidades,
  file.path(
    dir_saida,
    "ranking_modalidades_juros.csv"
  )
)

readr::write_csv(
  efeitos_taxas,
  file.path(
    dir_saida,
    "efeitos_acumulados_modalidades_juros.csv"
  )
)

# ============================================================================
# 26. SÍNTESE FINAL
# ============================================================================

cat("\n\n")
cat("==================================================================\n")
cat("SFN – INADIMPLÊNCIA PF ATÉ 10 SM\n")
cat("COMPARAÇÃO DE MODALIDADES DE JUROS\n")
cat(
  "Período efetivo: ",
  format(data_inicio, "%YM%m"),
  "–",
  format(data_fim, "%YM%m"),
  "\n",
  sep = ""
)
cat("==================================================================\n\n")

cat(
  "Observações disponíveis: ",
  nrow(base_final),
  "\n",
  sep = ""
)

cat(
  "Taxas comparadas: ",
  paste(
    modalidades_modelo$taxa_nome,
    collapse = "; "
  ),
  "\n\n",
  sep = ""
)

cat("RANKING DOS MODELOS:\n")
print(
  ranking_modalidades %>%
    select(
      modalidade,
      modelo,
      BIC,
      AICc,
      HQIC,
      R2_ajustado,
      RMSE,
      BG_p,
      LjungBox_p,
      BP_p,
      RESET_p,
      JB_p,
      ranking_BIC
    ),
  n = Inf,
  width = Inf
)

cat("\nEFEITOS ACUMULADOS DAS TAXAS:\n")
print(
  efeitos_taxas %>%
    select(
      modalidade,
      modelo,
      efeito_acumulado,
      erro_padrao,
      estatistica,
      p_valor
    ),
  n = Inf,
  width = Inf
)

cat("\nEFEITOS DAS BETS POR ESPECIFICAÇÃO:\n")
print(
  efeitos_bets %>%
    select(
      modalidade,
      efeito_acumulado,
      erro_padrao,
      estatistica,
      p_valor
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nIMPORTANTE:\n",
  "- O spread livre PF não entra mais em nenhuma regressão.\n",
  "- A Selic permanece como variável de política monetária.\n",
  "- Cada modalidade de crédito é estimada separadamente com a Selic.\n",
  "- Isso permite comparar canais de transmissão e reduz multicolinearidade entre modalidades.\n",
  "- As taxas do CSV foram convertidas de % a.m. para equivalente anual % a.a.\n",
  "- Crédito pessoal não consignado no CSV = SGS 25464 (% a.m.).\n",
  "- O equivalente anual calculado não é rotulado como a série oficial SGS 20742.\n",
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
