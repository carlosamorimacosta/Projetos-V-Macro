# ============================================================
# CORRELACAO:
# TAXAS MEDIAS DAS OPERACOES DE CREDITO
# x SELIC META
# x IBC-Br
# x % POPULACAO INADIMPLENTE - SERASA
# ============================================================


# ------------------------------------------------------------
# 0. PACOTES
# ------------------------------------------------------------

pacotes <- c(
  "readxl",
  "readr",
  "dplyr",
  "tidyr",
  "lubridate",
  "purrr",
  "stringr",
  "ggplot2",
  "writexl"
)

novos <- pacotes[
  !pacotes %in% rownames(installed.packages())
]

if (length(novos) > 0) {
  install.packages(
    novos,
    dependencies = TRUE
  )
}

invisible(
  lapply(
    pacotes,
    library,
    character.only = TRUE
  )
)


# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

pasta_base <- paste0(
  "C:/Users/carlo/Downloads/",
  "Projetos V - Macro/Base de dados"
)


# Funcao para localizar arquivos mesmo se houver
# sufixos como (1), (3), etc.

localizar_arquivo <- function(
    pasta,
    candidatos) {
  
  caminhos <- file.path(
    pasta,
    candidatos
  )
  
  existe <- file.exists(
    caminhos
  )
  
  if (!any(existe)) {
    
    stop(
      paste0(
        "Nenhum dos arquivos foi encontrado:\n",
        paste(
          caminhos,
          collapse = "\n"
        )
      )
    )
  }
  
  caminhos[
    which(existe)[1]
  ]
}


# Taxas medias das operacoes de credito

path_credito <- localizar_arquivo(
  pasta_base,
  c(
    "Taxas médias das op de crédito livre - modalidades - completa.csv"
  )
)


# Selic Meta

path_selic <- localizar_arquivo(
  pasta_base,
  c(
    "Selic Meta(3).csv",
    "Selic Meta(2).csv",
    "Selic Meta(1).csv",
    "Selic Meta.csv"
  )
)


# IBC-Br

path_ibc <- localizar_arquivo(
  pasta_base,
  c(
    "IBC(1).csv",
    "IBC.csv"
  )
)


# Serasa

path_serasa <- localizar_arquivo(
  pasta_base,
  c(
    paste0(
      "Cópia de inadimplencia-do-consumidor-",
      "jul26 - atualizada - serasa(3).xlsx"
    ),
    paste0(
      "Cópia de inadimplencia-do-consumidor-",
      "jul26 - atualizada - serasa.xlsx"
    )
  )
)


# Pasta de saida

pasta_saida <- file.path(
  pasta_base,
  "Saidas_R"
)

if (!dir.exists(pasta_saida)) {
  dir.create(
    pasta_saida,
    recursive = TRUE
  )
}


# ------------------------------------------------------------
# 2. FUNCOES AUXILIARES
# ------------------------------------------------------------


# Le CSV do Banco Central.
# Os arquivos usam:
# ;
# virgula como separador decimal
# codificacao Latin1.

ler_csv_bcb <- function(path) {
  
  readr::read_delim(
    file = path,
    delim = ";",
    locale = locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "Latin1"
    ),
    na = c(
      "",
      "NA",
      "-",
      "n.d.",
      "n.d"
    ),
    col_types = cols(
      .default = col_character()
    ),
    trim_ws = TRUE,
    show_col_types = FALSE
  ) %>%
    filter(
      Data != "Fonte"
    )
}


# Converte numeros no formato brasileiro.

num_br <- function(x) {
  
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      locale = locale(
        decimal_mark = ",",
        grouping_mark = "."
      ),
      na = c(
        "",
        "NA",
        "-",
        "n.d.",
        "n.d"
      )
    )
  )
}


# ------------------------------------------------------------
# Funcao para datas mensais
# ------------------------------------------------------------
#
# Ela aceita tanto:
#
# "jul/11"
# "ago/11"
#
# quanto:
#
# "08/2010"
# "01/2026"
#
# Isso e necessario porque os arquivos de credito
# usam meses em portugues.

parse_mes_ano <- function(x) {
  
  s <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  meses <- c(
    "jan" = "01",
    "fev" = "02",
    "mar" = "03",
    "abr" = "04",
    "mai" = "05",
    "jun" = "06",
    "jul" = "07",
    "ago" = "08",
    "set" = "09",
    "out" = "10",
    "nov" = "11",
    "dez" = "12"
  )
  
  
  for (m in names(meses)) {
    
    s <- stringr::str_replace(
      s,
      paste0(
        "^",
        m,
        "/"
      ),
      paste0(
        meses[[m]],
        "/"
      )
    )
  }
  
  
  partes <- stringr::str_match(
    s,
    "^(\\d{1,2})/(\\d{2,4})$"
  )
  
  
  mes <- suppressWarnings(
    as.integer(
      partes[, 2]
    )
  )
  
  
  ano <- suppressWarnings(
    as.integer(
      partes[, 3]
    )
  )
  
  
  # "11" -> 2011
  # "26" -> 2026
  
  ano <- ifelse(
    !is.na(ano) & ano < 100,
    2000 + ano,
    ano
  )
  
  
  data_txt <- ifelse(
    !is.na(mes) &
      !is.na(ano),
    
    sprintf(
      "%04d-%02d-01",
      ano,
      mes
    ),
    
    NA_character_
  )
  
  
  as.Date(
    data_txt
  )
}


# ------------------------------------------------------------
# 3. SELIC META
# Frequencia original: diaria
# ------------------------------------------------------------

selic_raw <- ler_csv_bcb(
  path_selic
)


# O arquivo possui:
# Data + Meta Selic

names(selic_raw)[1:2] <- c(
  "Data",
  "Selic_Meta"
)


selic_diaria <- selic_raw %>%
  
  transmute(
    
    Data = lubridate::dmy(
      Data
    ),
    
    Selic_Meta = num_br(
      Selic_Meta
    )
    
  ) %>%
  
  filter(
    !is.na(Data),
    !is.na(Selic_Meta)
  ) %>%
  
  arrange(
    Data
  )


# ------------------------------------------------------------
# Selic diaria -> media mensal
# ------------------------------------------------------------

selic_mensal <- selic_diaria %>%
  
  mutate(
    
    Data = lubridate::floor_date(
      Data,
      unit = "month"
    )
    
  ) %>%
  
  group_by(
    Data
  ) %>%
  
  summarise(
    
    Selic_Meta = mean(
      Selic_Meta,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    Data
  )


# ------------------------------------------------------------
# 4. IBC-Br
# Frequencia: mensal
# ------------------------------------------------------------

ibc_raw <- ler_csv_bcb(
  path_ibc
)


names(ibc_raw)[1:2] <- c(
  "Data",
  "IBC_Br"
)


ibc <- ibc_raw %>%
  
  transmute(
    
    Data = parse_mes_ano(
      Data
    ),
    
    IBC_Br = num_br(
      IBC_Br
    )
    
  ) %>%
  
  filter(
    !is.na(Data)
  ) %>%
  
  arrange(
    Data
  )


# ------------------------------------------------------------
# 5. SERASA
# Aba: Consumidores Inadimplentes
# ------------------------------------------------------------

serasa_raw <- readxl::read_excel(
  path = path_serasa,
  sheet = "Consumidores Inadimplentes",
  skip = 3,
  col_names = FALSE,
  na = c(
    "",
    "NA",
    "n.d.",
    "n.d"
  )
)


if (ncol(serasa_raw) < 14) {
  
  stop(
    paste0(
      "A aba Consumidores Inadimplentes possui apenas ",
      ncol(serasa_raw),
      " colunas."
    )
  )
}


serasa_raw <- serasa_raw[
  ,
  1:14
]


names(serasa_raw) <- c(
  "Data",
  "Serasa_Inadimplentes_milhoes",
  "Serasa_Dividas_Negativadas_milhoes",
  "Serasa_Dividas_Negativadas_R_bilhoes",
  "Serasa_Dividas_Media_por_CPF",
  "Serasa_Divida_Media_R",
  "Serasa_Ticket_Medio_R",
  "Serasa_Populacao_Adulta_pct",
  "Serasa_Genero_F_milhoes",
  "Serasa_Genero_M_milhoes",
  "Serasa_Ate_25_milhoes",
  "Serasa_26_40_milhoes",
  "Serasa_41_60_milhoes",
  "Serasa_Acima_60_milhoes"
)


serasa <- serasa_raw %>%
  
  mutate(
    
    Data = as.Date(
      Data
    )
    
  ) %>%
  
  mutate(
    
    across(
      -Data,
      ~ suppressWarnings(
        as.numeric(.x)
      )
    )
    
  ) %>%
  
  # O Excel armazena, por exemplo:
  #
  # 0.397 -> 39.7%
  
  mutate(
    
    Serasa_Populacao_Adulta_pct =
      100 *
      Serasa_Populacao_Adulta_pct
    
  ) %>%
  
  select(
    Data,
    Serasa_Populacao_Adulta_pct
  ) %>%
  
  filter(
    !is.na(Data)
  ) %>%
  
  arrange(
    Data
  )


# ------------------------------------------------------------
# 6. TAXAS MEDIAS DAS OPERACOES DE CREDITO
# ------------------------------------------------------------

credito_raw <- ler_csv_bcb(
  path_credito
)


# ------------------------------------------------------------
# Identificacao automatica das modalidades
# ------------------------------------------------------------

cabecalhos_taxas <- names(
  credito_raw
)[-1]


# Extrai o codigo SGS/BCB:
#
# 25463
# 25464
# 25465
# ...

codigos_taxas <- stringr::str_extract(
  cabecalhos_taxas,
  "^\\d+"
)


if (any(is.na(codigos_taxas))) {
  
  stop(
    paste0(
      "Nao foi possivel identificar o codigo BCB ",
      "de todas as series de credito."
    )
  )
}


# ------------------------------------------------------------
# Extrai apenas o nome da modalidade.
#
# Exemplo:
#
# "Cheque especial"
# "Credito pessoal nao consignado total"
# "Cartao de credito rotativo"
# etc.
# ------------------------------------------------------------

modalidades_taxas <- stringr::str_match(
  cabecalhos_taxas,
  "Pessoas físicas - (.*) - % a\\.m\\.$"
)[, 2]


# Caso algum nome nao seja identificado,
# mantemos o cabecalho original.

modalidades_taxas[
  is.na(modalidades_taxas)
] <- cabecalhos_taxas[
  is.na(modalidades_taxas)
]


# Nomes simples para trabalhar no R

nomes_taxas <- paste0(
  "Taxa_",
  codigos_taxas
)


# Dicionario para preservar os nomes originais

dicionario_taxas <- tibble(
  
  Serie = nomes_taxas,
  
  Codigo_BCB = codigos_taxas,
  
  Modalidade = modalidades_taxas,
  
  Cabecalho_Original = cabecalhos_taxas
  
)


# Renomeia o data frame

names(credito_raw) <- c(
  "Data",
  nomes_taxas
)


credito <- credito_raw %>%
  
  mutate(
    
    Data = parse_mes_ano(
      Data
    )
    
  ) %>%
  
  mutate(
    
    across(
      -Data,
      num_br
    )
    
  ) %>%
  
  filter(
    !is.na(Data)
  ) %>%
  
  arrange(
    Data
  )


# ------------------------------------------------------------
# 7. BASE MENSAL CONSOLIDADA
# ------------------------------------------------------------
#
# IMPORTANTE:
#
# usamos full_join, e nao inner_join.
#
# Assim, a correlacao Taxa x Selic pode aproveitar
# todo o periodo disponivel entre essas duas series,
# mesmo se o Serasa ainda nao tiver observacao.
#
# Cada correlacao usara seus proprios pares completos.

base_mensal <- purrr::reduce(
  
  list(
    credito,
    selic_mensal,
    ibc,
    serasa
  ),
  
  full_join,
  
  by = "Data"
  
) %>%
  
  arrange(
    Data
  )


# ------------------------------------------------------------
# 8. FUNCAO DE CORRELACAO
# ------------------------------------------------------------

calcular_correlacao <- function(
    serie_taxa,
    variavel_macro) {
  
  dados_cor <- base_mensal %>%
    select(
      all_of(
        c(
          serie_taxa,
          variavel_macro
        )
      )
    ) %>%
    tidyr::drop_na()
  
  
  n <- nrow(dados_cor)
  
  
  # Pelo menos 3 observacoes para cor.test()
  if (n < 3) {
    
    return(
      tibble(
        Serie_Taxa = serie_taxa,
        Variavel = variavel_macro,
        Correlacao_Pearson = NA_real_,
        P_Valor = NA_real_,
        N_Pares = n
      )
    )
  }
  
  
  # Extracao correta das colunas
  x <- dados_cor[[serie_taxa]]
  y <- dados_cor[[variavel_macro]]
  
  
  # Evita erro caso alguma serie seja constante
  if (
    sd(x, na.rm = TRUE) == 0 ||
    sd(y, na.rm = TRUE) == 0
  ) {
    
    return(
      tibble(
        Serie_Taxa = serie_taxa,
        Variavel = variavel_macro,
        Correlacao_Pearson = NA_real_,
        P_Valor = NA_real_,
        N_Pares = n
      )
    )
  }
  
  
  # Teste de correlacao de Pearson
  teste <- suppressWarnings(
    stats::cor.test(
      x,
      y,
      method = "pearson"
    )
  )
  
  
  tibble(
    Serie_Taxa = serie_taxa,
    Variavel = variavel_macro,
    Correlacao_Pearson = unname(teste$estimate),
    P_Valor = teste$p.value,
    N_Pares = n
  )
}


# ------------------------------------------------------------
# 9. CALCULO DAS CORRELACOES
# ------------------------------------------------------------

variaveis_macro <- c(
  "Selic_Meta",
  "IBC_Br",
  "Serasa_Populacao_Adulta_pct"
)


correlacoes <- purrr::map_dfr(
  
  nomes_taxas,
  
  function(serie_taxa) {
    
    purrr::map_dfr(
      
      variaveis_macro,
      
      function(variavel_macro) {
        
        calcular_correlacao(
          serie_taxa,
          variavel_macro
        )
        
      }
    )
  }
  
) %>%
  
  left_join(
    
    dicionario_taxas,
    
    by = c(
      "Serie_Taxa" = "Serie"
    )
    
  ) %>%
  
  mutate(
    
    Variavel_Label = recode(
      
      Variavel,
      
      "Selic_Meta" =
        "Selic Meta",
      
      "IBC_Br" =
        "IBC-Br",
      
      "Serasa_Populacao_Adulta_pct" =
        "% inadimplentes - Serasa"
      
    )
    
  ) %>%
  
  select(
    Codigo_BCB,
    Modalidade,
    Serie_Taxa,
    Variavel,
    Variavel_Label,
    Correlacao_Pearson,
    P_Valor,
    N_Pares
  )


# ------------------------------------------------------------
# 10. MOSTRAR RESULTADOS NO CONSOLE
# ------------------------------------------------------------

cat(
  "\n========================================\n"
)

cat(
  "CORRELACOES DAS TAXAS DE CREDITO\n"
)

cat(
  "========================================\n\n"
)


print(
  correlacoes,
  n = Inf
)


# ------------------------------------------------------------
# 11. TABELA EM FORMATO WIDE
# ------------------------------------------------------------
#
# Uma linha para cada modalidade.
#
# Colunas:
#
# Correlacao_Pearson_Selic
# Correlacao_Pearson_IBC_Br
# Correlacao_Pearson_Serasa
#
# e numero de observacoes de cada correlacao.

correlacoes_wide <- correlacoes %>%
  
  mutate(
    
    Variavel_Curta = recode(
      
      Variavel,
      
      "Selic_Meta" =
        "Selic",
      
      "IBC_Br" =
        "IBC_Br",
      
      "Serasa_Populacao_Adulta_pct" =
        "Serasa_pct"
      
    )
    
  ) %>%
  
  select(
    Codigo_BCB,
    Modalidade,
    Variavel_Curta,
    Correlacao_Pearson,
    P_Valor,
    N_Pares
  ) %>%
  
  pivot_wider(
    
    names_from =
      Variavel_Curta,
    
    values_from = c(
      Correlacao_Pearson,
      P_Valor,
      N_Pares
    ),
    
    names_sep = "_"
    
  )


cat(
  "\n========================================\n"
)

cat(
  "TABELA FINAL\n"
)

cat(
  "========================================\n\n"
)


print(
  correlacoes_wide,
  n = Inf
)


# ------------------------------------------------------------
# 12. HEATMAP
# ------------------------------------------------------------

# Cria um identificador unico para cada modalidade,
# combinando codigo BCB + nome da modalidade.

dados_heatmap <- correlacoes %>%
  
  mutate(
    
    Modalidade_Label = paste0(
      Codigo_BCB,
      " - ",
      Modalidade
    ),
    
    Variavel_Label = factor(
      Variavel_Label,
      levels = c(
        "Selic Meta",
        "IBC-Br",
        "% inadimplentes - Serasa"
      )
    ),
    
    Rotulo = ifelse(
      is.na(Correlacao_Pearson),
      paste0(
        "NA\nn=",
        N_Pares
      ),
      paste0(
        sprintf(
          "%.2f",
          Correlacao_Pearson
        ),
        "\n",
        "n=",
        N_Pares
      )
    )
  )


# ------------------------------------------------------------
# Ordena as modalidades na mesma ordem do dicionario
# ------------------------------------------------------------

ordem_modalidades <- dicionario_taxas %>%
  
  mutate(
    Modalidade_Label = paste0(
      Codigo_BCB,
      " - ",
      Modalidade
    )
  ) %>%
  
  distinct(
    Modalidade_Label
  ) %>%
  
  pull(
    Modalidade_Label
  )


dados_heatmap <- dados_heatmap %>%
  
  mutate(
    
    Modalidade_Label = factor(
      Modalidade_Label,
      levels = rev(
        ordem_modalidades
      )
    )
    
  )


# ------------------------------------------------------------
# Grafico
# ------------------------------------------------------------

grafico_correlacao <- ggplot(
  
  dados_heatmap,
  
  aes(
    x = Variavel_Label,
    y = Modalidade_Label,
    fill = Correlacao_Pearson
  )
  
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.4
  ) +
  
  geom_text(
    aes(
      label = Rotulo
    ),
    size = 2.7
  ) +
  
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(
      -1,
      1
    ),
    na.value = "grey90"
  ) +
  
  labs(
    title = paste0(
      "Correlação das taxas de crédito com ",
      "Selic, IBC-Br e inadimplência"
    ),
    subtitle = paste0(
      "Correlação contemporânea de Pearson | ",
      "n = número de pares disponíveis"
    ),
    x = NULL,
    y = NULL,
    fill = "Correlação\nde Pearson"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 7
    ),
    legend.position = "right"
  )


print(
  grafico_correlacao
)
# ------------------------------------------------------------
# 13. SALVAR HEATMAP
# ------------------------------------------------------------

ggsave(
  
  filename = file.path(
    pasta_saida,
    "heatmap_taxas_credito_selic_ibc_serasa.png"
  ),
  
  plot = grafico_correlacao,
  
  width = 11,
  height = 12,
  dpi = 300
  
)


# ------------------------------------------------------------
# 14. SALVAR EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(
  
  list(
    
    Correlacoes =
      correlacoes_wide,
    
    Correlacoes_long =
      correlacoes,
    
    Base_Mensal =
      base_mensal,
    
    Dicionario_Taxas =
      dicionario_taxas
    
  ),
  
  path = file.path(
    pasta_saida,
    "correlacoes_taxas_credito_selic_ibc_serasa.xlsx"
  )
  
)


# ------------------------------------------------------------
# 15. SALVAR CSV
# ------------------------------------------------------------

readr::write_csv(
  
  correlacoes_wide,
  
  file.path(
    pasta_saida,
    "correlacoes_taxas_credito_selic_ibc_serasa.csv"
  ),
  
  na = ""
  
)


readr::write_csv(
  
  base_mensal,
  
  file.path(
    pasta_saida,
    "base_mensal_credito_selic_ibc_serasa.csv"
  ),
  
  na = ""
  
)


# ------------------------------------------------------------
# 16. DIAGNOSTICO DA AMOSTRA
# ------------------------------------------------------------

cat(
  "\n========================================\n"
)

cat(
  "PERIODOS DAS BASES\n"
)

cat(
  "========================================\n"
)


cat(
  "\nTaxas de credito:\n",
  min(
    credito$Data,
    na.rm = TRUE
  ),
  " a ",
  max(
    credito$Data,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "\nSelic Meta:\n",
  min(
    selic_mensal$Data,
    na.rm = TRUE
  ),
  " a ",
  max(
    selic_mensal$Data,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "\nIBC-Br:\n",
  min(
    ibc$Data,
    na.rm = TRUE
  ),
  " a ",
  max(
    ibc$Data,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "\nSerasa - % populacao adulta:\n",
  min(
    serasa$Data[
      !is.na(
        serasa$Serasa_Populacao_Adulta_pct
      )
    ],
    na.rm = TRUE
  ),
  " a ",
  max(
    serasa$Data[
      !is.na(
        serasa$Serasa_Populacao_Adulta_pct
      )
    ],
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "\n========================================\n"
)

cat(
  "CONCLUIDO\n"
)

cat(
  "========================================\n"
)

cat(
  "Arquivos salvos em:\n",
  pasta_saida,
  "\n"
)