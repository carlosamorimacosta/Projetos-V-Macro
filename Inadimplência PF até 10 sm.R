# =============================================================================
# INADIMPLÊNCIA PF ATÉ 10 SALÁRIOS MÍNIMOS - SCR.DATA
# GAP EM RELAÇÃO À INADIMPLÊNCIA PF TOTAL - SGS
#
# O código:
# 1. Lê automaticamente TODOS os arquivos scrdata_*.zip da pasta
# 2. Extrai os CSVs mensais
# 3. Filtra pessoas físicas
# 4. Seleciona rendas até 10 salários mínimos
# 5. Calcula a inadimplência mensal PF <= 10 SM
# 6. Importa a inadimplência PF Total do SGS
# 7. Calcula o GAP
# 8. Calcula média, variância e desvio-padrão
# 9. Cria gráficos
# 10. Exporta a base final para Excel e CSV
#
# Ao adicionar novos ZIPs à pasta, basta rodar novamente.
# =============================================================================


# =============================================================================
# 0. PACOTES
# =============================================================================

pacotes <- c(
  "data.table",
  "dplyr",
  "lubridate",
  "stringi",
  "readr",
  "ggplot2",
  "openxlsx",
  "tidyr",
  "scales"
)

novos <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if(length(novos) > 0){
  install.packages(novos)
}

library(data.table)
library(dplyr)
library(lubridate)
library(stringi)
library(readr)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(scales)


# =============================================================================
# 1. CAMINHOS
# =============================================================================

# ---------------------------------------------------------------------------
# Coloque TODOS os arquivos do SCR.data nesta pasta:
#
# scrdata_2018.zip
# scrdata_2019.zip
# scrdata_2020.zip
# ...
# scrdata_2025.zip
# scrdata_2026.zip
#
# ---------------------------------------------------------------------------

pasta_scr <- "C:/Users/carlo/Downloads/Projetos V - Macro/scrdata"


# CSV da inadimplência PF total baixado do SGS
arquivo_sgs <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Inadimplencia de crédito - pesssoa física - SGS.csv"


# Pasta em que serão salvos os resultados
pasta_saida <- paste0(
  pasta_scr,
  "/Resultados"
)

dir.create(
  pasta_saida,
  showWarnings = FALSE,
  recursive = TRUE
)


# =============================================================================
# 2. FUNÇÕES AUXILIARES
# =============================================================================


# -----------------------------------------------------------------------------
# Padronizar texto
# -----------------------------------------------------------------------------

normalizar <- function(x){
  
  x <- as.character(x)
  
  x <- stringi::stri_trans_general(
    x,
    "Latin-ASCII"
  )
  
  x <- tolower(trimws(x))
  
  x <- gsub(
    "[^a-z0-9]+",
    "_",
    x
  )
  
  x <- gsub(
    "^_|_$",
    "",
    x
  )
  
  return(x)
}


# -----------------------------------------------------------------------------
# Converter valores monetários para numérico
# -----------------------------------------------------------------------------

converter_numero <- function(x){
  
  if(is.numeric(x)){
    return(as.numeric(x))
  }
  
  readr::parse_number(
    as.character(x),
    locale = locale(
      decimal_mark = ",",
      grouping_mark = "."
    )
  )
}


# -----------------------------------------------------------------------------
# Converter data-base para mês
# -----------------------------------------------------------------------------

converter_mes <- function(x){
  
  x <- as.character(x)
  
  data <- suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c(
        "ymd",
        "dmy",
        "Y-m",
        "m/Y",
        "Ym"
      )
    )
  )
  
  as.Date(
    lubridate::floor_date(
      data,
      unit = "month"
    )
  )
}


# =============================================================================
# 3. FUNÇÃO PARA PROCESSAR UM CSV DO SCR.DATA
# =============================================================================

processar_csv_scr <- function(arquivo){
  
  cat(
    "\nProcessando:",
    basename(arquivo),
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # Ler apenas o cabeçalho primeiro
  # --------------------------------------------------------------------------
  
  cabecalho <- names(
    data.table::fread(
      arquivo,
      nrows = 0,
      sep = ";",
      encoding = "UTF-8",
      check.names = FALSE
    )
  )
  
  
  nomes_limpos <- normalizar(cabecalho)
  
  
  # Criar correspondência entre nomes originais e padronizados
  mapa <- setNames(
    cabecalho,
    nomes_limpos
  )
  
  
  # --------------------------------------------------------------------------
  # Colunas necessárias
  # --------------------------------------------------------------------------
  
  colunas_necessarias <- c(
    "data_base",
    "cliente",
    "porte",
    "carteira_ativa",
    "carteira_inadimplencia"
  )
  
  
  faltando <- setdiff(
    colunas_necessarias,
    nomes_limpos
  )
  
  
  if(length(faltando) > 0){
    
    stop(
      paste0(
        "\nColunas ausentes em ",
        basename(arquivo),
        ": ",
        paste(faltando, collapse = ", ")
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Ler SOMENTE as colunas necessárias
  #
  # Isso economiza bastante memória porque o SCR.data é muito grande.
  # --------------------------------------------------------------------------
  
  colunas_originais <- unname(
    mapa[colunas_necessarias]
  )
  
  
  dados <- data.table::fread(
    arquivo,
    sep = ";",
    select = colunas_originais,
    encoding = "UTF-8",
    dec = ",",
    na.strings = c(
      "",
      "NA",
      "N/A"
    )
  )
  
  
  names(dados) <- normalizar(
    names(dados)
  )
  
  
  # --------------------------------------------------------------------------
  # Padronizar variáveis
  # --------------------------------------------------------------------------
  
  dados[, cliente_norm := normalizar(cliente)]
  
  dados[, porte_norm := normalizar(porte)]
  
  
  # --------------------------------------------------------------------------
  # Converter valores
  # --------------------------------------------------------------------------
  
  dados[, carteira_ativa :=
          converter_numero(carteira_ativa)]
  
  dados[, carteira_inadimplencia :=
          converter_numero(carteira_inadimplencia)]
  
  
  # --------------------------------------------------------------------------
  # Data
  # --------------------------------------------------------------------------
  
  dados[, data :=
          converter_mes(data_base)]
  
  
  # =============================================================================
  # 3.1 FILTRAR PESSOAS FÍSICAS
  # =============================================================================
  
  pf <- dados[
    grepl(
      "^pf($|_)|pessoas?_fisicas?",
      cliente_norm
    )
  ]
  
  
  if(nrow(pf) == 0){
    
    stop(
      paste0(
        "Nenhuma observação PF encontrada em ",
        basename(arquivo)
      )
    )
  }
  
  
  # =============================================================================
  # 3.2 DEFINIR FAIXAS DE RENDA ATÉ 10 SALÁRIOS MÍNIMOS
  # =============================================================================
  
  # Categorias oficiais:
  #
  # Até 1 SM
  # Mais de 1 a 2 SM
  # Mais de 2 a 3 SM
  # Mais de 3 a 5 SM
  # Mais de 5 a 10 SM
  #
  # NÃO entram:
  # Sem rendimento
  # Mais de 10 a 20 SM
  # Acima de 20 SM
  # Indisponível
  
  
  pf[, ate_10sm :=
       
       grepl(
         "^ate_1_salario",
         porte_norm
       ) |
       
       grepl(
         "mais_de_1_a_2_salario",
         porte_norm
       ) |
       
       grepl(
         "mais_de_2_a_3_salario",
         porte_norm
       ) |
       
       grepl(
         "mais_de_3_a_5_salario",
         porte_norm
       ) |
       
       grepl(
         "mais_de_5_a_10_salario",
         porte_norm
       )
  ]
  
  
  pf_ate10 <- pf[
    ate_10sm == TRUE
  ]
  
  
  if(nrow(pf_ate10) == 0){
    
    cat(
      "\nATENÇÃO: faixas encontradas no arquivo:\n"
    )
    
    print(
      unique(pf$porte)
    )
    
    stop(
      "Nenhuma faixa de renda até 10 SM foi identificada."
    )
  }
  
  
  # =============================================================================
  # 3.3 AGREGAR PF ATÉ 10 SM
  # =============================================================================
  
  serie_ate10 <- pf_ate10[
    ,
    .(
      Carteira_Ativa_PF_ate10SM =
        sum(
          carteira_ativa,
          na.rm = TRUE
        ),
      
      Carteira_Inadimplida_PF_ate10SM =
        sum(
          carteira_inadimplencia,
          na.rm = TRUE
        )
    ),
    by = data
  ]
  
  
  serie_ate10[
    ,
    Inadimplencia_PF_ate10SM_pct :=
      100 *
      Carteira_Inadimplida_PF_ate10SM /
      Carteira_Ativa_PF_ate10SM
  ]
  
  
  # =============================================================================
  # 3.4 PF TOTAL DENTRO DO PRÓPRIO SCR
  #
  # Esta série será útil como teste de robustez.
  # =============================================================================
  
  serie_pf_total_scr <- pf[
    ,
    .(
      Carteira_Ativa_PF_Total_SCR =
        sum(
          carteira_ativa,
          na.rm = TRUE
        ),
      
      Carteira_Inadimplida_PF_Total_SCR =
        sum(
          carteira_inadimplencia,
          na.rm = TRUE
        )
    ),
    by = data
  ]
  
  
  serie_pf_total_scr[
    ,
    Inadimplencia_PF_Total_SCR_pct :=
      100 *
      Carteira_Inadimplida_PF_Total_SCR /
      Carteira_Ativa_PF_Total_SCR
  ]
  
  
  # =============================================================================
  # 3.5 UNIR RESULTADOS
  # =============================================================================
  
  resultado <- merge(
    serie_ate10,
    serie_pf_total_scr,
    by = "data",
    all = TRUE
  )
  
  
  resultado[, arquivo_origem :=
              basename(arquivo)]
  
  
  return(resultado)
}


# =============================================================================
# 4. LER AUTOMATICAMENTE TODOS OS ZIPs
# =============================================================================

arquivos_zip <- list.files(
  pasta_scr,
  pattern = "^scrdata_.*\\.zip$",
  full.names = TRUE,
  ignore.case = TRUE
)


if(length(arquivos_zip) == 0){
  
  stop(
    "Nenhum arquivo scrdata_*.zip encontrado na pasta."
  )
}


cat(
  "\nForam encontrados",
  length(arquivos_zip),
  "arquivos ZIP.\n"
)

print(
  basename(arquivos_zip)
)


# =============================================================================
# 5. PROCESSAR TODOS OS ZIPs
# =============================================================================

lista_resultados <- list()


contador <- 1


for(zip_atual in arquivos_zip){
  
  cat(
    "\n====================================================\n"
  )
  
  cat(
    "ZIP:",
    basename(zip_atual),
    "\n"
  )
  
  cat(
    "====================================================\n"
  )
  
  
  pasta_temp <- tempfile(
    pattern = "scr_"
  )
  
  dir.create(
    pasta_temp
  )
  
  
  # --------------------------------------------------------------------------
  # Descompactar
  # --------------------------------------------------------------------------
  
  unzip(
    zip_atual,
    exdir = pasta_temp
  )
  
  
  # --------------------------------------------------------------------------
  # Encontrar todos os CSVs
  # --------------------------------------------------------------------------
  
  arquivos_csv <- list.files(
    pasta_temp,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  
  if(length(arquivos_csv) == 0){
    
    warning(
      paste(
        "Nenhum CSV encontrado em",
        basename(zip_atual)
      )
    )
    
    next
  }
  
  
  # --------------------------------------------------------------------------
  # Processar cada mês
  # --------------------------------------------------------------------------
  
  for(csv_atual in arquivos_csv){
    
    resultado_temp <- processar_csv_scr(
      csv_atual
    )
    
    
    lista_resultados[[contador]] <-
      resultado_temp
    
    
    contador <- contador + 1
  }
  
  
  # Apagar arquivos temporários
  unlink(
    pasta_temp,
    recursive = TRUE
  )
}


# =============================================================================
# 6. JUNTAR TODOS OS ANOS
# =============================================================================

scr_resultados <- data.table::rbindlist(
  lista_resultados,
  fill = TRUE
)


# Ordenar
setorder(
  scr_resultados,
  data
)


# =============================================================================
# 7. TRATAR EVENTUAIS ARQUIVOS DUPLICADOS
#
# Exemplo:
# scrdata_2026.zip
# scrdata_2026(1).zip
# scrdata_2026(2).zip
#
# O código verifica antes de remover duplicações.
# =============================================================================

checagem_duplicados <- scr_resultados[
  ,
  .(
    quantidade = .N,
    
    versoes_npl_ate10 =
      uniqueN(
        round(
          Inadimplencia_PF_ate10SM_pct,
          8
        )
      ),
    
    versoes_npl_total =
      uniqueN(
        round(
          Inadimplencia_PF_Total_SCR_pct,
          8
        )
      )
  ),
  by = data
]


problemas <- checagem_duplicados[
  versoes_npl_ate10 > 1 |
    versoes_npl_total > 1
]


if(nrow(problemas) > 0){
  
  print(problemas)
  
  stop(
    paste0(
      "Há arquivos diferentes produzindo valores diferentes ",
      "para o mesmo mês. Verifique ZIPs duplicados."
    )
  )
}


# Remover cópias idênticas
scr_mensal <- scr_resultados[
  !duplicated(data)
]


# =============================================================================
# 8. VALIDAÇÕES DA SÉRIE
# =============================================================================

if(
  any(
    scr_mensal$Inadimplencia_PF_ate10SM_pct < 0 |
    scr_mensal$Inadimplencia_PF_ate10SM_pct > 100,
    na.rm = TRUE
  )
){
  
  stop(
    "Há valores de inadimplência fora do intervalo 0-100%."
  )
}


cat(
  "\n\nPeríodo SCR encontrado:\n"
)

cat(
  format(min(scr_mensal$data), "%m/%Y"),
  "até",
  format(max(scr_mensal$data), "%m/%Y"),
  "\n"
)


# =============================================================================
# 9. IMPORTAR INADIMPLÊNCIA PF TOTAL - SGS
# =============================================================================

sgs <- data.table::fread(
  arquivo_sgs,
  sep = ";",
  encoding = "Latin-1",
  dec = ",",
  na.strings = c(
    "",
    "NA"
  )
)


# Padronizar nomes
names(sgs) <- normalizar(
  names(sgs)
)


cat(
  "\nColunas encontradas no SGS:\n"
)

print(
  names(sgs)
)


# =============================================================================
# 9.1 IDENTIFICAR COLUNA DE DATA
# =============================================================================

col_data <- grep(
  "^data$|^date$",
  names(sgs),
  value = TRUE
)


if(length(col_data) == 0){
  
  # Assume primeira coluna
  col_data <- names(sgs)[1]
}


# =============================================================================
# 9.2 IDENTIFICAR COLUNA DE VALOR
# =============================================================================

col_valor <- grep(
  "valor|inadimpl",
  names(sgs),
  value = TRUE
)


col_valor <- setdiff(
  col_valor,
  col_data
)


if(length(col_valor) == 0){
  
  # Assume segunda coluna
  col_valor <- names(sgs)[2]
}


col_data <- col_data[1]
col_valor <- col_valor[1]


# =============================================================================
# 9.3 CONVERTER DATA
# =============================================================================

sgs[, data_original := get(col_data)]


sgs[
  ,
  data :=
    as.Date(
      floor_date(
        parse_date_time(
          as.character(data_original),
          orders = c(
            "dmy",
            "ymd",
            "m/Y",
            "Y-m"
          )
        ),
        "month"
      )
    )
]


# =============================================================================
# 9.4 CONVERTER VALOR
# =============================================================================

sgs[
  ,
  Inadimplencia_PF_Total_SGS_pct :=
    converter_numero(
      get(col_valor)
    )
]


sgs_mensal <- sgs[
  !is.na(data),
  .(
    Inadimplencia_PF_Total_SGS_pct =
      last(
        Inadimplencia_PF_Total_SGS_pct
      )
  ),
  by = data
]


# =============================================================================
# 10. BASE FINAL
# =============================================================================

base_final <- merge(
  scr_mensal,
  sgs_mensal,
  by = "data",
  all = TRUE
)


setorder(
  base_final,
  data
)


# =============================================================================
# 11. GAP PRINCIPAL SOLICITADO
#
# PF até 10 SM - PF Total SGS
#
# Unidade = pontos percentuais
# =============================================================================

base_final[
  ,
  GAP_ate10SM_menos_PF_Total_SGS_pp :=
    Inadimplencia_PF_ate10SM_pct -
    Inadimplencia_PF_Total_SGS_pct
]


# =============================================================================
# 12. GAP ALTERNATIVO - MESMA BASE SCR.DATA
#
# Recomendado como teste de robustez
# =============================================================================

base_final[
  ,
  GAP_ate10SM_menos_PF_Total_SCR_pp :=
    Inadimplencia_PF_ate10SM_pct -
    Inadimplencia_PF_Total_SCR_pct
]


# =============================================================================
# 13. BASE COM JANELA COMUM
#
# Para comparar as séries corretamente usamos somente meses
# em que ambas as séries principais estão disponíveis.
# =============================================================================

base_comum <- base_final[
  !is.na(Inadimplencia_PF_ate10SM_pct) &
    !is.na(Inadimplencia_PF_Total_SGS_pct)
]


cat(
  "\nPeríodo comum:\n"
)

cat(
  format(min(base_comum$data), "%m/%Y"),
  "até",
  format(max(base_comum$data), "%m/%Y"),
  "\n"
)

cat(
  "Número de observações:",
  nrow(base_comum),
  "\n"
)


# =============================================================================
# 14. ESTATÍSTICAS DESCRITIVAS
# =============================================================================

estatisticas <- data.frame(
  
  Serie = c(
    "Inadimplência PF até 10 SM (%)",
    "Inadimplência PF Total SGS (%)",
    "GAP: até 10 SM - PF Total SGS (p.p.)",
    "Inadimplência PF Total SCR (%)",
    "GAP: até 10 SM - PF Total SCR (p.p.)"
  ),
  
  Media = c(
    
    mean(
      base_comum$Inadimplencia_PF_ate10SM_pct,
      na.rm = TRUE
    ),
    
    mean(
      base_comum$Inadimplencia_PF_Total_SGS_pct,
      na.rm = TRUE
    ),
    
    mean(
      base_comum$GAP_ate10SM_menos_PF_Total_SGS_pp,
      na.rm = TRUE
    ),
    
    mean(
      base_comum$Inadimplencia_PF_Total_SCR_pct,
      na.rm = TRUE
    ),
    
    mean(
      base_comum$GAP_ate10SM_menos_PF_Total_SCR_pp,
      na.rm = TRUE
    )
  ),
  
  Variancia = c(
    
    var(
      base_comum$Inadimplencia_PF_ate10SM_pct,
      na.rm = TRUE
    ),
    
    var(
      base_comum$Inadimplencia_PF_Total_SGS_pct,
      na.rm = TRUE
    ),
    
    var(
      base_comum$GAP_ate10SM_menos_PF_Total_SGS_pp,
      na.rm = TRUE
    ),
    
    var(
      base_comum$Inadimplencia_PF_Total_SCR_pct,
      na.rm = TRUE
    ),
    
    var(
      base_comum$GAP_ate10SM_menos_PF_Total_SCR_pp,
      na.rm = TRUE
    )
  ),
  
  Desvio_Padrao = c(
    
    sd(
      base_comum$Inadimplencia_PF_ate10SM_pct,
      na.rm = TRUE
    ),
    
    sd(
      base_comum$Inadimplencia_PF_Total_SGS_pct,
      na.rm = TRUE
    ),
    
    sd(
      base_comum$GAP_ate10SM_menos_PF_Total_SGS_pp,
      na.rm = TRUE
    ),
    
    sd(
      base_comum$Inadimplencia_PF_Total_SCR_pct,
      na.rm = TRUE
    ),
    
    sd(
      base_comum$GAP_ate10SM_menos_PF_Total_SCR_pp,
      na.rm = TRUE
    )
  )
)


print(
  estatisticas
)


# =============================================================================
# 15. CRIAR SÉRIES TEMPORAIS ts
# =============================================================================

# Garantir sequência mensal completa
datas_completas <- data.frame(
  data = seq(
    min(base_final$data),
    max(base_final$data),
    by = "month"
  )
)


base_final <- datas_completas %>%
  left_join(
    as.data.frame(base_final),
    by = "data"
  ) %>%
  arrange(data)


inicio_ano <- year(
  min(base_final$data)
)

inicio_mes <- month(
  min(base_final$data)
)


ts_inadimplencia_ate10 <- ts(
  base_final$Inadimplencia_PF_ate10SM_pct,
  start = c(
    inicio_ano,
    inicio_mes
  ),
  frequency = 12
)


ts_inadimplencia_pf_total <- ts(
  base_final$Inadimplencia_PF_Total_SGS_pct,
  start = c(
    inicio_ano,
    inicio_mes
  ),
  frequency = 12
)


ts_gap <- ts(
  base_final$GAP_ate10SM_menos_PF_Total_SGS_pp,
  start = c(
    inicio_ano,
    inicio_mes
  ),
  frequency = 12
)


# =============================================================================
# 16. GRÁFICO - INADIMPLÊNCIA
# =============================================================================

grafico_inadimplencia <- ggplot(
  base_comum,
  aes(x = data)
) +
  
  geom_line(
    aes(
      y = Inadimplencia_PF_ate10SM_pct,
      linetype = "PF até 10 SM"
    ),
    linewidth = 0.8
  ) +
  
  geom_line(
    aes(
      y = Inadimplencia_PF_Total_SGS_pct,
      linetype = "PF Total"
    ),
    linewidth = 0.8
  ) +
  
  labs(
    title = "Inadimplência de Pessoas Físicas",
    subtitle = "PF até 10 salários mínimos versus PF Total",
    x = NULL,
    y = "Inadimplência (%)",
    linetype = NULL
  ) +
  
  scale_y_continuous(
    labels = label_number(
      decimal_mark = ",",
      suffix = "%"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom"
  )


print(
  grafico_inadimplencia
)


# =============================================================================
# 17. GRÁFICO DO GAP
# =============================================================================

grafico_gap <- ggplot(
  base_comum,
  aes(
    x = data,
    y = GAP_ate10SM_menos_PF_Total_SGS_pp
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  labs(
    title = "GAP de inadimplência",
    subtitle = "PF até 10 SM menos PF Total",
    x = NULL,
    y = "Pontos percentuais"
  ) +
  
  scale_y_continuous(
    labels = label_number(
      decimal_mark = ","
    )
  ) +
  
  theme_minimal()


print(
  grafico_gap
)


# =============================================================================
# 18. SALVAR GRÁFICOS
# =============================================================================

ggsave(
  filename = paste0(
    pasta_saida,
    "/inadimplencia_pf_ate10sm_vs_total.png"
  ),
  plot = grafico_inadimplencia,
  width = 10,
  height = 6,
  dpi = 300
)


ggsave(
  filename = paste0(
    pasta_saida,
    "/gap_inadimplencia_pf_ate10sm.png"
  ),
  plot = grafico_gap,
  width = 10,
  height = 6,
  dpi = 300
)


# =============================================================================
# 19. ORGANIZAR BASE FINAL
# =============================================================================

base_exportar <- base_final %>%
  
  select(
    
    data,
    
    # Série principal <= 10 SM
    Inadimplencia_PF_ate10SM_pct,
    
    # Série PF total fornecida pelo SGS
    Inadimplencia_PF_Total_SGS_pct,
    
    # GAP solicitado
    GAP_ate10SM_menos_PF_Total_SGS_pp,
    
    # Série PF total calculada no próprio SCR
    Inadimplencia_PF_Total_SCR_pct,
    
    # GAP alternativo
    GAP_ate10SM_menos_PF_Total_SCR_pp,
    
    # Saldos utilizados na construção
    Carteira_Ativa_PF_ate10SM,
    
    Carteira_Inadimplida_PF_ate10SM,
    
    Carteira_Ativa_PF_Total_SCR,
    
    Carteira_Inadimplida_PF_Total_SCR
  )


# =============================================================================
# 20. EXPORTAR CSV
# =============================================================================

write.csv2(
  base_exportar,
  paste0(
    pasta_saida,
    "/Base_Final_Inadimplencia_PF.csv"
  ),
  row.names = FALSE,
  na = ""
)


# =============================================================================
# 21. EXPORTAR EXCEL
# =============================================================================

wb <- createWorkbook()


# Base mensal
addWorksheet(
  wb,
  "Base Mensal"
)

writeData(
  wb,
  "Base Mensal",
  base_exportar
)


# Estatísticas
addWorksheet(
  wb,
  "Estatisticas"
)

writeData(
  wb,
  "Estatisticas",
  estatisticas
)


# Base comum
addWorksheet(
  wb,
  "Amostra Comum"
)

writeData(
  wb,
  "Amostra Comum",
  base_comum
)


# Duplicidades
addWorksheet(
  wb,
  "Diagnostico"
)

writeData(
  wb,
  "Diagnostico",
  checagem_duplicados
)


saveWorkbook(
  wb,
  paste0(
    pasta_saida,
    "/Base_Final_Inadimplencia_PF.xlsx"
  ),
  overwrite = TRUE
)


# =============================================================================
# 22. RESULTADOS NO CONSOLE
# =============================================================================

cat(
  "\n====================================================\n"
)

cat(
  "PROCESSAMENTO CONCLUÍDO\n"
)

cat(
  "====================================================\n"
)

cat(
  "\nArquivos salvos em:\n",
  pasta_saida,
  "\n"
)


cat(
  "\nPrimeiras observações da base:\n"
)

print(
  head(
    base_exportar,
    12
  )
)


cat(
  "\nÚltimas observações da base:\n"
)

print(
  tail(
    base_exportar,
    12
  )
)


cat(
  "\nEstatísticas descritivas:\n"
)

print(
  estatisticas
)