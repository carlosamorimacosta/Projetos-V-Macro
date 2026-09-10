# =============================================================================
# SPREAD DE CRÉDITO LIVRE - PESSOA FÍSICA
#
# Selic: SGS 432 - Meta Selic (% a.a.)
# Crédito: SGS 20740 - Taxa média de juros das operações de crédito
#                     com recursos livres - Pessoas físicas - Total (% a.a.)
#
# Spread = Juros Livre PF - Selic
#
# Frequência final: MENSAL
# =============================================================================


# =============================================================================
# 0. PACOTES
# =============================================================================

pacotes <- c(
  "httr",
  "jsonlite",
  "dplyr",
  "lubridate",
  "ggplot2",
  "openxlsx",
  "scales"
)

novos <- pacotes[
  !(pacotes %in% installed.packages()[, "Package"])
]

if(length(novos) > 0){
  install.packages(novos)
}

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(scales)


# =============================================================================
# 1. DEFINIR PERÍODO
# =============================================================================

# Como sua inadimplência do SCR.data começa em junho de 2012,
# faz sentido construir o spread a partir da mesma data.

data_inicio <- as.Date("2012-06-01")

# Você pode trocar pela última data da sua amostra, por exemplo:
# data_fim <- as.Date("2026-07-31")

data_fim <- Sys.Date()


# =============================================================================
# 2. PASTA DE SAÍDA
# =============================================================================

pasta_saida <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/Taxa de juros"

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 3. FUNÇÃO PARA BAIXAR SÉRIES DO SGS
#
# Ela divide períodos longos em blocos para evitar problemas da API.
# =============================================================================

baixar_sgs <- function(codigo, inicio, fim){
  
  inicio <- as.Date(inicio)
  fim    <- as.Date(fim)
  
  # Divide o período em blocos de no máximo 5 anos
  inicios <- seq(
    inicio,
    fim,
    by = "5 years"
  )
  
  lista <- list()
  
  for(i in seq_along(inicios)){
    
    ini <- inicios[i]
    
    if(i < length(inicios)){
      f <- inicios[i + 1] - 1
    } else {
      f <- fim
    }
    
    if(f > fim){
      f <- fim
    }
    
    url <- paste0(
      "https://api.bcb.gov.br/dados/serie/",
      "bcdata.sgs.",
      codigo,
      "/dados?",
      "formato=json",
      "&dataInicial=",
      format(ini, "%d/%m/%Y"),
      "&dataFinal=",
      format(f, "%d/%m/%Y")
    )
    
    cat(
      "\nBaixando SGS",
      codigo,
      ":",
      format(ini, "%d/%m/%Y"),
      "a",
      format(f, "%d/%m/%Y"),
      "\n"
    )
    
    resposta <- GET(
      url,
      timeout(60)
    )
    
    if(status_code(resposta) != 200){
      
      stop(
        paste0(
          "Erro ao baixar a série SGS ",
          codigo,
          ". HTTP ",
          status_code(resposta)
        )
      )
    }
    
    texto <- content(
      resposta,
      as = "text",
      encoding = "UTF-8"
    )
    
    temp <- fromJSON(
      texto,
      flatten = TRUE
    )
    
    if(nrow(temp) > 0){
      
      lista[[length(lista) + 1]] <- temp
      
    }
  }
  
  
  if(length(lista) == 0){
    
    stop(
      paste0(
        "A série SGS ",
        codigo,
        " retornou zero observações."
      )
    )
  }
  
  
  dados <- bind_rows(lista)
  
  
  # Converter data
  dados$data <- dmy(
    dados$data
  )
  
  
  # O JSON do BCB usa ponto decimal
  dados$valor <- as.numeric(
    gsub(",", ".", dados$valor)
  )
  
  
  dados <- dados %>%
    
    filter(
      !is.na(data),
      !is.na(valor)
    ) %>%
    
    distinct(
      data,
      .keep_all = TRUE
    ) %>%
    
    arrange(data)
  
  
  return(dados)
}


# =============================================================================
# 4. BAIXAR SELIC - SGS 432
# =============================================================================

selic_diaria <- baixar_sgs(
  codigo = 432,
  inicio = data_inicio,
  fim = data_fim
)


names(selic_diaria) <- c(
  "Data",
  "Selic_pct_aa"
)


cat("\n==============================================")
cat("\nSELIC")
cat("\n==============================================\n")

cat(
  "Observações:",
  nrow(selic_diaria),
  "\n"
)

cat(
  "Período:",
  format(min(selic_diaria$Data), "%d/%m/%Y"),
  "a",
  format(max(selic_diaria$Data), "%d/%m/%Y"),
  "\n"
)

print(
  head(selic_diaria)
)


# =============================================================================
# 5. TRANSFORMAR SELIC DIÁRIA EM SELIC MENSAL
# =============================================================================

selic_mensal <- selic_diaria %>%
  
  mutate(
    
    Data = floor_date(
      Data,
      "month"
    )
    
  ) %>%
  
  group_by(Data) %>%
  
  summarise(
    
    Selic_media_mensal_pct_aa = mean(
      Selic_pct_aa,
      na.rm = TRUE
    ),
    
    Selic_min_pct_aa = min(
      Selic_pct_aa,
      na.rm = TRUE
    ),
    
    Selic_max_pct_aa = max(
      Selic_pct_aa,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(Data)


# Validação
if(nrow(selic_mensal) == 0){
  
  stop(
    "ERRO: a mensalização da Selic produziu uma base vazia."
  )
}


# =============================================================================
# 6. BAIXAR TAXA MÉDIA DE JUROS LIVRES PF - SGS 20740
# =============================================================================

juros_pf <- baixar_sgs(
  codigo = 20740,
  inicio = data_inicio,
  fim = data_fim
)


names(juros_pf) <- c(
  "Data",
  "Juros_Livre_PF_pct_aa"
)


cat("\n==============================================")
cat("\nJUROS LIVRES PF - SGS 20740")
cat("\n==============================================\n")

cat(
  "Observações:",
  nrow(juros_pf),
  "\n"
)

cat(
  "Período:",
  format(min(juros_pf$Data), "%d/%m/%Y"),
  "a",
  format(max(juros_pf$Data), "%d/%m/%Y"),
  "\n"
)

print(
  head(juros_pf)
)


# =============================================================================
# 7. PADRONIZAR A SÉRIE 20740 PARA O MÊS
#
# A SGS 20740 JÁ É MENSAL.
# Apenas colocamos a data como primeiro dia do mês.
# =============================================================================

juros_pf_mensal <- juros_pf %>%
  
  mutate(
    
    Data = floor_date(
      Data,
      "month"
    )
    
  ) %>%
  
  group_by(Data) %>%
  
  summarise(
    
    Juros_Livre_PF_pct_aa = mean(
      Juros_Livre_PF_pct_aa,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(Data)


if(nrow(juros_pf_mensal) == 0){
  
  stop(
    "ERRO: a série mensal de juros PF ficou vazia."
  )
}


# =============================================================================
# 8. CONFERIR AS DUAS SÉRIES ANTES DA JUNÇÃO
# =============================================================================

cat("\n==============================================")
cat("\nCONFERÊNCIA")
cat("\n==============================================\n")

cat(
  "Meses Selic:",
  nrow(selic_mensal),
  "\n"
)

cat(
  "Meses Juros PF:",
  nrow(juros_pf_mensal),
  "\n"
)


cat("\nPrimeiros meses da Selic:\n")

print(
  head(selic_mensal, 6)
)


cat("\nPrimeiros meses de juros PF:\n")

print(
  head(juros_pf_mensal, 6)
)


# =============================================================================
# 9. UNIR AS DUAS SÉRIES
# =============================================================================

base_spread <- inner_join(
  
  juros_pf_mensal,
  
  selic_mensal,
  
  by = "Data"
  
) %>%
  
  arrange(Data)


# Validação essencial
if(nrow(base_spread) == 0){
  
  stop(
    paste0(
      "ERRO: a junção entre Selic e juros PF resultou em ZERO linhas. ",
      "Verifique as datas."
    )
  )
}


# =============================================================================
# 10. CALCULAR SPREAD
# =============================================================================

base_spread <- base_spread %>%
  
  mutate(
    
    Spread_Livre_PF_pp_aa =
      Juros_Livre_PF_pct_aa -
      Selic_media_mensal_pct_aa,
    
    Ano = year(Data),
    
    Mes = month(Data),
    
    Ano_Mes = format(
      Data,
      "%Y-%m"
    )
    
  ) %>%
  
  select(
    
    Data,
    Ano_Mes,
    Ano,
    Mes,
    
    Selic_media_mensal_pct_aa,
    
    Juros_Livre_PF_pct_aa,
    
    Spread_Livre_PF_pp_aa,
    
    Selic_min_pct_aa,
    
    Selic_max_pct_aa
    
  )


# =============================================================================
# 11. CHECAR NAs
# =============================================================================

cat("\n==============================================")
cat("\nBASE FINAL")
cat("\n==============================================\n")

cat(
  "Número de meses:",
  nrow(base_spread),
  "\n"
)

cat(
  "Período:",
  format(min(base_spread$Data), "%m/%Y"),
  "a",
  format(max(base_spread$Data), "%m/%Y"),
  "\n"
)


cat(
  "\nNAs Selic:",
  sum(
    is.na(
      base_spread$Selic_media_mensal_pct_aa
    )
  ),
  "\n"
)


cat(
  "NAs juros PF:",
  sum(
    is.na(
      base_spread$Juros_Livre_PF_pct_aa
    )
  ),
  "\n"
)


cat(
  "NAs Spread:",
  sum(
    is.na(
      base_spread$Spread_Livre_PF_pp_aa
    )
  ),
  "\n"
)


print(
  head(
    base_spread,
    12
  )
)


print(
  tail(
    base_spread,
    12
  )
)


# =============================================================================
# 12. ESTATÍSTICAS
# =============================================================================

estatisticas <- data.frame(
  
  Serie = c(
    "Selic média mensal (% a.a.)",
    "Juros livres PF (% a.a.)",
    "Spread livre PF (p.p. a.a.)"
  ),
  
  Media = c(
    
    mean(
      base_spread$Selic_media_mensal_pct_aa,
      na.rm = TRUE
    ),
    
    mean(
      base_spread$Juros_Livre_PF_pct_aa,
      na.rm = TRUE
    ),
    
    mean(
      base_spread$Spread_Livre_PF_pp_aa,
      na.rm = TRUE
    )
    
  ),
  
  Mediana = c(
    
    median(
      base_spread$Selic_media_mensal_pct_aa,
      na.rm = TRUE
    ),
    
    median(
      base_spread$Juros_Livre_PF_pct_aa,
      na.rm = TRUE
    ),
    
    median(
      base_spread$Spread_Livre_PF_pp_aa,
      na.rm = TRUE
    )
    
  ),
  
  Variancia = c(
    
    var(
      base_spread$Selic_media_mensal_pct_aa,
      na.rm = TRUE
    ),
    
    var(
      base_spread$Juros_Livre_PF_pct_aa,
      na.rm = TRUE
    ),
    
    var(
      base_spread$Spread_Livre_PF_pp_aa,
      na.rm = TRUE
    )
    
  ),
  
  Desvio_Padrao = c(
    
    sd(
      base_spread$Selic_media_mensal_pct_aa,
      na.rm = TRUE
    ),
    
    sd(
      base_spread$Juros_Livre_PF_pct_aa,
      na.rm = TRUE
    ),
    
    sd(
      base_spread$Spread_Livre_PF_pp_aa,
      na.rm = TRUE
    )
    
  )
  
)


print(
  estatisticas
)


# =============================================================================
# 13. GRÁFICO 1 - SELIC E JUROS LIVRES PF
# =============================================================================

grafico_taxas <- ggplot(
  base_spread,
  aes(x = Data)
) +
  
  geom_line(
    aes(
      y = Selic_media_mensal_pct_aa,
      linetype = "Selic"
    ),
    linewidth = 0.9
  ) +
  
  geom_line(
    aes(
      y = Juros_Livre_PF_pct_aa,
      linetype = "Juros livres PF"
    ),
    linewidth = 0.9
  ) +
  
  labs(
    title = "Selic e taxa média de juros do crédito livre",
    subtitle = "Pessoas físicas - Total",
    x = NULL,
    y = "% ao ano",
    linetype = NULL
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


print(
  grafico_taxas
)


# =============================================================================
# 14. GRÁFICO 2 - SPREAD
# =============================================================================

grafico_spread <- ggplot(
  base_spread,
  aes(
    x = Data,
    y = Spread_Livre_PF_pp_aa
  )
) +
  
  geom_line(
    linewidth = 0.9
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Spread do crédito livre - Pessoas físicas",
    subtitle = "Juros livres PF menos Selic média mensal",
    x = NULL,
    y = "Pontos percentuais ao ano"
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold")
  )


print(
  grafico_spread
)


# =============================================================================
# 15. SALVAR OS GRÁFICOS COMO PNG
# =============================================================================

arquivo_grafico_taxas <- file.path(
  pasta_saida,
  "01_Selic_vs_Juros_Livres_PF.png"
)


arquivo_grafico_spread <- file.path(
  pasta_saida,
  "02_Spread_Livre_PF.png"
)


ggsave(
  arquivo_grafico_taxas,
  grafico_taxas,
  width = 11,
  height = 6,
  dpi = 300
)


ggsave(
  arquivo_grafico_spread,
  grafico_spread,
  width = 11,
  height = 6,
  dpi = 300
)


# =============================================================================
# 16. EXPORTAR PARA EXCEL
# =============================================================================

arquivo_excel <- file.path(
  pasta_saida,
  "Spread_Mensal_Credito_Livre_PF.xlsx"
)


wb <- createWorkbook()


# -----------------------------------------------------------------------------
# Estilo
# -----------------------------------------------------------------------------

estilo_header <- createStyle(
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)


estilo_numero <- createStyle(
  numFmt = "0.00"
)


estilo_data <- createStyle(
  numFmt = "mmm/yyyy"
)


# -----------------------------------------------------------------------------
# ABA 1 - BASE
# -----------------------------------------------------------------------------

addWorksheet(
  wb,
  "Base Mensal"
)


writeData(
  wb,
  "Base Mensal",
  base_spread,
  headerStyle = estilo_header
)


freezePane(
  wb,
  "Base Mensal",
  firstRow = TRUE
)


addFilter(
  wb,
  "Base Mensal",
  rows = 1,
  cols = 1:ncol(base_spread)
)


setColWidths(
  wb,
  "Base Mensal",
  cols = 1:ncol(base_spread),
  widths = "auto"
)


addStyle(
  wb,
  "Base Mensal",
  estilo_data,
  rows = 2:(nrow(base_spread) + 1),
  cols = 1,
  gridExpand = TRUE
)


addStyle(
  wb,
  "Base Mensal",
  estilo_numero,
  rows = 2:(nrow(base_spread) + 1),
  cols = 5:9,
  gridExpand = TRUE
)


# -----------------------------------------------------------------------------
# ABA 2 - ESTATÍSTICAS
# -----------------------------------------------------------------------------

addWorksheet(
  wb,
  "Estatisticas"
)


writeData(
  wb,
  "Estatisticas",
  estatisticas,
  headerStyle = estilo_header
)


setColWidths(
  wb,
  "Estatisticas",
  cols = 1:ncol(estatisticas),
  widths = "auto"
)


# -----------------------------------------------------------------------------
# ABA 3 - GRÁFICOS
# -----------------------------------------------------------------------------

addWorksheet(
  wb,
  "Graficos"
)


insertImage(
  wb,
  sheet = "Graficos",
  file = arquivo_grafico_taxas,
  startRow = 2,
  startCol = 2,
  width = 10,
  height = 5.5,
  units = "in"
)


insertImage(
  wb,
  sheet = "Graficos",
  file = arquivo_grafico_spread,
  startRow = 32,
  startCol = 2,
  width = 10,
  height = 5.5,
  units = "in"
)


# -----------------------------------------------------------------------------
# ABA 4 - METODOLOGIA
# -----------------------------------------------------------------------------

metodologia <- data.frame(
  
  Item = c(
    "Selic",
    "Taxa de crédito",
    "Selic original",
    "Juros PF original",
    "Transformação da Selic",
    "Transformação dos juros PF",
    "Spread",
    "Unidade"
  ),
  
  Descricao = c(
    "SGS 432 - Meta Selic definida pelo Copom",
    "SGS 20740 - Taxa média de juros das operações de crédito com recursos livres - Pessoas físicas - Total",
    "Diária",
    "Mensal",
    "Média mensal das observações diárias",
    "Nenhuma mensalização necessária; série já é mensal",
    "Juros livres PF menos Selic média mensal",
    "Pontos percentuais ao ano"
  )
  
)


addWorksheet(
  wb,
  "Metodologia"
)


writeData(
  wb,
  "Metodologia",
  metodologia,
  headerStyle = estilo_header
)


setColWidths(
  wb,
  "Metodologia",
  cols = 1:2,
  widths = "auto"
)


# =============================================================================
# 17. SALVAR EXCEL
# =============================================================================

saveWorkbook(
  wb,
  arquivo_excel,
  overwrite = TRUE
)


# =============================================================================
# 18. SALVAR CSV TAMBÉM
# =============================================================================

write.csv2(
  base_spread,
  file.path(
    pasta_saida,
    "Spread_Mensal_Credito_Livre_PF.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 19. MENSAGEM FINAL
# =============================================================================

cat("\n\n====================================================")
cat("\nPROCESSAMENTO CONCLUÍDO")
cat("\n====================================================\n")

cat(
  "\nObservações mensais:",
  nrow(base_spread)
)

cat(
  "\nPeríodo:",
  format(min(base_spread$Data), "%m/%Y"),
  "até",
  format(max(base_spread$Data), "%m/%Y")
)

cat(
  "\n\nPrimeira observação:\n"
)

print(
  head(base_spread, 1)
)

cat(
  "\nÚltima observação:\n"
)

print(
  tail(base_spread, 1)
)

cat(
  "\n\nExcel salvo em:\n",
  arquivo_excel,
  "\n"
)