# ============================================================
# PROJETOS V - MACRO
# Importacao, consolidacao, stargazer e graficos das series
# ============================================================

# ------------------------------------------------------------
# 0. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "readxl", "readr", "dplyr", "tidyr", "lubridate",
  "ggplot2", "purrr", "stringr", "scales", "stargazer"
)

novos <- pacotes[!pacotes %in% rownames(installed.packages())]

if (length(novos) > 0) {
  install.packages(novos, dependencies = TRUE)
}

invisible(
  lapply(
    pacotes,
    library,
    character.only = TRUE
  )
)

# ------------------------------------------------------------
# 1. Caminhos
# ------------------------------------------------------------
# Em R no Windows, prefira "/" em vez de "\".

pasta_base <- "C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados"

path_serasa <- file.path(
  pasta_base,
  "Cópia de inadimplencia-do-consumidor-jul26 - atualizada - serasa.xlsx"
)

path_selic <- file.path(
  pasta_base,
  "Selic Meta.csv"
)

path_modalidades <- file.path(
  pasta_base,
  "Inadimplência - PF - modalidades.csv"
)

path_sgs <- file.path(
  pasta_base,
  "Inadimplencia de crédito - pesssoa física - SGS.csv"
)

path_juros <- file.path(
  pasta_base,
  "Taxa média de juros em crédito.csv"
)

# Pasta para salvar tabelas, base compilada e graficos
pasta_saida <- file.path(pasta_base, "Saidas_R")

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# ------------------------------------------------------------
# 2. Funcoes auxiliares
# ------------------------------------------------------------

# Le os CSVs do BCB como texto.
# Isso evita problemas porque a ultima linha contem "Fonte".
ler_csv_bcb <- function(path) {
  
  readr::read_delim(
    file = path,
    delim = ";",
    locale = locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "Latin1"
    ),
    na = c("", "NA", "-", "n.d.", "n.d"),
    col_types = cols(.default = col_character()),
    trim_ws = TRUE,
    show_col_types = FALSE
  ) %>%
    filter(Data != "Fonte")
}

# Converte numero no padrao brasileiro
num_br <- function(x) {
  
  suppressWarnings(
    readr::parse_number(
      x,
      locale = locale(
        decimal_mark = ",",
        grouping_mark = "."
      ),
      na = c("", "NA", "-", "n.d.", "n.d")
    )
  )
}

# Padronizacao para os graficos conjuntos
z_score <- function(x) {
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  desvio <- sd(x, na.rm = TRUE)
  
  if (is.na(desvio) || desvio == 0) {
    return(rep(0, length(x)))
  }
  
  (x - mean(x, na.rm = TRUE)) / desvio
}

# Nome seguro para arquivo PNG
nome_seguro <- function(x) {
  
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_replace_all("[^A-Za-z0-9_-]", "_")
}

# ------------------------------------------------------------
# 3. SELIC META
# Frequencia original: diaria
# ------------------------------------------------------------

selic_raw <- ler_csv_bcb(path_selic)

names(selic_raw)[1:2] <- c(
  "Data",
  "Selic_Meta"
)

selic_diaria <- selic_raw %>%
  transmute(
    Data = lubridate::dmy(Data),
    Selic_Meta = num_br(Selic_Meta)
  ) %>%
  filter(
    !is.na(Data),
    !is.na(Selic_Meta)
  ) %>%
  arrange(Data)

# Para comparar com as demais series mensais:
# media mensal da Meta Selic.
#
# Se preferir o valor do fim de cada mes, substitua
# mean(Selic_Meta, na.rm = TRUE) por dplyr::last(Selic_Meta).

selic_mensal <- selic_diaria %>%
  mutate(
    Data = lubridate::floor_date(Data, unit = "month")
  ) %>%
  group_by(Data) %>%
  summarise(
    Selic_Meta = mean(Selic_Meta, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 4. TAXA MEDIA DE JUROS DO CREDITO
# ------------------------------------------------------------

juros_raw <- ler_csv_bcb(path_juros)

names(juros_raw)[1:2] <- c(
  "Data",
  "Taxa_Media_Juros_Credito"
)

juros <- juros_raw %>%
  transmute(
    Data = lubridate::my(Data),
    Taxa_Media_Juros_Credito = num_br(Taxa_Media_Juros_Credito)
  ) %>%
  filter(!is.na(Data)) %>%
  arrange(Data)

# ------------------------------------------------------------
# 5. INADIMPLENCIA PF TOTAL - SGS
# ------------------------------------------------------------

sgs_raw <- ler_csv_bcb(path_sgs)

names(sgs_raw)[1:2] <- c(
  "Data",
  "Inadimplencia_PF_Total"
)

inad_sgs <- sgs_raw %>%
  transmute(
    Data = lubridate::my(Data),
    Inadimplencia_PF_Total = num_br(Inadimplencia_PF_Total)
  ) %>%
  filter(!is.na(Data)) %>%
  arrange(Data)

# ------------------------------------------------------------
# 6. INADIMPLENCIA PF POR MODALIDADE
# ------------------------------------------------------------

modalidades_raw <- ler_csv_bcb(path_modalidades)

# O arquivo anexado possui:
# Data + 8 series.
if (ncol(modalidades_raw) != 9) {
  stop(
    paste0(
      "O arquivo de modalidades deveria ter 9 colunas, mas possui ",
      ncol(modalidades_raw),
      ". Verifique se a estrutura do CSV mudou."
    )
  )
}

names(modalidades_raw) <- c(
  "Data",
  "Inadimplencia_PF_Total_modalidades",
  "Inadimplencia_Recursos_Livres_Total",
  "Inadimplencia_Cheque_Especial",
  "Inadimplencia_Credito_Pessoal_Nao_Consignado",
  "Inadimplencia_Composicao_Dividas",
  "Inadimplencia_Consignado_Privado",
  "Inadimplencia_Nao_Consignado_Com_Garantia",
  "Inadimplencia_Nao_Consignado_Sem_Garantia"
)

modalidades <- modalidades_raw %>%
  mutate(
    Data = lubridate::my(Data)
  ) %>%
  mutate(
    across(
      -Data,
      num_br
    )
  ) %>%
  filter(!is.na(Data)) %>%
  arrange(Data)

# Observacao:
# "Inadimplencia_PF_Total_modalidades" replica a serie PF Total
# do arquivo SGS. Para nao duplicar a mesma serie no banco
# consolidado, ela sera retirada na etapa de merge.

# ------------------------------------------------------------
# 7. SERASA
# Aba: "Consumidores Inadimplentes"
# ------------------------------------------------------------
#
# Estrutura observada no Excel anexado:
# linhas 1-3 = titulo/cabecalho
# dados a partir da linha 4.
#
# Os "n.d." sao tratados como NA.

serasa_raw <- readxl::read_excel(
  path = path_serasa,
  sheet = "Consumidores Inadimplentes",
  skip = 3,
  col_names = FALSE,
  na = c("", "NA", "n.d.", "n.d")
)

# O arquivo possui 14 colunas nessa aba.
if (ncol(serasa_raw) < 14) {
  stop(
    paste0(
      "A aba 'Consumidores Inadimplentes' deveria ter pelo menos 14 colunas, mas possui ",
      ncol(serasa_raw),
      ". Verifique se a estrutura do Excel mudou."
    )
  )
}

serasa_raw <- serasa_raw[, 1:14]

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
    Data = as.Date(Data)
  ) %>%
  mutate(
    across(
      -Data,
      ~ suppressWarnings(as.numeric(.x))
    )
  ) %>%
  # A coluna "% da Populacao Adulta" vem do Excel como proporcao
  # (por exemplo, 0.397 = 39,7%). Convertemos para pontos percentuais.
  mutate(
    Serasa_Populacao_Adulta_pct =
      100 * Serasa_Populacao_Adulta_pct
  ) %>%
  filter(!is.na(Data)) %>%
  arrange(Data)

# Para a analise principal, usamos o numero de consumidores
# inadimplentes (milhoes) e o percentual da populacao adulta inadimplente.
serasa_principal <- serasa %>%
  select(
    Data,
    Serasa_Inadimplentes_milhoes,
    Serasa_Populacao_Adulta_pct
  ) %>%
  filter(
    !is.na(Serasa_Inadimplentes_milhoes) |
      !is.na(Serasa_Populacao_Adulta_pct)
  )

# ------------------------------------------------------------
# 8. BASE MENSAL CONSOLIDADA
# ------------------------------------------------------------

base_mensal <- purrr::reduce(
  list(
    selic_mensal,
    juros,
    inad_sgs,
    
    # Evita duplicar a PF Total, que ja esta no SGS:
    modalidades %>%
      select(-Inadimplencia_PF_Total_modalidades),
    
    serasa_principal
  ),
  full_join,
  by = "Data"
) %>%
  arrange(Data)

# Salva a base final
readr::write_csv(
  base_mensal,
  file.path(
    pasta_saida,
    "base_mensal_compilada.csv"
  ),
  na = ""
)

# Opcional: salva tambem a base Serasa completa
readr::write_csv(
  serasa,
  file.path(
    pasta_saida,
    "serasa_completo.csv"
  ),
  na = ""
)

# ------------------------------------------------------------
# 9. CHECAGENS
# ------------------------------------------------------------

cat("\n==============================\n")
cat("PERIODO DAS SERIES\n")
cat("==============================\n")

periodos <- tibble(
  Serie = names(base_mensal)[-1],
  Inicio = purrr::map_dbl(
    base_mensal[-1],
    ~ {
      idx <- which(!is.na(.x))
      if (length(idx) == 0) return(NA_real_)
      as.numeric(base_mensal$Data[min(idx)])
    }
  ),
  Fim = purrr::map_dbl(
    base_mensal[-1],
    ~ {
      idx <- which(!is.na(.x))
      if (length(idx) == 0) return(NA_real_)
      as.numeric(base_mensal$Data[max(idx)])
    }
  ),
  N = purrr::map_int(
    base_mensal[-1],
    ~ sum(!is.na(.x))
  )
) %>%
  mutate(
    Inicio = as.Date(Inicio, origin = "1970-01-01"),
    Fim = as.Date(Fim, origin = "1970-01-01")
  )

print(periodos)

readr::write_csv(
  periodos,
  file.path(
    pasta_saida,
    "periodos_e_observacoes.csv"
  )
)

# ------------------------------------------------------------
# 10. TABELA STARGAZER
# Estatisticas descritivas das series
# ------------------------------------------------------------

dados_stargazer <- base_mensal %>%
  select(-Data) %>%
  as.data.frame()

# Mostra no console
stargazer::stargazer(
  dados_stargazer,
  type = "text",
  title = "Estatisticas descritivas das series",
  digits = 2,
  summary.stat = c(
    "n",
    "mean",
    "sd",
    "min",
    "median",
    "max"
  )
)

# Salva como TXT
capture.output(
  stargazer::stargazer(
    dados_stargazer,
    type = "text",
    title = "Estatisticas descritivas das series",
    digits = 2,
    summary.stat = c(
      "n",
      "mean",
      "sd",
      "min",
      "median",
      "max"
    )
  ),
  file = file.path(
    pasta_saida,
    "tabela_stargazer.txt"
  )
)

# Salva como HTML
stargazer::stargazer(
  dados_stargazer,
  type = "html",
  title = "Estatisticas descritivas das series",
  digits = 2,
  summary.stat = c(
    "n",
    "mean",
    "sd",
    "min",
    "median",
    "max"
  ),
  out = file.path(
    pasta_saida,
    "tabela_stargazer.html"
  )
)

# Salva como LaTeX
stargazer::stargazer(
  dados_stargazer,
  type = "latex",
  title = "Estatisticas descritivas das series",
  label = "tab:estatisticas_series",
  digits = 2,
  summary.stat = c(
    "n",
    "mean",
    "sd",
    "min",
    "median",
    "max"
  ),
  out = file.path(
    pasta_saida,
    "tabela_stargazer.tex"
  )
)

# ------------------------------------------------------------
# 11. BASE EM FORMATO LONGO
# ------------------------------------------------------------

base_long <- base_mensal %>%
  pivot_longer(
    cols = -Data,
    names_to = "Serie",
    values_to = "Valor"
  )

# ------------------------------------------------------------
# 12. GRAFICOS INDIVIDUAIS
# Um PNG separado para cada serie
# ------------------------------------------------------------

series <- unique(base_long$Serie)

purrr::walk(
  series,
  function(serie_atual) {
    
    dados_grafico <- base_long %>%
      filter(
        Serie == serie_atual,
        !is.na(Valor)
      )
    
    if (nrow(dados_grafico) == 0) {
      return(NULL)
    }
    
    grafico <- ggplot(
      dados_grafico,
      aes(
        x = Data,
        y = Valor
      )
    ) +
      geom_line(
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      labs(
        title = serie_atual,
        x = NULL,
        y = NULL
      ) +
      scale_x_date(
        date_breaks = "2 years",
        date_labels = "%Y"
      ) +
      scale_y_continuous(
        labels = scales::label_number(
          decimal_mark = ",",
          big.mark = "."
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(
          face = "bold"
        )
      )
    
    ggsave(
      filename = file.path(
        pasta_saida,
        paste0(
          "grafico_",
          nome_seguro(serie_atual),
          ".png"
        )
      ),
      plot = grafico,
      width = 10,
      height = 5.5,
      dpi = 300
    )
  }
)

# ------------------------------------------------------------
# 13. GRAFICO SELIC META x INADIMPLENCIA SERASA
# ------------------------------------------------------------
#
# Como Selic esta em % a.a. e Serasa em milhoes de pessoas,
# nao e adequado simplesmente sobrepor os niveis brutos.
#
# Para comparar o movimento das duas series, usamos z-score:
# media = 0 e desvio-padrao = 1.

selic_serasa <- base_mensal %>%
  select(
    Data,
    Selic_Meta,
    Serasa_Inadimplentes_milhoes
  ) %>%
  drop_na() %>%
  mutate(
    Selic_Meta = z_score(Selic_Meta),
    Serasa_Inadimplentes_milhoes =
      z_score(Serasa_Inadimplentes_milhoes)
  ) %>%
  pivot_longer(
    cols = -Data,
    names_to = "Serie",
    values_to = "Z"
  ) %>%
  mutate(
    Serie = recode(
      Serie,
      "Selic_Meta" = "Meta Selic",
      "Serasa_Inadimplentes_milhoes" =
        "Consumidores inadimplentes - Serasa"
    )
  )

grafico_selic_inad <- ggplot(
  selic_serasa,
  aes(
    x = Data,
    y = Z,
    color = Serie
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Meta Selic e inadimplencia do consumidor",
    subtitle = "Series mensais padronizadas por z-score",
    x = NULL,
    y = "Desvios-padrao em relacao a media",
    color = NULL
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(grafico_selic_inad)

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_selic_meta_x_inadimplencia_serasa.png"
  ),
  plot = grafico_selic_inad,
  width = 11,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 14. GRAFICO CONJUNTO COM TODAS AS SERIES
# ------------------------------------------------------------
#
# As series possuem unidades muito diferentes:
# - Selic e juros: %
# - inadimplencia BCB: %
# - Serasa: milhoes de consumidores
#
# Portanto, o grafico conjunto e feito em z-score.
# Assim, todas ficam comparaveis no mesmo eixo.

base_long_z <- base_mensal %>%
  pivot_longer(
    cols = -Data,
    names_to = "Serie",
    values_to = "Valor"
  ) %>%
  group_by(Serie) %>%
  mutate(
    Z = z_score(Valor)
  ) %>%
  ungroup() %>%
  filter(!is.na(Z))

grafico_todas <- ggplot(
  base_long_z,
  aes(
    x = Data,
    y = Z,
    color = Serie
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.7,
    alpha = 0.9
  ) +
  labs(
    title = "Evolucao conjunta das series",
    subtitle = "Todas as series padronizadas por z-score",
    x = NULL,
    y = "Desvios-padrao em relacao a media",
    color = NULL
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE
    )
  )

print(grafico_todas)

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_conjunto_todas_series_zscore.png"
  ),
  plot = grafico_todas,
  width = 14,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# 15. GRAFICO CONJUNTO EM PAINEIS
# ------------------------------------------------------------
#
# Este grafico preserva a unidade original de cada serie,
# deixando cada uma em um painel separado dentro da mesma figura.

grafico_paineis <- ggplot(
  base_long %>%
    filter(!is.na(Valor)),
  aes(
    x = Data,
    y = Valor
  )
) +
  geom_line(
    linewidth = 0.6
  ) +
  facet_wrap(
    ~ Serie,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Todas as series em niveis",
    subtitle = "Escala vertical propria para cada serie",
    x = NULL,
    y = NULL
  ) +
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(
      face = "bold",
      size = 8
    )
  )

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_conjunto_todas_series_paineis.png"
  ),
  plot = grafico_paineis,
  width = 14,
  height = 20,
  dpi = 300
)

# ------------------------------------------------------------
# 16. CORRELACOES
# Opcional, mas util para a analise.
# ------------------------------------------------------------

matriz_cor <- base_mensal %>%
  select(-Data) %>%
  cor(
    use = "pairwise.complete.obs"
  )

matriz_cor

# ------------------------------------------------------------
# 16.1 PLOT DA MATRIZ DE CORRELAÇÃO
# ------------------------------------------------------------

matriz_cor_long <- matriz_cor %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variavel_1") %>%
  tidyr::pivot_longer(
    cols = -Variavel_1,
    names_to = "Variavel_2",
    values_to = "Correlacao"
  )

grafico_correlacao <- ggplot(
  matriz_cor_long,
  aes(
    x = Variavel_1,
    y = Variavel_2,
    fill = Correlacao
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Correlacao, 2)),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Matriz de correlação",
    subtitle = "Correlação de Pearson entre as séries",
    x = NULL,
    y = NULL,
    fill = "Correlação"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 9
    ),
    plot.title = element_text(
      face = "bold"
    ),
    panel.grid = element_blank()
  )

print(grafico_correlacao)

ggsave(
  filename = file.path(
    pasta_saida,
    "plot_matriz_correlacao.png"
  ),
  plot = grafico_correlacao,
  width = 12,
  height = 10,
  dpi = 300
)

write.csv(
  matriz_cor,
  file = file.path(
    pasta_saida,
    "matriz_correlacao.csv"
  ),
  row.names = TRUE
)

cat("\n==============================\n")
cat("CONCLUIDO\n")
cat("==============================\n")
cat(
  "Arquivos salvos em:\n",
  pasta_saida,
  "\n"
)