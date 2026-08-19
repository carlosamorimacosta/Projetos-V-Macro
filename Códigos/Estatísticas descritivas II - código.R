# ============================================================
# PROJETOS V - MACRO
# Importacao, consolidacao, stargazer e graficos das series
# ============================================================

# ------------------------------------------------------------
# 0. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "readxl", "readr", "dplyr", "tidyr", "lubridate",
  "ggplot2", "purrr", "stringr", "scales", "stargazer",
  "writexl"
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

# Localiza o primeiro nome de arquivo existente entre as alternativas.
# Isso deixa o script robusto a nomes como "(1)" e "Selic.Meta(1).CSV".
localizar_arquivo <- function(pasta, candidatos) {
  
  caminhos <- file.path(pasta, candidatos)
  existe <- file.exists(caminhos)
  
  if (!any(existe)) {
    stop(
      paste0(
        "Nenhum dos arquivos foi encontrado:\n",
        paste(caminhos, collapse = "\n")
      )
    )
  }
  
  caminhos[which(existe)[1]]
}

path_serasa <- localizar_arquivo(
  pasta_base,
  c(
    "Cópia de inadimplencia-do-consumidor-jul26 - atualizada - serasa.xlsx"
  )
)

path_selic <- localizar_arquivo(
  pasta_base,
  c(
    "Selic.Meta(1).CSV",
    "Selic Meta(1).csv",
    "Selic Meta.csv"
  )
)

path_modalidades <- localizar_arquivo(
  pasta_base,
  c(
    "Inadimplência - PF - modalidades(1).csv",
    "Inadimplência - PF - modalidades.csv"
  )
)

path_sgs <- localizar_arquivo(
  pasta_base,
  c(
    "Inadimplencia de crédito - pesssoa física - SGS(1).csv",
    "Inadimplencia de crédito - pesssoa física - SGS.csv"
  )
)

path_juros <- localizar_arquivo(
  pasta_base,
  c(
    "Taxa média de juros em crédito(1).csv",
    "Taxa média de juros em crédito.csv"
  )
)

path_itau <- localizar_arquivo(
  pasta_base,
  c(
    "Planilha de Séries Históricas - Demonstrativos do Itaú.xlsx"
  )
)

# Novas bases macroeconomicas
path_pib <- localizar_arquivo(
  pasta_base,
  c(
    "pib.xls"
  )
)

path_desemprego_pnad <- localizar_arquivo(
  pasta_base,
  c(
    "Desemprego Pnad(1).csv",
    "Desemprego Pnad.csv"
  )
)

path_rendimento_pnad <- localizar_arquivo(
  pasta_base,
  c(
    "rendimento médio pnad(1).csv",
    "rendimento médio pnad.csv",
    "Rendimento médio pnad(1).csv",
    "Rendimento médio pnad.csv"
  )
)

path_ggr_bets <- localizar_arquivo(
  pasta_base,
  c(
    "GGR Bets Brasil(1).xlsx",
    "GGR Bets Brasil.xlsx"
  )
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

# ============================================================
# 8. NOVAS BASES MACROECONOMICAS
# PIB, PNAD CONTINUA E GGR BETS
# ============================================================

# ------------------------------------------------------------
# 8.1 Funcoes auxiliares
# ------------------------------------------------------------

# Media que devolve NA quando toda a janela e NA.
# Evita NaN nas agregacoes mensal -> trimestral -> anual.
media_na <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

# Parser simples para numeros que podem vir com virgula decimal.
parse_num_flex <- function(x) {
  
  s <- stringr::str_squish(as.character(x))
  s[s %in% c("", "NA", "-", "n.d.", "n.d")] <- NA_character_
  
  # Para as novas bases, os numeros nao possuem separadores de milhar
  # ambiguos. Assim, basta transformar virgula decimal em ponto.
  s <- stringr::str_replace_all(s, ",", ".")
  
  suppressWarnings(
    readr::parse_number(
      s,
      locale = locale(decimal_mark = ".")
    )
  )
}

# Converte os periodos moveis da PNAD, por exemplo:
# "jan-fev-mar 2012" -> 2012-03-01
# "nov-dez-jan 2013" -> 2013-01-01
#
# A data de referencia e o ULTIMO MES da janela de tres meses.
parse_periodo_pnad <- function(periodo) {
  
  s <- stringr::str_to_lower(
    stringr::str_squish(periodo)
  )
  
  ano <- suppressWarnings(
    as.integer(
      stringr::str_extract(
        s,
        "\\d{4}$"
      )
    )
  )
  
  bloco_meses <- stringr::str_remove(
    s,
    "\\s*\\d{4}$"
  )
  
  mes_final <- stringr::str_extract(
    bloco_meses,
    "[^-]+$"
  )
  
  mapa_meses <- c(
    "jan" = 1,
    "fev" = 2,
    "mar" = 3,
    "abr" = 4,
    "mai" = 5,
    "jun" = 6,
    "jul" = 7,
    "ago" = 8,
    "set" = 9,
    "out" = 10,
    "nov" = 11,
    "dez" = 12
  )
  
  mes <- unname(
    mapa_meses[mes_final]
  )
  
  as.Date(
    sprintf(
      "%04d-%02d-01",
      ano,
      mes
    )
  )
}

# Le os CSVs horizontais da PNAD fornecidos pelo IBGE.
# A linha 2 contem os periodos e a linha iniciada por "Brasil;"
# contem os valores.
ler_pnad_horizontal <- function(path, nome_saida) {
  
  linhas <- readLines(
    path,
    encoding = "UTF-8",
    warn = FALSE
  )
  
  if (length(linhas) < 3) {
    stop(
      paste0(
        "Arquivo PNAD com estrutura inesperada: ",
        path
      )
    )
  }
  
  linha_periodos <- strsplit(
    linhas[2],
    ";",
    fixed = TRUE
  )[[1]]
  
  periodos <- linha_periodos[-1]
  
  idx_brasil <- which(
    stringr::str_detect(
      stringr::str_to_lower(linhas),
      "^brasil;"
    )
  )[1]
  
  if (is.na(idx_brasil)) {
    stop(
      paste0(
        "Linha 'Brasil' nao encontrada em: ",
        path
      )
    )
  }
  
  linha_valores <- strsplit(
    linhas[idx_brasil],
    ";",
    fixed = TRUE
  )[[1]]
  
  valores <- linha_valores[-1]
  
  n <- min(
    length(periodos),
    length(valores)
  )
  
  resultado <- tibble(
    Data = parse_periodo_pnad(
      periodos[seq_len(n)]
    ),
    Valor = parse_num_flex(
      valores[seq_len(n)]
    )
  ) %>%
    filter(!is.na(Data))
  
  names(resultado)[2] <- nome_saida
  
  resultado
}

# ------------------------------------------------------------
# 8.2 Desemprego - PNAD Continua
# ------------------------------------------------------------

desemprego_pnad <- ler_pnad_horizontal(
  path_desemprego_pnad,
  "PNAD_Taxa_Desocupacao_pct"
)

# ------------------------------------------------------------
# 8.3 Rendimento medio - PNAD Continua
# ------------------------------------------------------------

rendimento_pnad <- ler_pnad_horizontal(
  path_rendimento_pnad,
  "PNAD_Rendimento_Medio_R"
)

# ------------------------------------------------------------
# 8.4 PIB - arquivo historico .xls
# ------------------------------------------------------------
#
# O arquivo "pib.xls" possui uma estrutura antiga, com cabecalhos
# em mais de uma linha. Por isso, a extracao abaixo localiza as
# colunas pelo TEXTO do cabecalho, em vez de depender de numero
# fixo de linha/coluna.
#
# O script tenta extrair:
#   1) PIB - variacao em volume, serie atual (%)
#   2) PIB em valores correntes (R$ milhoes)
#   3) PIB per capita em valores correntes
#
# Se alguma coluna nao existir, ela e simplesmente omitida.

pib_raw <- readxl::read_excel(
  path = path_pib,
  sheet = 1,
  col_names = FALSE,
  col_types = "text",
  na = c("", "NA", "-", "n.d.", "n.d"),
  .name_repair = "unique_quiet"
)

pib_mat <- as.matrix(pib_raw)

achar_posicao_pib <- function(padrao) {
  
  vetor <- as.character(pib_mat)
  
  hit <- stringr::str_detect(
    stringr::str_to_lower(vetor),
    stringr::regex(
      padrao,
      ignore_case = TRUE
    )
  )
  
  hit[is.na(hit)] <- FALSE
  
  hit_mat <- matrix(
    hit,
    nrow = nrow(pib_mat),
    ncol = ncol(pib_mat)
  )
  
  pos <- which(
    hit_mat,
    arr.ind = TRUE
  )
  
  if (nrow(pos) == 0) {
    return(NULL)
  }
  
  pos[1, , drop = FALSE]
}

# Coluna do ano
pos_ano <- achar_posicao_pib(
  "^\\s*ano"
)

if (is.null(pos_ano)) {
  stop(
    "Nao foi possivel localizar a coluna de ano no arquivo pib.xls."
  )
}

col_ano <- pos_ano[1, "col"]

anos_pib <- suppressWarnings(
  as.integer(
    stringr::str_extract(
      as.character(pib_raw[[col_ano]]),
      "(19|20)\\d{2}"
    )
  )
)

# Localiza a coluna "Serie atual" associada a variacao em volume.
pos_serie_atual <- achar_posicao_pib(
  "s[eé]rie\\s+atual"
)

pos_var_volume <- achar_posicao_pib(
  "produto interno bruto.*varia"
)

if (!is.null(pos_serie_atual)) {
  col_pib_var <- pos_serie_atual[1, "col"]
} else if (!is.null(pos_var_volume)) {
  col_pib_var <- pos_var_volume[1, "col"]
} else {
  col_pib_var <- NA_integer_
}

pos_pib_corrente <- achar_posicao_pib(
  "pib\\s+valores\\s+correntes"
)

pos_pib_pc <- achar_posicao_pib(
  "pib\\s+per\\s+capita"
)

col_pib_corrente <- if (
  is.null(pos_pib_corrente)
) NA_integer_ else pos_pib_corrente[1, "col"]

col_pib_pc <- if (
  is.null(pos_pib_pc)
) NA_integer_ else pos_pib_pc[1, "col"]

pib_anual <- tibble(
  Ano = anos_pib
)

if (!is.na(col_pib_var)) {
  pib_anual$PIB_Crescimento_Real_pct <- parse_num_flex(
    pib_raw[[col_pib_var]]
  )
}

if (!is.na(col_pib_corrente)) {
  pib_anual$PIB_Valores_Correntes_R_milhoes <- parse_num_flex(
    pib_raw[[col_pib_corrente]]
  )
}

if (!is.na(col_pib_pc)) {
  pib_anual$PIB_Per_Capita_R <- parse_num_flex(
    pib_raw[[col_pib_pc]]
  )
}

pib_anual <- pib_anual %>%
  filter(
    !is.na(Ano),
    Ano >= 1900,
    Ano <= 2100
  ) %>%
  distinct(
    Ano,
    .keep_all = TRUE
  ) %>%
  arrange(Ano)

# ------------------------------------------------------------
# 8.5 GGR Bets Brasil
# ------------------------------------------------------------
#
# A planilha possui tres series:
#   - operacoes offshore
#   - operacoes nacionais
#   - total
#
# Os anos 2026-2030 sao PROJECOES no arquivo.
# Eles sao mantidos nas estatisticas, mas excluidos da matriz de
# correlacao empirica anual por padrao.

ggr_raw <- readxl::read_excel(
  path = path_ggr_bets,
  sheet = 1,
  col_names = FALSE,
  na = c("", "NA", "-", "n.d.", "n.d"),
  .name_repair = "unique_quiet"
)

anos_ggr <- suppressWarnings(
  as.integer(
    unlist(
      ggr_raw[
        1,
        -1
      ]
    )
  )
)

rotulos_ggr <- stringr::str_squish(
  as.character(
    ggr_raw[[1]]
  )
)

extrair_linha_ggr <- function(
    rotulo,
    nome_saida) {
  
  idx <- which(
    stringr::str_to_lower(rotulos_ggr) ==
      stringr::str_to_lower(
        stringr::str_squish(rotulo)
      )
  )[1]
  
  if (is.na(idx)) {
    stop(
      paste0(
        "Linha nao encontrada no arquivo GGR: ",
        rotulo
      )
    )
  }
  
  valores <- parse_num_flex(
    unlist(
      ggr_raw[
        idx,
        -1
      ]
    )
  )
  
  n <- min(
    length(anos_ggr),
    length(valores)
  )
  
  out <- tibble(
    Ano = anos_ggr[seq_len(n)],
    Valor = valores[seq_len(n)]
  )
  
  names(out)[2] <- nome_saida
  
  out
}

ggr_anual <- purrr::reduce(
  list(
    extrair_linha_ggr(
      "GGR Operações Offshore",
      "GGR_Offshore_R_bilhoes"
    ),
    extrair_linha_ggr(
      "GGR Operações Nacionais",
      "GGR_Nacional_R_bilhoes"
    ),
    extrair_linha_ggr(
      "Total",
      "GGR_Total_R_bilhoes"
    )
  ),
  full_join,
  by = "Ano"
) %>%
  mutate(
    GGR_Tipo_Dado = if_else(
      Ano <= 2025,
      "Observado",
      "Projecao"
    )
  ) %>%
  arrange(Ano)


# ------------------------------------------------------------
# 9. BASE MENSAL CONSOLIDADA
# ------------------------------------------------------------

base_mensal <- purrr::reduce(
  list(
    selic_mensal,
    juros,
    inad_sgs,
    
    # Evita duplicar a PF Total, que ja esta no SGS:
    modalidades %>%
      select(-Inadimplencia_PF_Total_modalidades),
    
    serasa_principal,
    desemprego_pnad,
    rendimento_pnad
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
# 10. BASE COMPLETA PARA ESTATISTICAS DESCRITIVAS
# ------------------------------------------------------------
#
# Aqui mantemos TODAS as variaveis extraidas, inclusive
# Inadimplencia_PF_Total_modalidades, mesmo ela repetindo a serie
# PF Total do SGS. Isso garante que cada coluna extraida tenha
# suas proprias estatisticas registradas no Excel.

base_estatisticas <- purrr::reduce(
  list(
    selic_mensal,
    juros,
    inad_sgs,
    modalidades,
    serasa,
    desemprego_pnad,
    rendimento_pnad
  ),
  full_join,
  by = "Data"
) %>%
  arrange(Data)

# Funcao para calcular as estatisticas solicitadas por serie.
calcular_estatisticas <- function(nome_serie, x, datas) {
  
  ok <- !is.na(x)
  x_valido <- x[ok]
  datas_validas <- datas[ok]
  
  tibble(
    Serie = nome_serie,
    N = length(x_valido),
    Inicio = if (length(x_valido) > 0) min(datas_validas) else as.Date(NA),
    Fim = if (length(x_valido) > 0) max(datas_validas) else as.Date(NA),
    Media = if (length(x_valido) > 0) mean(x_valido) else NA_real_,
    Mediana = if (length(x_valido) > 0) median(x_valido) else NA_real_,
    Variancia = if (length(x_valido) > 1) var(x_valido) else NA_real_,
    Desvio_Padrao = if (length(x_valido) > 1) sd(x_valido) else NA_real_
  )
}

estatisticas_series <- purrr::map_dfr(
  names(base_estatisticas)[-1],
  ~ calcular_estatisticas(
    nome_serie = .x,
    x = base_estatisticas[[.x]],
    datas = base_estatisticas$Data
  )
)

# Mostra no console
print(estatisticas_series)

# Salva tambem em CSV
readr::write_csv(
  estatisticas_series,
  file.path(
    pasta_saida,
    "estatisticas_descritivas_todas_series.csv"
  ),
  na = ""
)

# Cria o Excel solicitado:
#   aba 1 = estatisticas descritivas
#   aba 2 = base mensal completa usada nos calculos
#   aba 3 = modalidades + Selic, que alimentam o grafico especifico
base_selic_modalidades_excel <- selic_mensal %>%
  full_join(
    modalidades,
    by = "Data"
  ) %>%
  arrange(Data)

writexl::write_xlsx(
  list(
    Estatisticas = estatisticas_series,
    Base_Mensal_Completa = base_estatisticas,
    Selic_e_Modalidades = base_selic_modalidades_excel
  ),
  path = file.path(
    pasta_saida,
    "estatisticas_descritivas_todas_series.xlsx"
  )
)


# ------------------------------------------------------------
# 11. CHECAGENS
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
# 12. TABELA STARGAZER
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
# 13. BASE EM FORMATO LONGO
# ------------------------------------------------------------

base_long <- base_mensal %>%
  pivot_longer(
    cols = -Data,
    names_to = "Serie",
    values_to = "Valor"
  )

# ------------------------------------------------------------
# 14. GRAFICOS INDIVIDUAIS
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
# 15. GRAFICO SELIC META x INADIMPLENCIA SERASA
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
# 16. GRAFICO SELIC META x MODALIDADES DE INADIMPLENCIA
# ------------------------------------------------------------
#
# Selic Meta: media mensal da serie diaria.
# Modalidades: percentuais mensais do BCB.
#
# O grafico abaixo mantem TODAS as 8 modalidades solicitadas.

selic_modalidades <- selic_mensal %>%
  inner_join(
    modalidades,
    by = "Data"
  ) %>%
  pivot_longer(
    cols = -Data,
    names_to = "Serie",
    values_to = "Valor"
  ) %>%
  filter(!is.na(Valor)) %>%
  mutate(
    Serie = recode(
      Serie,
      "Selic_Meta" = "Meta Selic",
      "Inadimplencia_PF_Total_modalidades" =
        "Inadimplencia PF - Total",
      "Inadimplencia_Recursos_Livres_Total" =
        "Recursos livres - Total",
      "Inadimplencia_Cheque_Especial" =
        "Cheque especial",
      "Inadimplencia_Credito_Pessoal_Nao_Consignado" =
        "Credito pessoal nao consignado",
      "Inadimplencia_Composicao_Dividas" =
        "Composicao de dividas",
      "Inadimplencia_Consignado_Privado" =
        "Consignado privado",
      "Inadimplencia_Nao_Consignado_Com_Garantia" =
        "Nao consignado com garantia",
      "Inadimplencia_Nao_Consignado_Sem_Garantia" =
        "Nao consignado sem garantia"
    )
  )

grafico_selic_modalidades <- ggplot(
  selic_modalidades,
  aes(
    x = Data,
    y = Valor,
    color = Serie,
    linetype = Serie
  )
) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.9
  ) +
  labs(
    title = "Meta Selic e modalidades de inadimplencia das pessoas fisicas",
    subtitle = "Meta Selic mensal e taxas de inadimplencia do BCB",
    x = NULL,
    y = "Percentual (%)",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      decimal_mark = ",",
      big.mark = ".",
      accuracy = 0.1
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  guides(
    color = guide_legend(
      ncol = 3,
      byrow = TRUE
    ),
    linetype = "none"
  )

print(grafico_selic_modalidades)

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_selic_meta_x_modalidades_inadimplencia.png"
  ),
  plot = grafico_selic_modalidades,
  width = 14,
  height = 8,
  dpi = 300
)


# ------------------------------------------------------------
# 17. GRAFICO CONJUNTO COM TODAS AS SERIES
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
# 18. GRAFICO CONJUNTO EM PAINEIS
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
# 19. ITAU - INADIMPLENCIA, CARTEIRA, COBERTURA E NPL CREATION
# ============================================================
#
# IMPORTANTE SOBRE FREQUENCIA:
# Os indicadores do Itau sao trimestrais. Para os graficos e
# correlacoes, a Meta Selic diaria e convertida para MEDIA
# TRIMESTRAL. Assim, todas as comparacoes usam a mesma frequencia.
#
# IMPORTANTE SOBRE "INADIMPLENCIA POR PRODUTO":
# A aba "Carteira_Nova segm. e títul" NAO apresenta taxa de
# inadimplencia por produto. Ela apresenta SALDO DA CARTEIRA
# por produto. Portanto, Cartao, Credito Pessoal, Consignado,
# Veiculos e Imobiliario sao extraidos como exposicao/carteira
# (R$ milhoes), e nao como NPL por produto.

# ------------------------------------------------------------
# 19.1 Funcoes auxiliares para o Excel do Itau
# ------------------------------------------------------------

ler_aba_itau <- function(aba) {
  
  readxl::read_excel(
    path = path_itau,
    sheet = aba,
    col_names = FALSE,
    col_types = "text",
    na = c("", "NA", "-", "n.d.", "n.d"),
    .name_repair = "unique_quiet"
  )
}

# Converte:
# 45657        -> 31/12/2024
# "31/mar/25"  -> 31/03/2025
# "2026-03-31" -> 31/03/2026
parse_data_itau <- function(x) {
  
  s <- stringr::str_squish(as.character(x))
  
  out <- rep(as.Date(NA), length(s))
  
  # Datas armazenadas como serial do Excel
  num <- suppressWarnings(as.numeric(s))
  idx_num <- !is.na(num) & num > 30000
  
  out[idx_num] <- as.Date(
    num[idx_num],
    origin = "1899-12-30"
  )
  
  # Meses em portugues
  s2 <- stringr::str_to_lower(s)
  
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
    s2 <- stringr::str_replace_all(
      s2,
      paste0("/", m, "/"),
      paste0("/", meses[[m]], "/")
    )
  }
  
  idx_txt <- is.na(out) & !is.na(s2) & s2 != ""
  
  if (any(idx_txt)) {
    
    data_ymd <- suppressWarnings(
      lubridate::ymd(s2[idx_txt])
    )
    
    data_dmy <- suppressWarnings(
      lubridate::dmy(s2[idx_txt])
    )
    
    out[idx_txt] <- dplyr::coalesce(
      data_ymd,
      data_dmy
    )
  }
  
  as.Date(out)
}

parse_num_itau <- function(x) {
  
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      locale = locale(
        decimal_mark = ".",
        grouping_mark = ","
      ),
      na = c("", "NA", "-", "n.d.", "n.d")
    )
  )
}

# Extrai uma linha pelo nome presente em uma coluna.
# Por padrao, pega a primeira ocorrencia do rotulo.
extrair_linha_itau <- function(
    dados,
    rotulo,
    nome_saida,
    linha_datas = 2,
    coluna_rotulo = 1,
    primeira_coluna_valores = 2,
    multiplicador = 1,
    primeira_ocorrencia = TRUE) {
  
  labels <- stringr::str_squish(
    as.character(dados[[coluna_rotulo]])
  )
  
  indices <- which(
    labels == stringr::str_squish(rotulo)
  )
  
  if (length(indices) == 0) {
    stop(
      paste0(
        "Rotulo nao encontrado no arquivo do Itau: ",
        rotulo
      )
    )
  }
  
  linha <- if (primeira_ocorrencia) {
    indices[1]
  } else {
    indices[length(indices)]
  }
  
  datas <- parse_data_itau(
    unlist(
      dados[
        linha_datas,
        primeira_coluna_valores:ncol(dados)
      ],
      use.names = FALSE
    )
  )
  
  valores <- parse_num_itau(
    unlist(
      dados[
        linha,
        primeira_coluna_valores:ncol(dados)
      ],
      use.names = FALSE
    )
  ) * multiplicador
  
  resultado <- tibble(
    Data_Trimestre = lubridate::floor_date(
      datas,
      unit = "quarter"
    ),
    Valor = valores
  ) %>%
    filter(!is.na(Data_Trimestre))
  
  names(resultado)[2] <- nome_saida
  
  resultado
}

# ------------------------------------------------------------
# 19.2 Selic em frequencia trimestral
# ------------------------------------------------------------

selic_trimestral <- selic_diaria %>%
  mutate(
    Data_Trimestre = lubridate::floor_date(
      Data,
      unit = "quarter"
    )
  ) %>%
  group_by(Data_Trimestre) %>%
  summarise(
    Selic_Meta = mean(
      Selic_Meta,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(Data_Trimestre)

# ------------------------------------------------------------
# 19.3 NPL 90 dias - Total, Brasil e segmentos
# Aba: NPL_com_TVM
# ------------------------------------------------------------

npl_itau_raw <- ler_aba_itau(
  "NPL_com_TVM"
)

npl_itau <- purrr::reduce(
  list(
    
    extrair_linha_itau(
      npl_itau_raw,
      "NPL 90 dias - Total",
      "Itau_NPL90_Total_pct",
      multiplicador = 100
    ),
    
    extrair_linha_itau(
      npl_itau_raw,
      "NPL 90 dias - Brasil",
      "Itau_NPL90_Brasil_pct",
      multiplicador = 100
    ),
    
    extrair_linha_itau(
      npl_itau_raw,
      "NPL 90 dias - Pessoas Físicas - Brasil",
      "Itau_NPL90_PF_Brasil_pct",
      multiplicador = 100
    ),
    
    extrair_linha_itau(
      npl_itau_raw,
      "NPL 90 dias - Micro, Pequenas e Médias Empresas - Brasil",
      "Itau_NPL90_MPME_Brasil_pct",
      multiplicador = 100
    ),
    
    extrair_linha_itau(
      npl_itau_raw,
      "NPL 90 dias - Grandes Empresas - Brasil",
      "Itau_NPL90_Grandes_Empresas_Brasil_pct",
      multiplicador = 100
    )
  ),
  full_join,
  by = "Data_Trimestre"
) %>%
  arrange(Data_Trimestre)

# ------------------------------------------------------------
# 19.4 Carteira por produto/segmento
# Aba: Carteira_Nova segm. e títul
# ------------------------------------------------------------
#
# Estes valores sao SALDOS DA CARTEIRA (R$ milhoes),
# nao taxas de inadimplencia por produto.

carteira_itau_raw <- ler_aba_itau(
  "Carteira_Nova segm. e títul"
)

carteira_produtos_itau <- purrr::reduce(
  list(
    
    # Primeira ocorrencia de Pessoas Fisicas = carteira total PF
    extrair_linha_itau(
      carteira_itau_raw,
      "Pessoas Físicas",
      "Itau_Carteira_PF_Total_R_milhoes",
      primeira_ocorrencia = TRUE
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "Cartão de Crédito",
      "Itau_Carteira_Cartao_Credito_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "Crédito Pessoal",
      "Itau_Carteira_Credito_Pessoal_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "Crédito Consignado",
      "Itau_Carteira_Credito_Consignado_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "Veículos",
      "Itau_Carteira_Veiculos_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "Crédito Imobiliário",
      "Itau_Carteira_Credito_Imobiliario_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "Micro, Pequenas e Médias Empresas (*)",
      "Itau_Carteira_MPME_R_milhoes"
    )
  ),
  full_join,
  by = "Data_Trimestre"
) %>%
  arrange(Data_Trimestre)

# ------------------------------------------------------------
# 19.5 Cobertura de provisoes - Estagio 3
# Aba: Tabela 4966
# ------------------------------------------------------------

tabela_4966_raw <- ler_aba_itau(
  "Tabela 4966"
)

# Localiza o inicio do bloco "Cobertura do Estagio 3"
coluna_a_4966 <- stringr::str_squish(
  as.character(tabela_4966_raw[[1]])
)

linha_inicio_cobertura3 <- which(
  stringr::str_detect(
    coluna_a_4966,
    "^Cobertura do Estágio 3"
  )
)[1]

if (is.na(linha_inicio_cobertura3)) {
  stop(
    "Bloco 'Cobertura do Estágio 3' nao encontrado na aba Tabela 4966."
  )
}

# As cinco linhas do bloco:
# PF Brasil, PJ Brasil, Brasil, America Latina e Total.
linhas_cobertura3 <- linha_inicio_cobertura3:
  (linha_inicio_cobertura3 + 4)

datas_cobertura3 <- parse_data_itau(
  unlist(
    tabela_4966_raw[
      2,
      3:ncol(tabela_4966_raw)
    ],
    use.names = FALSE
  )
)

nomes_cobertura3 <- c(
  "Pessoas Físicas - Brasil" =
    "Itau_Cobertura_Estagio3_PF_Brasil_pct",
  "Pessoas Jurídicas - Brasil" =
    "Itau_Cobertura_Estagio3_PJ_Brasil_pct",
  "Brasil" =
    "Itau_Cobertura_Estagio3_Brasil_pct",
  "América Latina" =
    "Itau_Cobertura_Estagio3_America_Latina_pct",
  "Total" =
    "Itau_Cobertura_Estagio3_Total_pct"
)

lista_cobertura3 <- purrr::map(
  linhas_cobertura3,
  function(linha) {
    
    segmento <- stringr::str_squish(
      as.character(
        tabela_4966_raw[[2]][linha]
      )
    )
    
    nome_saida <- nomes_cobertura3[[segmento]]
    
    if (is.null(nome_saida)) {
      stop(
        paste0(
          "Segmento inesperado no bloco de Cobertura Estagio 3: ",
          segmento
        )
      )
    }
    
    valores <- parse_num_itau(
      unlist(
        tabela_4966_raw[
          linha,
          3:ncol(tabela_4966_raw)
        ],
        use.names = FALSE
      )
    ) * 100
    
    resultado <- tibble(
      Data_Trimestre = lubridate::floor_date(
        datas_cobertura3,
        unit = "quarter"
      ),
      Valor = valores
    ) %>%
      filter(!is.na(Data_Trimestre))
    
    names(resultado)[2] <- nome_saida
    
    resultado
  }
)

cobertura_estagio3_itau <- purrr::reduce(
  lista_cobertura3,
  full_join,
  by = "Data_Trimestre"
) %>%
  arrange(Data_Trimestre)

# ------------------------------------------------------------
# 19.6 NPL Creation
# Aba: Carteira_Nova segm. e títul
# ------------------------------------------------------------

npl_creation_itau <- purrr::reduce(
  list(
    
    extrair_linha_itau(
      carteira_itau_raw,
      "NPL Creation - Total",
      "Itau_NPL_Creation_Total_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "NPL Creation - Varejo - Brasil",
      "Itau_NPL_Creation_Varejo_Brasil_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "NPL Creation - Atacado - Brasil",
      "Itau_NPL_Creation_Atacado_Brasil_R_milhoes"
    ),
    
    extrair_linha_itau(
      carteira_itau_raw,
      "NPL Creation - América Latina",
      "Itau_NPL_Creation_America_Latina_R_milhoes"
    )
  ),
  full_join,
  by = "Data_Trimestre"
) %>%
  arrange(Data_Trimestre)

# ------------------------------------------------------------
# 19.7 Base trimestral consolidada Itau + Selic
# ------------------------------------------------------------

base_itau_trimestral <- purrr::reduce(
  list(
    npl_itau,
    carteira_produtos_itau,
    cobertura_estagio3_itau,
    npl_creation_itau
  ),
  full_join,
  by = "Data_Trimestre"
) %>%
  arrange(Data_Trimestre)

base_itau_selic <- selic_trimestral %>%
  inner_join(
    base_itau_trimestral,
    by = "Data_Trimestre"
  ) %>%
  arrange(Data_Trimestre)

# ------------------------------------------------------------
# 19.8 Estatisticas descritivas das series do Itau
# ------------------------------------------------------------

estatisticas_itau <- purrr::map_dfr(
  names(base_itau_trimestral)[-1],
  ~ calcular_estatisticas(
    nome_serie = .x,
    x = base_itau_trimestral[[.x]],
    datas = base_itau_trimestral$Data_Trimestre
  )
) %>%
  mutate(
    Frequencia = "Trimestral",
    .after = Serie
  )

print(estatisticas_itau)

# ------------------------------------------------------------
# 19.9 Correlacao contemporanea com a Meta Selic
# ------------------------------------------------------------
#
# Pearson, usando apenas trimestres em que ambas as series
# possuem observacao. A coluna N_Trimestres mostra quantos
# pares efetivamente entraram na correlacao.

correlacao_selic_itau <- purrr::map_dfr(
  names(base_itau_trimestral)[-1],
  function(nome_serie) {
    
    dados_cor <- base_itau_selic %>%
      select(
        Selic_Meta,
        all_of(nome_serie)
      ) %>%
      tidyr::drop_na()
    
    valor_cor <- if (nrow(dados_cor) >= 2) {
      cor(
        dados_cor$Selic_Meta,
        dados_cor[[nome_serie]],
        method = "pearson"
      )
    } else {
      NA_real_
    }
    
    tibble(
      Serie = nome_serie,
      Correlacao_Pearson_Selic = valor_cor,
      N_Trimestres = nrow(dados_cor)
    )
  }
) %>%
  arrange(
    desc(
      abs(Correlacao_Pearson_Selic)
    )
  )

print(correlacao_selic_itau)

# ------------------------------------------------------------
# 19.10 Dicionario das series do Itau
# ------------------------------------------------------------

dicionario_itau <- tribble(
  ~Serie, ~Bloco, ~Unidade, ~Aba_Origem, ~Observacao,
  
  "Itau_NPL90_Total_pct",
  "NPL 90 dias",
  "%",
  "NPL_com_TVM",
  "Taxa NPL 90 dias - Total",
  
  "Itau_NPL90_Brasil_pct",
  "NPL 90 dias",
  "%",
  "NPL_com_TVM",
  "Taxa NPL 90 dias - Brasil",
  
  "Itau_NPL90_PF_Brasil_pct",
  "NPL 90 dias",
  "%",
  "NPL_com_TVM",
  "Taxa NPL 90 dias - Pessoas Fisicas Brasil",
  
  "Itau_NPL90_MPME_Brasil_pct",
  "NPL 90 dias",
  "%",
  "NPL_com_TVM",
  "Taxa NPL 90 dias - Micro, Pequenas e Medias Empresas Brasil",
  
  "Itau_NPL90_Grandes_Empresas_Brasil_pct",
  "NPL 90 dias",
  "%",
  "NPL_com_TVM",
  "Taxa NPL 90 dias - Grandes Empresas Brasil",
  
  "Itau_Carteira_PF_Total_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo da carteira PF; NAO e taxa de inadimplencia",
  
  "Itau_Carteira_Cartao_Credito_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo de Cartao de Credito; NAO e taxa de inadimplencia",
  
  "Itau_Carteira_Credito_Pessoal_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo de Credito Pessoal; NAO e taxa de inadimplencia",
  
  "Itau_Carteira_Credito_Consignado_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo de Credito Consignado; NAO e taxa de inadimplencia",
  
  "Itau_Carteira_Veiculos_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo de Veiculos; NAO e taxa de inadimplencia",
  
  "Itau_Carteira_Credito_Imobiliario_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo de Credito Imobiliario; NAO e taxa de inadimplencia",
  
  "Itau_Carteira_MPME_R_milhoes",
  "Carteira por produto/segmento",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "Saldo da carteira MPME; NAO e taxa de inadimplencia",
  
  "Itau_Cobertura_Estagio3_PF_Brasil_pct",
  "Cobertura Estagio 3",
  "%",
  "Tabela 4966",
  "Provisao Estagio 3 / Carteira Estagio 3 - PF Brasil",
  
  "Itau_Cobertura_Estagio3_PJ_Brasil_pct",
  "Cobertura Estagio 3",
  "%",
  "Tabela 4966",
  "Provisao Estagio 3 / Carteira Estagio 3 - PJ Brasil",
  
  "Itau_Cobertura_Estagio3_Brasil_pct",
  "Cobertura Estagio 3",
  "%",
  "Tabela 4966",
  "Provisao Estagio 3 / Carteira Estagio 3 - Brasil",
  
  "Itau_Cobertura_Estagio3_America_Latina_pct",
  "Cobertura Estagio 3",
  "%",
  "Tabela 4966",
  "Provisao Estagio 3 / Carteira Estagio 3 - America Latina",
  
  "Itau_Cobertura_Estagio3_Total_pct",
  "Cobertura Estagio 3",
  "%",
  "Tabela 4966",
  "Provisao Estagio 3 / Carteira Estagio 3 - Total",
  
  "Itau_NPL_Creation_Total_R_milhoes",
  "NPL Creation",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "NPL Creation - Total",
  
  "Itau_NPL_Creation_Varejo_Brasil_R_milhoes",
  "NPL Creation",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "NPL Creation - Varejo Brasil",
  
  "Itau_NPL_Creation_Atacado_Brasil_R_milhoes",
  "NPL Creation",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "NPL Creation - Atacado Brasil",
  
  "Itau_NPL_Creation_America_Latina_R_milhoes",
  "NPL Creation",
  "R$ milhoes",
  "Carteira_Nova segm. e títul",
  "NPL Creation - America Latina"
)

# Adiciona bloco/unidade a estatisticas e correlacoes
estatisticas_itau <- estatisticas_itau %>%
  left_join(
    dicionario_itau %>%
      select(Serie, Bloco, Unidade),
    by = "Serie"
  ) %>%
  relocate(
    Bloco,
    Unidade,
    .after = Frequencia
  )

correlacao_selic_itau <- correlacao_selic_itau %>%
  left_join(
    dicionario_itau %>%
      select(Serie, Bloco, Unidade),
    by = "Serie"
  ) %>%
  relocate(
    Bloco,
    Unidade,
    .after = Serie
  )

# ------------------------------------------------------------
# 19.11 Exportacao para Excel
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    Estatisticas_Gerais = estatisticas_series,
    Estatisticas_Itau = estatisticas_itau,
    Base_Mensal_Geral = base_estatisticas,
    Base_Itau_Trimestral = base_itau_selic,
    Correlacao_Selic_Itau = correlacao_selic_itau,
    Dicionario_Itau = dicionario_itau
  ),
  path = file.path(
    pasta_saida,
    "estatisticas_descritivas_com_itau.xlsx"
  )
)

readr::write_csv(
  base_itau_selic,
  file.path(
    pasta_saida,
    "base_itau_selic_trimestral.csv"
  ),
  na = ""
)

readr::write_csv(
  correlacao_selic_itau,
  file.path(
    pasta_saida,
    "correlacao_selic_itau.csv"
  ),
  na = ""
)

# ------------------------------------------------------------
# 19.12 Stargazer - series do Itau
# ------------------------------------------------------------

dados_stargazer_itau <- base_itau_trimestral %>%
  select(-Data_Trimestre) %>%
  as.data.frame()

stargazer::stargazer(
  dados_stargazer_itau,
  type = "text",
  title = "Estatisticas descritivas - series do Itau",
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

stargazer::stargazer(
  dados_stargazer_itau,
  type = "latex",
  title = "Estatisticas descritivas - series do Itau",
  label = "tab:estatisticas_itau",
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
    "tabela_stargazer_itau.tex"
  )
)

# ------------------------------------------------------------
# 19.13 Grafico Itau + Selic
# ------------------------------------------------------------
#
# Como ha series em % e series em R$ milhoes, o grafico conjunto
# usa z-score. Cada serie e padronizada por sua propria media e
# desvio-padrao. Os paineis evitam misturar 20 linhas no mesmo
# quadro, mas continuam compondo UMA unica figura.

itau_long <- base_itau_trimestral %>%
  pivot_longer(
    cols = -Data_Trimestre,
    names_to = "Serie",
    values_to = "Valor"
  ) %>%
  left_join(
    dicionario_itau %>%
      select(Serie, Bloco),
    by = "Serie"
  ) %>%
  group_by(Serie) %>%
  mutate(
    Z = z_score(Valor)
  ) %>%
  ungroup() %>%
  filter(!is.na(Z))

selic_itau_periodo <- selic_trimestral %>%
  filter(
    Data_Trimestre >= min(
      base_itau_trimestral$Data_Trimestre,
      na.rm = TRUE
    ),
    Data_Trimestre <= max(
      base_itau_trimestral$Data_Trimestre,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    Z = z_score(Selic_Meta)
  )

blocos_itau <- unique(
  dicionario_itau$Bloco
)

selic_repetida_blocos <- purrr::map_dfr(
  blocos_itau,
  ~ selic_itau_periodo %>%
    transmute(
      Data_Trimestre,
      Serie = "Meta Selic",
      Bloco = .x,
      Z
    )
)

dados_grafico_itau <- bind_rows(
  itau_long %>%
    select(
      Data_Trimestre,
      Serie,
      Bloco,
      Z
    ),
  selic_repetida_blocos
)

grafico_itau_selic <- ggplot(
  dados_grafico_itau,
  aes(
    x = Data_Trimestre,
    y = Z,
    color = Serie,
    group = Serie
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.35
  ) +
  geom_line(
    linewidth = 0.85,
    na.rm = TRUE
  ) +
  geom_point(
    size = 1.5,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ Bloco,
    ncol = 2
  ) +
  labs(
    title = "Meta Selic e indicadores de credito/inadimplencia do Itau",
    subtitle = paste0(
      "Frequencia trimestral; Meta Selic = media do trimestre; ",
      "series padronizadas por z-score"
    ),
    x = NULL,
    y = "Desvios-padrao em relacao a media",
    color = NULL
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%m/%Y"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold"
    )
  ) +
  guides(
    color = guide_legend(
      ncol = 3,
      byrow = TRUE
    )
  )

print(grafico_itau_selic)

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_itau_selic_trimestral_zscore.png"
  ),
  plot = grafico_itau_selic,
  width = 16,
  height = 10,
  dpi = 300
)

# ------------------------------------------------------------
# 19.14 Graficos especificos - NPL 90 e Selic
# ------------------------------------------------------------
#
# Como NPL 90 e Selic estao ambos em %, este grafico pode ser
# apresentado em niveis, sem padronizacao.

grafico_npl90_selic <- base_itau_selic %>%
  select(
    Data_Trimestre,
    Selic_Meta,
    starts_with("Itau_NPL90_")
  ) %>%
  pivot_longer(
    cols = -Data_Trimestre,
    names_to = "Serie",
    values_to = "Valor"
  ) %>%
  filter(!is.na(Valor)) %>%
  ggplot(
    aes(
      x = Data_Trimestre,
      y = Valor,
      color = Serie,
      group = Serie
    )
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  labs(
    title = "Meta Selic e NPL 90 dias do Itau",
    subtitle = "Frequencia trimestral; Meta Selic = media do trimestre",
    x = NULL,
    y = "%",
    color = NULL
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%m/%Y"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_itau_npl90_selic_trimestral.png"
  ),
  plot = grafico_npl90_selic,
  width = 13,
  height = 7,
  dpi = 300
)

# ============================================================
# 20. INTEGRACAO MACRO + ITAU + BETS
# ESTATISTICAS, BASES HARMONIZADAS, CORRELACOES E HEATMAPS
# ============================================================

# ------------------------------------------------------------
# 20.1 Estatisticas descritivas das novas bases anuais
# ------------------------------------------------------------

datas_pib <- as.Date(
  paste0(
    pib_anual$Ano,
    "-01-01"
  )
)

estatisticas_pib <- purrr::map_dfr(
  setdiff(
    names(pib_anual),
    "Ano"
  ),
  ~ calcular_estatisticas(
    nome_serie = .x,
    x = pib_anual[[.x]],
    datas = datas_pib
  )
) %>%
  mutate(
    Frequencia = "Anual",
    Amostra = "Historica disponivel no arquivo",
    .after = Serie
  )

datas_ggr <- as.Date(
  paste0(
    ggr_anual$Ano,
    "-01-01"
  )
)

variaveis_ggr <- setdiff(
  names(ggr_anual),
  c(
    "Ano",
    "GGR_Tipo_Dado"
  )
)

# Estatisticas usando tudo o que existe no arquivo,
# inclusive as projecoes 2026-2030.
estatisticas_ggr_todas <- purrr::map_dfr(
  variaveis_ggr,
  ~ calcular_estatisticas(
    nome_serie = .x,
    x = ggr_anual[[.x]],
    datas = datas_ggr
  )
) %>%
  mutate(
    Frequencia = "Anual",
    Amostra = "2021-2030; inclui projecoes 2026-2030",
    .after = Serie
  )

# Estatisticas somente com os anos observados.
ggr_observado <- ggr_anual %>%
  filter(
    GGR_Tipo_Dado == "Observado"
  )

datas_ggr_observado <- as.Date(
  paste0(
    ggr_observado$Ano,
    "-01-01"
  )
)

estatisticas_ggr_observado <- purrr::map_dfr(
  variaveis_ggr,
  ~ calcular_estatisticas(
    nome_serie = .x,
    x = ggr_observado[[.x]],
    datas = datas_ggr_observado
  )
) %>%
  mutate(
    Frequencia = "Anual",
    Amostra = "Somente observado: 2021-2025",
    .after = Serie
  )

# ------------------------------------------------------------
# 20.2 Tabela unica de estatisticas descritivas
# ------------------------------------------------------------

estatisticas_mensais_pad <- estatisticas_series %>%
  mutate(
    Frequencia = case_when(
      stringr::str_starts(
        Serie,
        "PNAD_"
      ) ~ "Mensal - trimestre movel de 3 meses",
      TRUE ~ "Mensal"
    ),
    Amostra = "Observada",
    .after = Serie
  )

estatisticas_itau_pad <- estatisticas_itau %>%
  select(
    Serie,
    Frequencia,
    N,
    Inicio,
    Fim,
    Media,
    Mediana,
    Variancia,
    Desvio_Padrao
  ) %>%
  mutate(
    Amostra = "Observada",
    .after = Frequencia
  )

estatisticas_todas <- bind_rows(
  estatisticas_mensais_pad %>%
    select(
      Serie,
      Frequencia,
      Amostra,
      N,
      Inicio,
      Fim,
      Media,
      Mediana,
      Variancia,
      Desvio_Padrao
    ),
  estatisticas_itau_pad,
  estatisticas_pib %>%
    select(
      Serie,
      Frequencia,
      Amostra,
      N,
      Inicio,
      Fim,
      Media,
      Mediana,
      Variancia,
      Desvio_Padrao
    ),
  estatisticas_ggr_todas %>%
    select(
      Serie,
      Frequencia,
      Amostra,
      N,
      Inicio,
      Fim,
      Media,
      Mediana,
      Variancia,
      Desvio_Padrao
    )
) %>%
  arrange(
    Frequencia,
    Serie
  )

print(estatisticas_todas)

readr::write_csv(
  estatisticas_todas,
  file.path(
    pasta_saida,
    "estatisticas_descritivas_todas_as_bases.csv"
  ),
  na = ""
)

# Tabela Stargazer com as estatisticas solicitadas.
tabela_estatisticas_stargazer <- estatisticas_todas %>%
  select(
    Serie,
    Frequencia,
    N,
    Media,
    Mediana,
    Variancia,
    Desvio_Padrao
  ) %>%
  as.data.frame()

stargazer::stargazer(
  tabela_estatisticas_stargazer,
  type = "text",
  summary = FALSE,
  rownames = FALSE,
  title = "Estatisticas descritivas de todas as series",
  digits = 3
)

stargazer::stargazer(
  tabela_estatisticas_stargazer,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  title = "Estatisticas descritivas de todas as series",
  label = "tab:estatisticas_todas_series",
  digits = 3,
  out = file.path(
    pasta_saida,
    "tabela_estatisticas_todas_series.tex"
  )
)

stargazer::stargazer(
  tabela_estatisticas_stargazer,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Estatisticas descritivas de todas as series",
  digits = 3,
  out = file.path(
    pasta_saida,
    "tabela_estatisticas_todas_series.html"
  )
)

# ------------------------------------------------------------
# 20.3 Base trimestral harmonizada
# ------------------------------------------------------------
#
# Regra temporal:
#
# 1) Series mensais comuns:
#    media dos tres meses do trimestre.
#
# 2) PNAD:
#    os CSVs sao trimestres moveis, observados mensalmente.
#    Para evitar usar janelas sobrepostas dentro do mesmo trimestre,
#    usamos apenas os pontos terminados em MAR, JUN, SET e DEZ.
#
# 3) Itau:
#    ja esta em frequencia trimestral.
#
# 4) Selic:
#    media dos valores diarios dentro do trimestre.

variaveis_pnad <- c(
  "PNAD_Taxa_Desocupacao_pct",
  "PNAD_Rendimento_Medio_R"
)

macro_mensal_sem_pnad <- base_estatisticas %>%
  select(
    -any_of(
      variaveis_pnad
    )
  )

macro_trimestral <- macro_mensal_sem_pnad %>%
  mutate(
    Data_Trimestre = lubridate::floor_date(
      Data,
      unit = "quarter"
    )
  ) %>%
  select(
    -Data
  ) %>%
  group_by(
    Data_Trimestre
  ) %>%
  summarise(
    across(
      where(is.numeric),
      media_na
    ),
    .groups = "drop"
  )

pnad_trimestral <- purrr::reduce(
  list(
    desemprego_pnad,
    rendimento_pnad
  ),
  full_join,
  by = "Data"
) %>%
  filter(
    lubridate::month(Data) %in%
      c(
        3,
        6,
        9,
        12
      )
  ) %>%
  mutate(
    Data_Trimestre = lubridate::floor_date(
      Data,
      unit = "quarter"
    )
  ) %>%
  select(
    -Data
  ) %>%
  arrange(
    Data_Trimestre
  )

base_trimestral_completa <- purrr::reduce(
  list(
    macro_trimestral,
    pnad_trimestral,
    base_itau_trimestral
  ),
  full_join,
  by = "Data_Trimestre"
) %>%
  arrange(
    Data_Trimestre
  )

readr::write_csv(
  base_trimestral_completa,
  file.path(
    pasta_saida,
    "base_trimestral_completa.csv"
  ),
  na = ""
)

# ------------------------------------------------------------
# 20.4 Base anual harmonizada
# ------------------------------------------------------------
#
# A base anual e a unica frequencia em que o GGR pode entrar
# sem repetir artificialmente um valor anual em todos os meses.
#
# Para a matriz EMPIRICA:
# - GGR usa apenas 2021-2025;
# - 2026-2030 ficam fora por serem projecoes;
# - PIB entra nos anos disponiveis no arquivo.
#
# Como o PIB historico fornecido termina antes do periodo recente,
# algumas correlacoes PIB x GGR/Itau podem ficar NA por falta de
# sobreposicao temporal. Isso e correto e preferivel a inventar dados.

base_anual_macro <- base_trimestral_completa %>%
  mutate(
    Ano = lubridate::year(
      Data_Trimestre
    )
  ) %>%
  select(
    -Data_Trimestre
  ) %>%
  group_by(
    Ano
  ) %>%
  summarise(
    across(
      where(is.numeric),
      media_na
    ),
    .groups = "drop"
  )

ggr_observado_cor <- ggr_observado %>%
  select(
    -GGR_Tipo_Dado
  )

base_anual_completa <- base_anual_macro %>%
  full_join(
    pib_anual,
    by = "Ano"
  ) %>%
  full_join(
    ggr_observado_cor,
    by = "Ano"
  ) %>%
  arrange(
    Ano
  )

readr::write_csv(
  base_anual_completa,
  file.path(
    pasta_saida,
    "base_anual_completa.csv"
  ),
  na = ""
)

# ------------------------------------------------------------
# 20.5 Funcoes para matriz de correlacao
# ------------------------------------------------------------

calcular_correlacoes <- function(
    base,
    coluna_tempo,
    min_pares = 5) {
  
  dados <- base %>%
    select(
      -all_of(
        coluna_tempo
      )
    ) %>%
    select(
      where(is.numeric)
    )
  
  # Remove series completamente vazias.
  dados <- dados %>%
    select(
      where(
        ~ any(!is.na(.x))
      )
    )
  
  nomes <- names(dados)
  
  matriz_cor <- cor(
    dados,
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  matriz_n <- matrix(
    NA_integer_,
    nrow = length(nomes),
    ncol = length(nomes),
    dimnames = list(
      nomes,
      nomes
    )
  )
  
  for (i in seq_along(nomes)) {
    for (j in seq_along(nomes)) {
      
      matriz_n[i, j] <- sum(
        stats::complete.cases(
          dados[[nomes[i]]],
          dados[[nomes[j]]]
        )
      )
    }
  }
  
  # Versao recomendada para interpretacao:
  # pares com menos de min_pares observacoes ficam como NA.
  matriz_cor_filtrada <- matriz_cor
  
  matriz_cor_filtrada[
    matriz_n < min_pares
  ] <- NA_real_
  
  list(
    cor_bruta = matriz_cor,
    cor = matriz_cor_filtrada,
    n = matriz_n
  )
}

matriz_para_df <- function(matriz) {
  
  matriz %>%
    as.data.frame() %>%
    tibble::rownames_to_column(
      "Variavel"
    )
}

# ------------------------------------------------------------
# 20.6 Matrizes de correlacao por frequencia
# ------------------------------------------------------------

# MENSAL:
# inclui Selic, credito/inadimplencia BCB, Serasa e PNAD.
corr_mensal <- calcular_correlacoes(
  base = base_estatisticas,
  coluna_tempo = "Data",
  min_pares = 12
)

# TRIMESTRAL:
# inclui macro mensal agregada + PNAD sem sobreposicao + Itau.
corr_trimestral <- calcular_correlacoes(
  base = base_trimestral_completa,
  coluna_tempo = "Data_Trimestre",
  min_pares = 5
)

# ANUAL:
# inclui base macro anual + PIB + GGR observado.
corr_anual <- calcular_correlacoes(
  base = base_anual_completa,
  coluna_tempo = "Ano",
  min_pares = 5
)

# Salva matrizes principais e numero de pares.
write.csv(
  corr_mensal$cor,
  file = file.path(
    pasta_saida,
    "matriz_correlacao_mensal.csv"
  ),
  row.names = TRUE
)

write.csv(
  corr_mensal$n,
  file = file.path(
    pasta_saida,
    "matriz_n_pares_mensal.csv"
  ),
  row.names = TRUE
)

write.csv(
  corr_trimestral$cor,
  file = file.path(
    pasta_saida,
    "matriz_correlacao_trimestral.csv"
  ),
  row.names = TRUE
)

write.csv(
  corr_trimestral$n,
  file = file.path(
    pasta_saida,
    "matriz_n_pares_trimestral.csv"
  ),
  row.names = TRUE
)

write.csv(
  corr_anual$cor,
  file = file.path(
    pasta_saida,
    "matriz_correlacao_anual.csv"
  ),
  row.names = TRUE
)

write.csv(
  corr_anual$n,
  file = file.path(
    pasta_saida,
    "matriz_n_pares_anual.csv"
  ),
  row.names = TRUE
)

# ------------------------------------------------------------
# 20.7 Heatmap generico
# ------------------------------------------------------------

criar_heatmap_correlacao <- function(
    matriz,
    titulo,
    subtitulo,
    arquivo) {
  
  ordem <- colnames(
    matriz
  )
  
  cor_long <- matriz %>%
    as.data.frame() %>%
    tibble::rownames_to_column(
      "Variavel_1"
    ) %>%
    tidyr::pivot_longer(
      cols = -Variavel_1,
      names_to = "Variavel_2",
      values_to = "Correlacao"
    ) %>%
    mutate(
      i = match(
        Variavel_1,
        ordem
      ),
      j = match(
        Variavel_2,
        ordem
      ),
      Variavel_1 = factor(
        Variavel_1,
        levels = rev(ordem)
      ),
      Variavel_2 = factor(
        Variavel_2,
        levels = ordem
      )
    ) %>%
    # Somente triangulo inferior:
    # elimina a duplicacao visual da matriz simetrica.
    filter(
      i >= j
    )
  
  p <- ggplot(
    cor_long,
    aes(
      x = Variavel_2,
      y = Variavel_1,
      fill = Correlacao
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.15
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
      title = titulo,
      subtitle = subtitulo,
      x = NULL,
      y = NULL,
      fill = "Correlacao"
    ) +
    theme_minimal(
      base_size = 10
    ) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(
        face = "bold"
      ),
      axis.text.x = element_text(
        angle = 60,
        hjust = 1,
        size = ifelse(
          length(ordem) > 20,
          6,
          8
        )
      ),
      axis.text.y = element_text(
        size = ifelse(
          length(ordem) > 20,
          6,
          8
        )
      )
    )
  
  # Em matrizes menores, escreve o coeficiente dentro da celula.
  if (length(ordem) <= 16) {
    
    p <- p +
      geom_text(
        aes(
          label = ifelse(
            is.na(Correlacao),
            "",
            sprintf(
              "%.2f",
              Correlacao
            )
          )
        ),
        size = 2.8
      )
  }
  
  largura <- max(
    10,
    0.42 * length(ordem) + 4
  )
  
  altura <- max(
    8,
    0.38 * length(ordem) + 3
  )
  
  ggsave(
    filename = file.path(
      pasta_saida,
      arquivo
    ),
    plot = p,
    width = largura,
    height = altura,
    dpi = 300,
    limitsize = FALSE
  )
  
  print(p)
  
  invisible(p)
}

# ------------------------------------------------------------
# 20.8 Heatmaps
# ------------------------------------------------------------

heatmap_mensal <- criar_heatmap_correlacao(
  matriz = corr_mensal$cor,
  titulo = "Matriz de correlacao - frequencia mensal",
  subtitulo = paste0(
    "Pearson; inclui BCB, Serasa e PNAD. ",
    "Correlacoes com menos de 12 pares foram omitidas."
  ),
  arquivo = "heatmap_correlacao_mensal.png"
)

heatmap_trimestral <- criar_heatmap_correlacao(
  matriz = corr_trimestral$cor,
  titulo = "Matriz de correlacao - frequencia trimestral",
  subtitulo = paste0(
    "Pearson; macro agregada, PNAD em mar/jun/set/dez e Itau. ",
    "Correlacoes com menos de 5 pares foram omitidas."
  ),
  arquivo = "heatmap_correlacao_trimestral.png"
)

heatmap_anual <- criar_heatmap_correlacao(
  matriz = corr_anual$cor,
  titulo = "Matriz de correlacao - frequencia anual",
  subtitulo = paste0(
    "Pearson; inclui PIB e GGR observado (2021-2025). ",
    "Projecoes do GGR nao entram; pares com N < 5 foram omitidos."
  ),
  arquivo = "heatmap_correlacao_anual.png"
)

# ------------------------------------------------------------
# 20.9 Heatmap compacto - variaveis centrais do trabalho
# ------------------------------------------------------------
#
# O heatmap completo pode ficar grande. Esta versao seleciona
# variaveis macro/credito mais diretamente ligadas ao mecanismo
# de inadimplencia.

vars_centrais_trimestral <- c(
  "Selic_Meta",
  "Taxa_Media_Juros_Credito",
  "Inadimplencia_PF_Total",
  "Serasa_Populacao_Adulta_pct",
  "PNAD_Taxa_Desocupacao_pct",
  "PNAD_Rendimento_Medio_R",
  "Itau_NPL90_Total_pct",
  "Itau_NPL90_PF_Brasil_pct",
  "Itau_NPL90_MPME_Brasil_pct",
  "Itau_NPL_Creation_Total_R_milhoes"
)

vars_centrais_trimestral <- intersect(
  vars_centrais_trimestral,
  colnames(
    corr_trimestral$cor
  )
)

matriz_central_trimestral <- corr_trimestral$cor[
  vars_centrais_trimestral,
  vars_centrais_trimestral,
  drop = FALSE
]

heatmap_central <- criar_heatmap_correlacao(
  matriz = matriz_central_trimestral,
  titulo = "Correlacoes centrais: macroeconomia, credito e inadimplencia",
  subtitulo = paste0(
    "Frequencia trimestral; Pearson. ",
    "A Meta Selic e as series mensais foram harmonizadas ao trimestre."
  ),
  arquivo = "heatmap_correlacao_variaveis_centrais.png"
)

# ------------------------------------------------------------
# 20.10 Excel final com todas as bases e resultados
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    Estatisticas_Todas = estatisticas_todas,
    Estat_GGR_Observado = estatisticas_ggr_observado,
    Base_Mensal = base_estatisticas,
    Base_Trimestral = base_trimestral_completa,
    Base_Anual = base_anual_completa,
    PIB_Anual = pib_anual,
    GGR_Anual = ggr_anual,
    PNAD_Desemprego = desemprego_pnad,
    PNAD_Rendimento = rendimento_pnad,
    Corr_Mensal = matriz_para_df(
      corr_mensal$cor
    ),
    N_Mensal = matriz_para_df(
      corr_mensal$n
    ),
    Corr_Trimestral = matriz_para_df(
      corr_trimestral$cor
    ),
    N_Trimestral = matriz_para_df(
      corr_trimestral$n
    ),
    Corr_Anual = matriz_para_df(
      corr_anual$cor
    ),
    N_Anual = matriz_para_df(
      corr_anual$n
    ),
    Corr_Anual_Bruta = matriz_para_df(
      corr_anual$cor_bruta
    ),
    Correlacao_Selic_Itau = correlacao_selic_itau,
    Dicionario_Itau = dicionario_itau
  ),
  path = file.path(
    pasta_saida,
    "analise_completa_macro_credito_inadimplencia.xlsx"
  )
)

# ------------------------------------------------------------
# 20.11 Graficos individuais das novas series macro
# ------------------------------------------------------------

novas_series_mensais <- purrr::reduce(
  list(
    desemprego_pnad,
    rendimento_pnad
  ),
  full_join,
  by = "Data"
) %>%
  pivot_longer(
    cols = -Data,
    names_to = "Serie",
    values_to = "Valor"
  )

purrr::walk(
  unique(
    novas_series_mensais$Serie
  ),
  function(serie_atual) {
    
    p <- novas_series_mensais %>%
      filter(
        Serie == serie_atual,
        !is.na(Valor)
      ) %>%
      ggplot(
        aes(
          x = Data,
          y = Valor
        )
      ) +
      geom_line(
        linewidth = 0.8
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
      theme_minimal(
        base_size = 11
      ) +
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
          nome_seguro(
            serie_atual
          ),
          ".png"
        )
      ),
      plot = p,
      width = 10,
      height = 5.5,
      dpi = 300
    )
  }
)

# GGR: observado e projecoes permanecem identificados no dado.
ggr_long <- ggr_anual %>%
  pivot_longer(
    cols = all_of(
      variaveis_ggr
    ),
    names_to = "Serie",
    values_to = "Valor"
  )

grafico_ggr <- ggplot(
  ggr_long,
  aes(
    x = Ano,
    y = Valor,
    color = Serie,
    linetype = GGR_Tipo_Dado,
    group = interaction(
      Serie,
      GGR_Tipo_Dado
    )
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title = "GGR das apostas no Brasil",
    subtitle = "2026-2030 sao projecoes no arquivo fornecido",
    x = NULL,
    y = "R$ bilhoes",
    color = NULL,
    linetype = "Tipo de dado"
  ) +
  scale_x_continuous(
    breaks = sort(
      unique(
        ggr_anual$Ano
      )
    )
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    pasta_saida,
    "grafico_ggr_bets_brasil.png"
  ),
  plot = grafico_ggr,
  width = 11,
  height = 6,
  dpi = 300
)

cat("\n==============================\n")
cat("CONCLUIDO\n")
cat("==============================\n")
cat(
  "Arquivos salvos em:\n",
  pasta_saida,
  "\n"
)