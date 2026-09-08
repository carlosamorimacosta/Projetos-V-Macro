# ============================================================
# COMPOSIÇÃO DA POPULAÇÃO POR CLASSE DE RENDA E ESTADO
# PNAD CONTÍNUA - 2025
#
# Resultado:
# Estado | A (%) | B (%) | C (%) | D (%) | E (%)
# ============================================================


# ------------------------------------------------------------
# 0. PACOTES
# ------------------------------------------------------------

pacotes <- c(
  "PNADcIBGE",
  "survey",
  "dplyr",
  "writexl"
)

novos <- pacotes[
  !(pacotes %in% installed.packages()[, "Package"])
]

if(length(novos) > 0){
  install.packages(novos)
}

library(PNADcIBGE)
library(survey)
library(dplyr)
library(writexl)


# Para evitar problemas em UFs com estratos específicos
options(survey.lonely.psu = "adjust")


# ------------------------------------------------------------
# 1. PARÂMETROS
# ------------------------------------------------------------

ano <- 2025

# Salário mínimo de 2025
salario_minimo <- 1518


# ------------------------------------------------------------
# 2. BAIXAR PNAD CONTÍNUA ANUAL
# ------------------------------------------------------------

pnad <- get_pnadc(
  
  year = ano,
  
  # Primeira visita
  interview = 1,
  
  selected = FALSE,
  
  # Variáveis necessárias
  vars = c(
    "UF",
    "VD5008"
  ),
  
  # Manter nomes das UFs
  labels = TRUE,
  
  # Não precisamos deflacionar porque
  # as classes serão definidas em SM de 2025
  deflator = FALSE,
  
  # MUITO IMPORTANTE:
  # cria objeto com desenho amostral da PNAD
  design = TRUE
  
)


# ------------------------------------------------------------
# 3. VERIFICAR VARIÁVEIS
# ------------------------------------------------------------

names(pnad$variables)

summary(pnad$variables$VD5008)


# ------------------------------------------------------------
# 4. CRIAR RENDA DOMICILIAR PER CAPITA
# ------------------------------------------------------------

pnad$variables <- pnad$variables %>%
  
  mutate(
    
    renda_pc = as.numeric(VD5008),
    
    renda_sm = renda_pc / salario_minimo
    
  )


# ------------------------------------------------------------
# 5. CRIAR CLASSES DE RENDA
# ------------------------------------------------------------
#
# E = até 0,5 SM
# D = > 0,5 até 1 SM
# C = > 1 até 2 SM
# B = > 2 até 5 SM
# A = > 5 SM
#
# Cada variável abaixo será:
#
# 1 = pessoa pertence à classe
# 0 = pessoa não pertence à classe
#
# ------------------------------------------------------------

pnad$variables <- pnad$variables %>%
  
  mutate(
    
    # -------------------------
    # Classe E
    # -------------------------
    
    E = case_when(
      
      is.na(renda_sm) ~ NA_real_,
      
      renda_sm <= 0.5 ~ 1,
      
      TRUE ~ 0
      
    ),
    
    
    # -------------------------
    # Classe D
    # -------------------------
    
    D = case_when(
      
      is.na(renda_sm) ~ NA_real_,
      
      renda_sm > 0.5 &
        renda_sm <= 1 ~ 1,
      
      TRUE ~ 0
      
    ),
    
    
    # -------------------------
    # Classe C
    # -------------------------
    
    C = case_when(
      
      is.na(renda_sm) ~ NA_real_,
      
      renda_sm > 1 &
        renda_sm <= 2 ~ 1,
      
      TRUE ~ 0
      
    ),
    
    
    # -------------------------
    # Classe B
    # -------------------------
    
    B = case_when(
      
      is.na(renda_sm) ~ NA_real_,
      
      renda_sm > 2 &
        renda_sm <= 5 ~ 1,
      
      TRUE ~ 0
      
    ),
    
    
    # -------------------------
    # Classe A
    # -------------------------
    
    A = case_when(
      
      is.na(renda_sm) ~ NA_real_,
      
      renda_sm > 5 ~ 1,
      
      TRUE ~ 0
      
    )
    
  )


# ------------------------------------------------------------
# 6. CALCULAR % DA POPULAÇÃO EM CADA CLASSE POR UF
# ------------------------------------------------------------
#
# svyby + svymean utiliza os pesos amostrais da PNAD.
#
# NÃO é uma média simples da amostra.
#
# Exemplo:
# A = 0,15 significa que aproximadamente 15%
# da população daquela UF está na classe A.
#
# ------------------------------------------------------------

resultado <- svyby(
  
  ~ A + B + C + D + E,
  
  ~ UF,
  
  design = pnad,
  
  FUN = svymean,
  
  na.rm = TRUE,
  
  vartype = NULL
  
)


# Transformar em data.frame
resultado <- as.data.frame(resultado)


# ------------------------------------------------------------
# 7. TRANSFORMAR PROPORÇÕES EM PERCENTUAIS
# ------------------------------------------------------------

resultado_final <- resultado %>%
  
  mutate(
    
    Classe_A_pct = A * 100,
    
    Classe_B_pct = B * 100,
    
    Classe_C_pct = C * 100,
    
    Classe_D_pct = D * 100,
    
    Classe_E_pct = E * 100
    
  ) %>%
  
  select(
    
    Estado = UF,
    
    Classe_A_pct,
    
    Classe_B_pct,
    
    Classe_C_pct,
    
    Classe_D_pct,
    
    Classe_E_pct
    
  )


# ------------------------------------------------------------
# 8. ARREDONDAR
# ------------------------------------------------------------

resultado_final <- resultado_final %>%
  
  mutate(
    
    across(
      
      starts_with("Classe_"),
      
      ~ round(.x, 2)
      
    )
    
  )


# ------------------------------------------------------------
# 9. VERIFICAR SE A + B + C + D + E = 100%
# ------------------------------------------------------------

resultado_final <- resultado_final %>%
  
  mutate(
    
    Soma_pct =
      
      Classe_A_pct +
      Classe_B_pct +
      Classe_C_pct +
      Classe_D_pct +
      Classe_E_pct
    
  )


# ------------------------------------------------------------
# 10. ORDENAR
# ------------------------------------------------------------

resultado_final <- resultado_final %>%
  
  arrange(Estado)


# ------------------------------------------------------------
# 11. MOSTRAR RESULTADO
# ------------------------------------------------------------

print(
  resultado_final,
  n = 27
)


# ------------------------------------------------------------
# 12. VER APENAS SÃO PAULO
# ------------------------------------------------------------

resultado_final %>%
  
  filter(
    grepl(
      "Paulo",
      Estado,
      ignore.case = TRUE
    )
  )


# ------------------------------------------------------------
# 13. EXPORTAR PARA EXCEL
# ------------------------------------------------------------

write_xlsx(
  
  resultado_final,
  
  "composicao_classes_renda_estados_2025.xlsx"
  
)


# ------------------------------------------------------------
# 14. EXPORTAR PARA CSV
# ------------------------------------------------------------

write.csv(
  
  resultado_final,
  
  "composicao_classes_renda_estados_2025.csv",
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
  
)