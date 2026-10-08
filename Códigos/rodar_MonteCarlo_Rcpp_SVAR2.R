
rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

pacotes <- c("Rcpp","RcppArmadillo","readr","dplyr","tidyr","ggplot2","openxlsx","purrr")
ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(ausentes) > 0) install.packages(ausentes, dependencies = TRUE)

suppressPackageStartupMessages({
  library(Rcpp)
  library(RcppArmadillo)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(openxlsx)
  library(purrr)
})

DIR_OUT_SVAR <- "C:/Users/carlo/Downloads/Projetos V - Macro/Output do SVAR - itaú"

ARQ_BASE <- file.path(DIR_OUT_SVAR, "base_SVAR_Itau_Selic.csv")
ARQ_CPP  <- "C:/Users/carlo/Downloads/Projetos V - Macro/Códigos/SVAR_MonteCarlo_Rcpp.cpp"

DIR_MC <- file.path(DIR_OUT_SVAR, "MonteCarlo_Rcpp")
if (!dir.exists(DIR_MC)) dir.create(DIR_MC, recursive = TRUE)

P_VAR <- 2
H <- 24
N_BOOT <- 20000
N_BETS_MC <- 10000
BETS_NOISE_SCENARIOS <- c(0.05, 0.10, 0.20)
N_SIGN_ACCEPT <- 5000
MAX_ROTATIONS <- 500000
CUMULATIVE_INAD <- TRUE

# NA = choque estrutural de 1 desvio-padrão.
# Use 10 para normalizar o impacto contemporâneo em Bets para ~+10%.
TARGET_BETS_IMPACT <- NA_real_

SEED_BOOT <- 20261007
SEED_BETS <- 20261008
SEED_SIGN <- 20261009

if (!file.exists(ARQ_CPP)) stop("Arquivo C++ não encontrado: ", ARQ_CPP)
Rcpp::sourceCpp(ARQ_CPP, rebuild = TRUE, verbose = FALSE)

if (!file.exists(ARQ_BASE)) stop("Base do SVAR não encontrada: ", ARQ_BASE)

base <- readr::read_csv(ARQ_BASE, show_col_types = FALSE)

vars <- c("bets","ibc","ipca","selic","inad")

faltantes <- setdiff(c("data", vars), names(base))
if (length(faltantes) > 0) {
  stop("Colunas ausentes: ", paste(faltantes, collapse = ", "))
}

base <- base %>%
  dplyr::select(data, dplyr::all_of(vars)) %>%
  tidyr::drop_na()

Y <- as.matrix(base[, vars, drop = FALSE])
storage.mode(Y) <- "double"

IDX_BETS <- which(vars == "bets") - 1L
IDX_INAD <- which(vars == "inad") - 1L

fit_cpp <- fit_var_cpp(Y, P_VAR)

cat("\nMaior raiz C++:", round(fit_cpp$max_root, 6), "\n")
if (fit_cpp$max_root >= 1) stop("VAR(2) instável no C++.")

irf_base <- recursive_irf_cpp(
  Y = Y,
  p = P_VAR,
  H = H,
  shock_idx = IDX_BETS,
  response_idx = IDX_INAD,
  cumulative = CUMULATIVE_INAD,
  target_impact = TARGET_BETS_IMPACT
)

irf_base_df <- tibble(
  horizonte = 0:H,
  irf = as.numeric(irf_base)
)

write.csv(
  irf_base_df,
  file.path(DIR_MC, "IRF_base_Cpp_Bets_Inad.csv"),
  row.names = FALSE
)

quantile_safe <- function(x, probs) {
  stats::quantile(
    x[is.finite(x)],
    probs = probs,
    na.rm = TRUE,
    names = FALSE,
    type = 8
  )
}

resumir_draws <- function(draws, nome) {
  if (is.null(draws) || nrow(draws) == 0) {
    stop("Nenhuma simulação válida em ", nome)
  }

  H_local <- ncol(draws) - 1L

  purrr::map_dfr(
    0:H_local,
    function(h) {
      x <- draws[, h + 1L]

      q68 <- quantile_safe(x, c(0.16, 0.84))
      q90 <- quantile_safe(x, c(0.05, 0.95))
      q95 <- quantile_safe(x, c(0.025, 0.975))

      tibble(
        metodo = nome,
        horizonte = h,
        media = mean(x, na.rm = TRUE),
        mediana = median(x, na.rm = TRUE),
        p16 = q68[1],
        p84 = q68[2],
        p05 = q90[1],
        p95 = q90[2],
        p025 = q95[1],
        p975 = q95[2],
        prob_positivo = mean(x > 0, na.rm = TRUE),
        prob_negativo = mean(x < 0, na.rm = TRUE),
        significativo_90 = q90[1] > 0 | q90[2] < 0,
        significativo_95 = q95[1] > 0 | q95[2] < 0
      )
    }
  )
}

plotar_resumo <- function(df, titulo, subtitulo, arquivo) {
  g <- ggplot(df, aes(x = horizonte, y = mediana)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.20) +
    geom_line(linewidth = 0.9) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Meses após o choque",
      y = "Efeito acumulado sobre a inadimplência (p.p.)"
    ) +
    theme_minimal(base_size = 12)

  ggsave(arquivo, g, width = 9, height = 5.5, dpi = 300)
  g
}

# =============================================================================
# 1. MONTE CARLO / BOOTSTRAP RESIDUAL
# =============================================================================

boot <- residual_bootstrap_svar_cpp(
  Y = Y,
  p = P_VAR,
  H = H,
  B = N_BOOT,
  shock_idx = IDX_BETS,
  response_idx = IDX_INAD,
  cumulative = CUMULATIVE_INAD,
  target_impact = TARGET_BETS_IMPACT,
  stability_cutoff = 0.999999,
  seed = SEED_BOOT
)

cat(
  "\nBOOTSTRAP:",
  "\nSolicitadas:", boot$requested,
  "\nAceitas:", boot$accepted,
  "\nInstáveis:", boot$unstable,
  "\nFalhas:", boot$failed,
  "\nTaxa de aceitação:", round(100 * boot$acceptance_rate, 2), "%\n"
)

boot_summary <- resumir_draws(
  as.matrix(boot$draws),
  "Bootstrap residual SVAR(2)"
) %>%
  left_join(
    irf_base_df %>% rename(irf_pontual = irf),
    by = "horizonte"
  )

write.csv(
  boot_summary,
  file.path(DIR_MC, "MonteCarlo_bootstrap_IRF_Bets_Inad.csv"),
  row.names = FALSE
)

plotar_resumo(
  boot_summary,
  "Monte Carlo / bootstrap residual — Bets → Inadimplência",
  paste0("SVAR(2), ", format(N_BOOT, big.mark = "."), " replicações | IC 90%"),
  file.path(DIR_MC, "MonteCarlo_bootstrap_IRF_Bets_Inad.png")
)

# =============================================================================
# 2. INCERTEZA DA SÉRIE TRANSFORMADA DE BETS
# =============================================================================

bets_uncert_summaries <- list()

for (i in seq_along(BETS_NOISE_SCENARIOS)) {
  tau <- BETS_NOISE_SCENARIOS[i]

  mc_bets <- bets_measurement_uncertainty_cpp(
    Y = Y,
    p = P_VAR,
    H = H,
    M = N_BETS_MC,
    bets_idx = IDX_BETS,
    response_idx = IDX_INAD,
    noise_sd_fraction = tau,
    cumulative = CUMULATIVE_INAD,
    target_impact = TARGET_BETS_IMPACT,
    stability_cutoff = 0.999999,
    seed = SEED_BETS + i
  )

  cat(
    "\nBETS tau =", tau,
    "| aceitas:", mc_bets$accepted,
    "/", mc_bets$requested,
    "| taxa:", round(100 * mc_bets$acceptance_rate, 2), "%\n"
  )

  resumo_tau <- resumir_draws(
    as.matrix(mc_bets$draws),
    paste0("Erro de mensuração Bets — ", round(100 * tau), "% do DP")
  ) %>%
    mutate(
      tau = tau,
      noise_sd = mc_bets$noise_sd,
      .before = 1
    )

  bets_uncert_summaries[[i]] <- resumo_tau
}

bets_uncert_summary <- bind_rows(bets_uncert_summaries)

write.csv(
  bets_uncert_summary,
  file.path(DIR_MC, "Incerteza_Bets_IRF_resumo.csv"),
  row.names = FALSE
)

g_uncert <- ggplot(
  bets_uncert_summary,
  aes(
    x = horizonte,
    y = mediana,
    linetype = factor(tau)
  )
) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Sensibilidade da IRF à incerteza de mensuração de Bets",
    subtitle = "Ruído aditivo sobre 100×Δlog(GGR real)",
    x = "Meses após o choque",
    y = "Mediana do efeito acumulado (p.p.)",
    linetype = "DP erro / DP Bets"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(DIR_MC, "Incerteza_Bets_comparacao.png"),
  g_uncert,
  width = 9,
  height = 5.5,
  dpi = 300
)

# =============================================================================
# 3. SIGN RESTRICTIONS
# =============================================================================
#
# NÃO imponha inad > 0 se o objetivo é testar se Bets aumenta inadimplência.
#
# Esquema cauteloso:
#   Bets > 0 no impacto e em h=1.
#   IBC, IPCA, Selic e Inad ficam LIVRES.
#
# Se houver base teórica/literatura para outros sinais, acrescente-os aqui.
# =============================================================================

RESTRICOES <- tribble(
  ~variavel, ~horizonte, ~sinal,
  "bets",     0L,         1L,
  "bets",     1L,         1L
) %>%
  mutate(
    var_idx = match(variavel, vars) - 1L
  )

signres <- sign_restrictions_cpp(
  Y = Y,
  p = P_VAR,
  H = H,
  restrict_var_idx = as.integer(RESTRICOES$var_idx),
  restrict_h_idx = as.integer(RESTRICOES$horizonte),
  restrict_sign = as.integer(RESTRICOES$sinal),
  response_idx = IDX_INAD,
  n_accept = N_SIGN_ACCEPT,
  max_rotations = MAX_ROTATIONS,
  cumulative_response = CUMULATIVE_INAD,
  target_impact = TARGET_BETS_IMPACT,
  tol = 1e-10,
  seed = SEED_SIGN
)

cat(
  "\nSIGN RESTRICTIONS:",
  "\nRotações:", signres$rotations,
  "\nAceitas:", signres$accepted,
  "\nTaxa aceitas/rotação:",
  round(100 * signres$acceptance_rate_per_rotation, 2), "%\n"
)

if (signres$accepted == 0) {
  stop("Nenhuma rotação satisfez as sign restrictions.")
}

sign_summary <- resumir_draws(
  as.matrix(signres$draws),
  "Sign restrictions"
)

write.csv(
  sign_summary,
  file.path(DIR_MC, "SignRestrictions_IRF_Bets_Inad.csv"),
  row.names = FALSE
)

impact_df <- as.data.frame(signres$impact_vectors)
names(impact_df) <- vars

write.csv(
  impact_df,
  file.path(DIR_MC, "SignRestrictions_impact_vectors.csv"),
  row.names = FALSE
)

plotar_resumo(
  sign_summary,
  "Sign restrictions — Bets → Inadimplência",
  paste0(signres$accepted, " identificações aceitas | inadimplência não restringida | IC 90%"),
  file.path(DIR_MC, "SignRestrictions_IRF_Bets_Inad.png")
)

# =============================================================================
# 4. COMPARAÇÃO
# =============================================================================

comparacao_metodos <- bind_rows(
  boot_summary %>%
    select(
      metodo, horizonte, mediana, p05, p95,
      prob_positivo, significativo_90
    ),

  bets_uncert_summary %>%
    filter(abs(tau - 0.10) < 1e-12) %>%
    select(
      metodo, horizonte, mediana, p05, p95,
      prob_positivo, significativo_90
    ),

  sign_summary %>%
    select(
      metodo, horizonte, mediana, p05, p95,
      prob_positivo, significativo_90
    )
)

write.csv(
  comparacao_metodos,
  file.path(DIR_MC, "Comparacao_MonteCarlo_Bets_SignRestrictions.csv"),
  row.names = FALSE
)

g_comp <- ggplot(
  comparacao_metodos,
  aes(
    x = horizonte,
    y = mediana,
    linetype = metodo
  )
) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Robustez da resposta Bets → Inadimplência",
    subtitle = "Bootstrap residual, incerteza de Bets e sign restrictions",
    x = "Meses após o choque",
    y = "Mediana do efeito acumulado (p.p.)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(DIR_MC, "Comparacao_tres_metodos.png"),
  g_comp,
  width = 10,
  height = 6,
  dpi = 300
)

H_RESUMO <- c(0, 1, 3, 6, 12, 24)

resumo_final <- comparacao_metodos %>%
  filter(horizonte %in% H_RESUMO) %>%
  arrange(metodo, horizonte)

write.csv(
  resumo_final,
  file.path(DIR_MC, "Resumo_Final_Robustez.csv"),
  row.names = FALSE
)

# =============================================================================
# 5. EXCEL CONSOLIDADO
# =============================================================================

wb <- createWorkbook()

addWorksheet(wb, "IRF_Base_CPP")
writeData(wb, "IRF_Base_CPP", irf_base_df)

addWorksheet(wb, "Bootstrap_IRF")
writeData(wb, "Bootstrap_IRF", boot_summary)

addWorksheet(wb, "Bets_Uncertainty")
writeData(wb, "Bets_Uncertainty", bets_uncert_summary)

addWorksheet(wb, "Sign_Restrictions")
writeData(wb, "Sign_Restrictions", sign_summary)

addWorksheet(wb, "Sign_Definition")
writeData(wb, "Sign_Definition", RESTRICOES)

addWorksheet(wb, "Comparacao")
writeData(wb, "Comparacao", comparacao_metodos)

addWorksheet(wb, "Resumo_Final")
writeData(wb, "Resumo_Final", resumo_final)

saveWorkbook(
  wb,
  file.path(DIR_MC, "Resultados_MonteCarlo_Rcpp_SVAR2.xlsx"),
  overwrite = TRUE
)

cat(
  "\n============================================================\n",
  "MONTE CARLO / Rcpp FINALIZADO\n",
  "============================================================\n",
  "VAR(2) raiz máxima: ", round(fit_cpp$max_root, 4), "\n",
  "Bootstrap aceito: ", boot$accepted, "/", boot$requested, "\n",
  "Sign restrictions aceitas: ", signres$accepted, "\n",
  "Resultados em:\n",
  normalizePath(DIR_MC, winslash = "/", mustWork = FALSE),
  "\n============================================================\n",
  sep = ""
)

print(resumo_final, n = Inf)

