# =============================================================================
# PROJETO V - ITAU/SFN - EXTENSOES DO ARDL USANDO C++
# ARDL original inalterado; serie Bets / IBC / IPCA / Selic / INAD = SVAR(2).
# Execute este arquivo NO RSTUDIO depois de salvar os dois scripts em CODIGOS.
# =============================================================================

rm(list=ls())
options(stringsAsFactors=FALSE, scipen=999)
set.seed(20261008)

# 0. CAMINHOS E PARAMETROS ----------------------------------------------------
DIR_CODIGOS <- "C:/Users/carlo/Downloads/Projetos V - Macro/Códigos"
DIR_ARDL <- "C:/Users/carlo/Downloads/Projetos V - Macro/Output do ARDL - modelo final"
DIR_NOVO <- file.path(DIR_ARDL, "Extensoes_CPP")
dir.create(DIR_NOVO, showWarnings=FALSE, recursive=TRUE)

CPP_FILE <- file.path(DIR_CODIGOS, "ardl_extensoes.cpp")
CSV_BASE <- file.path(DIR_ARDL, "base_transformada_mesma_do_SVAR.csv")
CSV_REG  <- file.path(DIR_ARDL, "base_regressao_ARDL_SVAR.csv")
RDS_EQ   <- file.path(DIR_ARDL, "modelo_equacao_explicita.rds")

B_BOOT      <- 5000L
B_SENS      <- 1000L   # tamanho do bootstrap para sensibilidade dos blocos
B_MONTE     <- 2000L   # por cenario da variavel Bets
BLOCOS      <- c(3L, 4L, 6L)
BLOCO_ALVO  <- 4L
H           <- 24L     # horizonte, meses
HOLDOUT     <- 12L    # mesmo horizonte original para OOS
VALID_INTER <- 10L    # validacao temporal antes dos 12 meses finais
SEMENTE     <- 20261008L

# 1. PACOTES E COMPILACAO ------------------------------------------------------
required <- c("Rcpp", "RcppArmadillo", "sandwich", "ggplot2", "openxlsx")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) install.packages(missing, dependencies=TRUE)

if(!file.exists(CPP_FILE)) {
  stop("Arquivo C++ nao encontrado: ", CPP_FILE,
       "\nSalve ardl_extensoes.cpp na pasta Códigos.")
}
for(p in c(CSV_BASE, CSV_REG)) {
  if(!file.exists(p)) stop("Arquivo base inexistente: ", p,
                            "\nRode primeiro o ARDL original.")
}

cat("\nCompilando nucleo C++...\n")
Rcpp::sourceCpp(CPP_FILE, rebuild=FALSE, showOutput=TRUE)

# 2. CARREGAR E AUDITAR A BASE -------------------------------------------------
raw <- read.csv(CSV_BASE, check.names=FALSE, stringsAsFactors=FALSE)
reg <- read.csv(CSV_REG, check.names=FALSE, stringsAsFactors=FALSE)
vars <- c("data", "bets", "ibc", "ipca", "selic", "inad")
xvars <- c("inad_L1", "inad_L2", "bets_L1", "bets_L2",
           "ibc_L1", "ibc_L2", "ipca_L1", "ipca_L2",
           "selic_L1", "selic_L2")
if(!all(vars %in% names(raw))) stop("Faltam colunas na base transformada: ",
                                     paste(setdiff(vars,names(raw)),collapse=", "))
if(!all(c(vars[1],"inad",xvars) %in% names(reg))) stop("Colunas ausentes no CSV da regressao.")

raw$data <- as.Date(raw$data)
reg$data <- as.Date(reg$data)
raw <- raw[order(raw$data),]
reg <- reg[order(reg$data),]
if(anyNA(raw[,vars]) || anyDuplicated(raw$data) ||
   !identical(raw$data,seq(min(raw$data),max(raw$data),by="month"))) {
  stop("A base transformada tem valores ausentes, meses em falta ou duplicados.")
}
if(!identical(as.character(reg$data),as.character(raw$data[-c(1,2)]))) {
  stop("As datas das bases transformada e de regressao nao estao alinhadas.")
}

lag_v <- function(x,k) c(rep(NA_real_,k),head(x,-k))
esperado <- data.frame(
  inad=raw$inad, inad_L1=lag_v(raw$inad,1), inad_L2=lag_v(raw$inad,2),
  bets_L1=lag_v(raw$bets,1), bets_L2=lag_v(raw$bets,2),
  ibc_L1=lag_v(raw$ibc,1), ibc_L2=lag_v(raw$ibc,2),
  ipca_L1=lag_v(raw$ipca,1), ipca_L2=lag_v(raw$ipca,2),
  selic_L1=lag_v(raw$selic,1), selic_L2=lag_v(raw$selic,2)
)
esperado <- esperado[-c(1,2),,drop=FALSE]
for(nome in c("inad",xvars)) {
  dif <- max(abs(as.numeric(reg[[nome]])-esperado[[nome]]))
  if(!is.finite(dif) || dif>1e-8)
    stop("Falha na auditoria das defasagens: ",nome, " (dif=", dif,")")
}

f_ardl <- as.formula(paste("inad ~",paste(xvars,collapse=" + ")))
m_ols <- lm(f_ardl,data=reg)
b_ols <- coef(m_ols)
if(anyNA(b_ols)) stop("Equacao ARDL com regressor colinear exato.")
L_HAC <- max(1L,min(12L,as.integer(floor(nobs(m_ols)^(1/4)))))
V_HAC_R <- sandwich::NeweyWest(m_ols,lag=L_HAC,prewhite=FALSE,adjust=TRUE)

cpp_ref <- ardl_ols_hac_cpp(raw$inad,raw$bets,raw$ibc,raw$ipca,
                            raw$selic,L_HAC)
if(max(abs(unname(b_ols)-as.numeric(cpp_ref$coef)))>1e-7)
  stop("Os coeficientes C++ nao coincidem com lm() em R.")
if(max(abs(unname(V_HAC_R)-unname(cpp_ref$V_HAC)))>1e-6)
  warning("HAC em C++ difere numericamente do sandwich::NeweyWest: revisar antes de interpretar Wald.")

if(file.exists(RDS_EQ)) {
  m_salvo <- readRDS(RDS_EQ)
  if(inherits(m_salvo,"lm")) {
    cf_salvos <- coef(m_salvo)
    if(!isTRUE(all.equal(unname(cf_salvos[names(b_ols)]),unname(b_ols),tolerance=1e-7)))
      stop("O modelo .rds salvo difere do CSV. Confira se sao da mesma execucao.")
  }
}

cat("\nAUDITORIA OK: mesmas datas, lags e coeficientes do ARDL/SVAR(2).\n")
cat("Amostra:",as.character(min(reg$data)),"a",as.character(max(reg$data)),
    "| n =",nrow(reg),"| lag HAC =",L_HAC,"\n")

# Funcoes de exportacao
csv <- function(d,nome) write.csv(d,file.path(DIR_NOVO,nome),row.names=FALSE,
                                   fileEncoding="UTF-8")
q2 <- function(z,p=c(0.025,0.975)) as.numeric(quantile(z,p,na.rm=TRUE,names=FALSE))
ratio <- function(z) mean(z,na.rm=TRUE)

# 3. BOOTSTRAP RECURSIVO EM BLOCOS ---------------------------------------------
# Reamostra residuos em blocos circulares; preserva trajetoria observada das
# regressoras e simula recursivamente a variavel dependente defasada.
# Amostras sob H0 sao geradas com betas das Bets fixadas em ZERO.
cat("\n[1/4] Bootstrap em blocos...\n")
boot <- bootstrap_ardl_cpp(
  y=raw$inad,bets=raw$bets,ibc=raw$ibc,ipca=raw$ipca,selic=raw$selic,
  B=B_BOOT,block_size=BLOCO_ALVO,H=H,hac_lag=L_HAC
)
coef_b <- as.matrix(boot$coef_boot)
colnames(coef_b) <- names(b_ols)
coef_tab <- data.frame(
  termo=names(b_ols),estimativa_OLS=as.numeric(b_ols),
  media_boot=colMeans(coef_b),
  erro_padrao_boot=apply(coef_b,2,sd),
  ic95_inferior=apply(coef_b,2,function(z) q2(z)[1]),
  ic95_superior=apply(coef_b,2,function(z) q2(z)[2]),
  row.names=NULL
)
csv(coef_tab,"01_bootstrap_coeficientes.csv")

bet_sum <- coef_b[,"bets_L1"]+coef_b[,"bets_L2"]
soma_tab <- data.frame(
  estimativa=sum(b_ols[c("bets_L1","bets_L2")]),
  ic95_inf=q2(bet_sum)[1],ic95_sup=q2(bet_sum)[2],
  prop_positiva=mean(bet_sum>0),prop_negativa=mean(bet_sum<0)
)
csv(soma_tab,"01_bootstrap_soma_bets.csv")

sens <- vector("list",length(BLOCOS))
for(j in seq_along(BLOCOS)) {
  k <- BLOCOS[j]
  bj <- if(k==BLOCO_ALVO) boot else bootstrap_ardl_cpp(
    raw$inad,raw$bets,raw$ibc,raw$ipca,raw$selic,
    B=B_SENS,block_size=k,H=H,hac_lag=L_HAC
  )
  sens[[j]] <- data.frame(
    bloco=k,B=if(k==BLOCO_ALVO) B_BOOT else B_SENS,
    Wald_HAC_original=as.numeric(bj$wald_original),
    p_boot_H0_Bets=as.numeric(bj$p_boot_wald),
    prop_AR_estavel=mean(bj$estavel_boot),
    B_validos_teste=bj$B_validos_nulo
  )
}
sens_tab <- do.call(rbind,sens)
csv(sens_tab,"01_bootstrap_Wald_e_sensibilidade_blocos.csv")

# 4. MULTIPLICADORES DINAMICOS -----------------------------------------------
cat("\n[2/4] Multiplicadores dinamicos...\n")
mult <- multiplicadores_cpp(unname(b_ols),H=H)
resp_b <- as.matrix(boot$resp_boot)
nivel_b <- as.matrix(boot$nivel_boot)
bandas <- data.frame(
  horizonte_meses=0:H,
  resposta_delta_inad=as.numeric(mult$d_inad),
  delta_ic95_inf=apply(resp_b,2,function(z) q2(z)[1]),
  delta_ic95_sup=apply(resp_b,2,function(z) q2(z)[2]),
  resposta_nivel_inad=as.numeric(mult$inad_nivel),
  nivel_ic95_inf=apply(nivel_b,2,function(z) q2(z)[1]),
  nivel_ic95_sup=apply(nivel_b,2,function(z) q2(z)[2])
)
csv(bandas,"02_multiplicadores_dinamicos_24_meses.csv")

library(ggplot2)
g_delta <- ggplot(bandas,aes(horizonte_meses,resposta_delta_inad))+
  geom_hline(yintercept=0,linetype="dashed",color="gray50")+
  geom_ribbon(aes(ymin=delta_ic95_inf,ymax=delta_ic95_sup),alpha=0.20)+
  geom_line(linewidth=0.9,color="#153E75")+
  geom_point(size=1.2,color="#153E75")+
  labs(title="ARDL | resposta da variacao da inadimplencia a Bets",
       subtitle="Inovacao unica de 1 p.p. no crescimento real de Bets | IC bootstrap 95%",
       x="Horizonte (meses)",y="Resposta em Delta Inad (p.p.)")+
  theme_minimal(base_size=12)
ggsave(file.path(DIR_NOVO,"02_resposta_delta_inad.png"),g_delta,
       width=10,height=6,dpi=300)

g_nivel <- ggplot(bandas,aes(horizonte_meses,resposta_nivel_inad))+
  geom_hline(yintercept=0,linetype="dashed",color="gray50")+
  geom_ribbon(aes(ymin=nivel_ic95_inf,ymax=nivel_ic95_sup),alpha=0.20)+
  geom_line(linewidth=0.9,color="#153E75")+
  geom_point(size=1.2,color="#153E75")+
  labs(title="ARDL | resposta acumulada do NIVEL da inadimplencia a Bets",
       subtitle="Soma das respostas de Delta Inad | nao e multiplicador ECM de longo prazo",
       x="Horizonte (meses)",y="Resposta acumulada do nivel (p.p.)")+
  theme_minimal(base_size=12)
ggsave(file.path(DIR_NOVO,"02_resposta_nivel_inad.png"),g_nivel,
       width=10,height=6,dpi=300)

# 5. MONTE CARLO: PERFIS MENSAIS DE BETS --------------------------------------
# SEM a planilha de GGR real em nivel, reconstruimos um indice real proporcional
# pelo dlog mensal. Cada trajetoria alternativa conserva a soma por ano desse
# indice, nao o GGR nominal oficial. Cenarios SAO HIPOTESES, nao erro medido.
cat("\n[3/4] Monte Carlo para Bets (distribuicao mensal incerta)...\n")
anos <- as.integer(format(raw$data,"%Y"))
cenarios <- c(0.05,0.10,0.20)
mc_list <- vector("list",length(cenarios))
mc_reps <- vector("list",length(cenarios))
for(j in seq_along(cenarios)) {
  sigma <- cenarios[j]
  mm <- monte_carlo_bets_cpp(raw$inad,raw$bets,raw$ibc,raw$ipca,
                              raw$selic,anos,B=B_MONTE,sigma=sigma,rho=0.5,H=H)
  s <- as.numeric(mm$soma_coef)
  lvl <- as.numeric(mm$efeito_nivel_H)
  mc_list[[j]] <- data.frame(
    desvio_log_mensal=sigma,B=B_MONTE,
    media_beta1=mean(mm$beta_bets[,1]),media_beta2=mean(mm$beta_bets[,2]),
    media_soma=mean(s),soma_p025=q2(s)[1],soma_p975=q2(s)[2],
    frac_soma_positiva=mean(s>0),
    media_resposta_nivel_H=mean(lvl),nivel_H_p025=q2(lvl)[1],
    nivel_H_p975=q2(lvl)[2],frac_AR_estavel=mean(mm$estavel)
  )
  mc_reps[[j]] <- data.frame(
    cenario_sigma=sigma,replicacao=seq_len(B_MONTE),
    beta_bets_L1=mm$beta_bets[,1],beta_bets_L2=mm$beta_bets[,2],
    soma_bets=s,efeito_nivel_H=lvl,estavel=as.logical(mm$estavel)
  )
  # Checagem observavel da preservacao anual com os tres exemplos exportados.
  exemplo <- data.frame(
    data=raw$data,indice_original=mm$exemplos_nivel[,1],
    indice_sim_1=mm$exemplos_nivel[,2],indice_sim_2=mm$exemplos_nivel[,3],
    indice_sim_3=mm$exemplos_nivel[,4],
    bets_original=raw$bets,bets_sim_1=mm$exemplos_crescimento[,2]
  )
  for(yr in unique(anos)) {
    sel <- which(anos==yr)
    ref <- sum(exemplo$indice_original[sel])
    for(v in c("indice_sim_1","indice_sim_2","indice_sim_3")) {
      if(abs(sum(exemplo[[v]][sel])-ref)>1e-8*max(1,abs(ref)))
        stop("Erro: Monte Carlo nao preservou o total anual do indice.")
    }
  }
  csv(exemplo,sprintf("03_MC_exemplos_sigma_%02d.csv",round(sigma*100)))
}
mc_tab <- do.call(rbind,mc_list)
mc_repeticoes <- do.call(rbind,mc_reps)
csv(mc_tab,"03_MC_resumo_cenarios.csv")
csv(mc_repeticoes,"03_MC_todas_replicacoes.csv")

g_mc <- ggplot(mc_repeticoes,aes(soma_bets,group=factor(cenario_sigma),
                                color=factor(cenario_sigma)))+
  geom_density(linewidth=0.9)+
  geom_vline(xintercept=sum(b_ols[c("bets_L1","bets_L2")]),linetype="dashed")+
  labs(title="Monte Carlo | sensibilidade da soma dos coeficientes de Bets",
       subtitle="Cenarios hipoteticos de incerteza no perfil mensal real",
       x="Soma dos coeficientes de Bets",y="Densidade",color="Desvio log")+
  theme_minimal(base_size=12)
ggsave(file.path(DIR_NOVO,"03_MC_distribuicao_soma_Bets.png"),g_mc,
       width=10,height=6,dpi=300)

# 6. RIDGE E ELASTIC NET -------------------------------------------------------
# Validacao interna ocorre APENAS antes dos ultimos 12 meses: evita usar teste
# OOS para escolher lambda. No teste final, treino expande em cada previsao.
# Regressoras continuam defasadas (informacao disponivel em t-1).
cat("\n[4/4] Ridge / Elastic Net com validacao de origem movel...\n")
X <- as.matrix(reg[,xvars,drop=FALSE]); storage.mode(X) <- "double"
y <- as.numeric(reg$inad)
n <- nrow(X)
if(n < HOLDOUT+VALID_INTER+25L)
  stop("Amostra insuficiente para validacao temporal interna + teste OOS.")
lim_treino <- n-HOLDOUT
idx_val <- seq.int(lim_treino-VALID_INTER+1L,lim_treino)
idx_oos <- seq.int(lim_treino+1L,n)

# A grade vem somente do PRIMEIRO conjunto de treino da validacao interna.
i0 <- idx_val[1]-1L
Z0 <- scale(X[1:i0,,drop=FALSE])
Z0[,!is.finite(colSums(Z0))] <- 0
cy0 <- y[1:i0]-mean(y[1:i0])
lambda_base <- max(abs(colSums(Z0*cy0)/i0))
if(!is.finite(lambda_base) || lambda_base<1e-10) lambda_base <- 0.1
lambdas <- exp(seq(log(lambda_base*4),log(lambda_base*0.0001),length.out=60))

seleciona_lambda <- function(alpha) {
  erros <- matrix(NA_real_,nrow=length(idx_val),ncol=length(lambdas))
  for(h in seq_along(idx_val)) {
    t <- idx_val[h]
    caminho <- elastic_net_path_cpp(X[1:(t-1),,drop=FALSE],
                                     y[1:(t-1)],lambdas,alpha)
    pr <- caminho[,1]+as.numeric(caminho[,-1,drop=FALSE] %*% X[t,])
    erros[h,] <- (y[t]-pr)^2
  }
  mse <- colMeans(erros)
  i <- which.min(mse)
  list(alpha=alpha,lambda=lambdas[i],indice=i,
       validacao_rmse=sqrt(mse[i]),trilha=sqrt(mse))
}

alphas <- c(0,0.5,0.8)
selecoes <- lapply(alphas,seleciona_lambda)
val_tabela <- data.frame(
  modelo=c("Ridge","ElasticNet_05","ElasticNet_08"),
  alpha=alphas,
  lambda_selecionado=vapply(selecoes,function(z) z$lambda,numeric(1)),
  RMSE_validacao=vapply(selecoes,function(z) z$validacao_rmse,numeric(1)),
  n_validacao=length(idx_val)
)
csv(val_tabela,"04_validacao_lambdas_temporal.csv")

prev <- data.frame(data=reg$data[idx_oos],observado=y[idx_oos])
for(j in seq_along(selecoes)) {
  s <- selecoes[[j]]
  out <- numeric(length(idx_oos))
  for(h in seq_along(idx_oos)) {
    t <- idx_oos[h]
    cf <- elastic_net_path_cpp(X[1:(t-1),,drop=FALSE],
                                y[1:(t-1)],s$lambda,s$alpha)
    out[h] <- cf[1,1]+sum(cf[1,-1]*X[t,])
  }
  prev[[val_tabela$modelo[j]]] <- out
}
prev$OLS_ARDL <- vapply(idx_oos,function(t) {
  mm <- lm(f_ardl,data=reg[1:(t-1),,drop=FALSE])
  as.numeric(predict(mm,newdata=reg[t,,drop=FALSE]))
},numeric(1))
prev$AR2 <- vapply(idx_oos,function(t) {
  mm <- lm(inad ~ inad_L1+inad_L2,data=reg[1:(t-1),,drop=FALSE])
  as.numeric(predict(mm,newdata=reg[t,,drop=FALSE]))
},numeric(1))

# Compara com o OOS salvo no fluxo original (nao usa para calibracao).
orig_oos_file <- file.path(DIR_ARDL,"previsoes_OOS_ARDL_SVAR.csv")
if(file.exists(orig_oos_file)) {
  orig_oos <- read.csv(orig_oos_file)
  orig_oos$data <- as.Date(orig_oos$data)
  ref <- merge(prev[,c("data","OLS_ARDL")],orig_oos[,c("data","previsto")],by="data")
  if(nrow(ref)>0 && max(abs(ref$OLS_ARDL-ref$previsto))>1e-6)
    warning("OLS OOS recalculado difere do arquivo salvo: verificar versao da base.")
}

metricas <- do.call(rbind,lapply(c("OLS_ARDL","AR2",val_tabela$modelo),function(nome) {
  e <- prev$observado-prev[[nome]]
  data.frame(modelo=nome,RMSE_OOS=sqrt(mean(e^2)),MAE_OOS=mean(abs(e)),
             n_OOS=length(e))
}))
metricas <- metricas[order(metricas$RMSE_OOS),]
csv(metricas,"04_metricas_OOS_comparacao.csv")
csv(prev,"04_previsoes_OOS_comparacao.csv")

coef_full <- data.frame(termo=c("(Intercept)",xvars),
                        OLS=as.numeric(b_ols))
for(j in seq_along(selecoes)) {
  s <- selecoes[[j]]
  cm <- elastic_net_path_cpp(X,y,s$lambda,s$alpha)
  coef_full[[val_tabela$modelo[j]]] <- as.numeric(cm[1,])
}
csv(coef_full,"04_coeficientes_Ridge_ElasticNet_OLS.csv")

g_oos_data <- data.frame(
  data=rep(prev$data,times=5),
  modelo=rep(c("Observado","OLS_ARDL","Ridge","ElasticNet_05","ElasticNet_08"),
             each=nrow(prev)),
  valor=c(prev$observado,prev$OLS_ARDL,prev$Ridge,
          prev$ElasticNet_05,prev$ElasticNet_08)
)
g_oos <- ggplot(g_oos_data,aes(data,valor,color=modelo))+
  geom_line(linewidth=0.9)+
  labs(title="Previsao OOS (12 meses) | OLS, Ridge e Elastic Net",
       subtitle="Lambda definido antes do holdout; previsao one-step-ahead",
       x=NULL,y="Delta Inad (p.p.)",color=NULL)+
  theme_minimal(base_size=12)
ggsave(file.path(DIR_NOVO,"04_previsoes_OOS_penalizados.png"),g_oos,
       width=11,height=6,dpi=300)

# 7. CONSOLIDAR EM PLANILHA ----------------------------------------------------
wb <- openxlsx::createWorkbook()
addaba <- function(nome,dados) {
  openxlsx::addWorksheet(wb,nome)
  openxlsx::writeData(wb,nome,dados)
  openxlsx::freezePane(wb,nome,firstRow=TRUE)
}
addaba("Parametros",data.frame(
  parametro=c("B bootstrap","Bloco principal","B por sensibilidade","Horizonte meses",
              "B Monte Carlo por cenario","Holdout OOS","Validacao interna","L HAC"),
  valor=c(B_BOOT,BLOCO_ALVO,B_SENS,H,B_MONTE,HOLDOUT,VALID_INTER,L_HAC)
))
addaba("Bootstrap_coef",coef_tab)
addaba("Bootstrap_Wald",sens_tab)
addaba("Bootstrap_soma",soma_tab)
addaba("Multiplicadores",bandas)
addaba("MC_cenarios",mc_tab)
addaba("EN_validacao",val_tabela)
addaba("EN_coeficientes",coef_full)
addaba("OOS_metricas",metricas)
addaba("OOS_previsoes",prev)
openxlsx::saveWorkbook(wb,file.path(DIR_NOVO,
  "Resultados_Extensoes_ARDL_CPP.xlsx"),overwrite=TRUE)

# 8. METADADOS / ALERTAS DE INTERPRETACAO -------------------------------------
log_file <- file.path(DIR_NOVO,"LEIA-ME_metodologia.txt")
writeLines(c(
  "PROJETO V - Extensoes do ARDL/SVAR(2) implementadas em Rcpp/C++",
  paste("Janela:",min(reg$data),"a",max(reg$data),"; N regressao:",nrow(reg)),
  "Modelo original, transformacoes e defasagens preservados (ARDL curto prazo).",
  "BOOTSTRAP: residuos centrados em blocos moveis CIRCULARES, geracao recursiva de Delta Inad, regressoras fixas.",
  "TESTE DE BETS: Wald HAC conjunto das duas defasagens; distrib. bootstrap sob H0 obtida em modelo restrito.",
  "LIMITACAO BOOT: exogeneidade condicional dos X e estabilidade da dinamica sao pressupostos; nao prova causalidade.",
  "BANDAS: percentis bootstrap simples, nao sao intervalos simultaneos; podem ser instaveis com AR perto de 1.",
  "MULTIPLICADORES: choque TRANSITORIO +1 p.p. em crescimento real de Bets, mantendo outros X fixos.",
  "Resposta de NIVEL = soma cumulativa de Delta Inad; nao e ECM, bounds test ou efeito causal identificado.",
  "MONTE CARLO: o nivel real de Bets e INDICE reconstruido de crescimento (escala inicial arbitraria).",
  "Cada ano preserva a soma do indice real ORIGINAL; NAO preserva a soma oficial de GGR nominal.",
  "Sigmas (5%, 10%, 20%) representam cenarios hipoteticos, nao incerteza estimada empiricamente.",
  "RIDGE/ELASTIC NET: penalizacao inclui 10 slopes, nao intercepto; padronizacao so com treino.",
  "Lambdas selecionados com origens moveis antes do ultimo bloco de 12 meses; ultimo bloco usado apenas para teste.",
  "P-valores da regressao OLS nao sao validos automaticamente para coeficientes penalizados.",
  "O conteudo nao identifica efeitos causais: Bets GGR nacional e proxy imperfeita da exposicao ate 10 SM.",
  paste("Fracao de draws AR-estavel no bootstrap principal:",round(mean(boot$estavel_boot),4)),
  paste("Wald HAC observ.:",round(boot$wald_original,4),
        "; p bootstrap sob H0:",round(boot$p_boot_wald,4))
),con=log_file,useBytes=TRUE)

cat("\n====================== RESUMO FINAL ======================\n")
cat("\nWald-HAC bootstrap para Bets:\n");print(sens_tab,row.names=FALSE)
cat("\nMonte Carlo de Bets:\n");print(mc_tab,row.names=FALSE)
cat("\nRegularizacao, validacao previa:\n");print(val_tabela,row.names=FALSE)
cat("\nComparacao OOS:\n");print(metricas,row.names=FALSE)
cat("\nAR bootstrap estavel:",round(mean(boot$estavel_boot)*100,1),"%\n")
cat("\nArquivos exportados para:\n",DIR_NOVO,"\n")
