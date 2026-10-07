# Projetos V — Macroeconomia

Projeto desenvolvido na disciplina **Projetos V — Macroeconomia**, da **FGV EESP**, em parceria com o **Itaú**.

O objetivo é analisar os determinantes da **inadimplência das famílias brasileiras** e avaliar em quais condições a carteira de crédito pode ser expandida de forma sustentável.

A pesquisa também investiga se a expansão das **apostas esportivas online (*bets*)** acrescenta informação à dinâmica da inadimplência.

---

# Pergunta de pesquisa

> **Como a elevada inadimplência das famílias brasileiras condiciona a possibilidade de expansão da carteira de crédito do Itaú?**

A estratégia empírica combina modelos **SVAR** e **ARDL**, utilizando séries macroeconômicas, financeiras e de crédito.

---

# Estratégia econométrica

São utilizadas duas abordagens complementares:

- **SVAR** — análise da transmissão de choques e das respostas dinâmicas da inadimplência;
- **ARDL** — análise das relações dinâmicas de curto e, quando aplicável, longo prazo.

As especificações são estimadas **com e sem a variável de bets**, permitindo avaliar sua contribuição marginal.

---

# 1. SVAR

O SVAR é utilizado para estudar como choques em variáveis macroeconômicas e financeiras se propagam para a inadimplência.

A representação reduzida é:

$$
Y_t = A_1Y_{t-1} + \cdots + A_pY_{t-p} + u_t
$$

e sua representação estrutural:

$$
A_0Y_t = A_1^*Y_{t-1} + \cdots + A_p^*Y_{t-p} + \varepsilon_t
$$

A análise utiliza principalmente:

- Funções de Resposta ao Impulso (IRFs);
- Decomposição da Variância dos Erros de Previsão (FEVD);
- testes de estabilidade e diagnóstico;
- especificações alternativas para avaliação de robustez.

O foco é responder:

> **Como a inadimplência reage aos diferentes choques macroeconômicos e por quanto tempo esses efeitos persistem?**

---

# 2. ARDL

Os modelos ARDL permitem incorporar simultaneamente:

- persistência da inadimplência;
- efeitos contemporâneos e defasados das variáveis explicativas;
- dinâmica de curto prazo;
- relações de longo prazo, quando houver evidência de cointegração.

A especificação geral é:

$$
Y_t =
\alpha +
\sum_{i=1}^{p}\phi_iY_{t-i}
+
\sum_{j=0}^{q}\beta_jX_{t-j}
+
\varepsilon_t
$$

São consideradas três aplicações principais.

---

## Modelo 1 — Inadimplência PF até 10 SM no SFN

### Sem bets

$$
Inad_t^{10SM} = \alpha + \sum_{i=1}^{p}\phi_i Inad_{t-i}^{10SM} + \sum_m\sum_{j=0}^{q_m}\beta_{mj}J_{t-j}^{m} + \sum_k\sum_{r=0}^{s_k}\gamma_{kr}X_{k,t-r} + \varepsilon_t
$$

### Com bets

$$
Inad_t^{10SM}
=
\alpha
+
\sum_{i=1}^{p}\phi_i Inad_{t-i}^{10SM}
+
\sum_m\sum_{j=0}^{q_m}\beta_{mj}J_{t-j}^{m}
+
\sum_k\sum_{r=0}^{s_k}\gamma_{kr}X_{k,t-r}
+
\sum_{\ell=0}^{q_B}\delta_\ell Bets_{t-\ell}
+
\varepsilon_t
$$

em que:

- $Inad_t^{10SM}$: inadimplência PF de famílias com renda de até 10 salários mínimos no SFN;
- $J_t^m$: taxas de juros das modalidades de crédito PF;
- $X_t$: controles macroeconômicos e financeiros;
- $Bets_t$: GGR mensal estimado do mercado de apostas.

Entre as variáveis consideradas estão **Selic, inflação, atividade econômica, mercado de trabalho, renda e condições financeiras das famílias**.

A comparação entre os modelos com e sem bets permite verificar se a variável acrescenta poder explicativo à dinâmica da inadimplência.

---

## Modelo 2 — Inadimplência do Itaú condicionada ao SFN

$$
NPL_t^{Itaú}
=
\alpha
+
\sum_{i=1}^{p}\phi_iNPL_{t-i}^{Itaú}
+
\sum_{j=0}^{q}\theta_jNPL_{t-j}^{SFN}
+
\sum_{\ell=0}^{q_B}\delta_\ell Bets_{t-\ell}
+
\varepsilon_t
$$

O modelo avalia se a inadimplência do Itaú acompanha a dinâmica agregada do SFN e se a variável de bets acrescenta informação ao comportamento da carteira do banco.

Devido ao menor número de observações disponíveis para o Itaú, essa especificação deve permanecer **parsimoniosa**.

---

## Modelo 3 — Gap Itaú × SFN

Define-se:

$$
Gap_t = NPL_t^{Itaú} - NPL_t^{SFN}
$$

e estima-se:

$$
Gap_t
=
\alpha
+
\sum_{i=1}^{p}\rho_i Gap_{t-i}
+
\sum_{\ell=0}^{q_B}\delta_\ell Bets_{t-\ell}
+
\sum_k \gamma_k'X_{t-k}
+
\varepsilon_t
$$

O objetivo é investigar se fatores macroeconômicos, financeiros e relacionados às bets estão associados a uma **deterioração relativa da carteira do Itaú em relação ao SFN**.

Como as séries de inadimplência do Itaú e do SFN não são perfeitamente comparáveis em nível, a interpretação deve privilegiar o comportamento dinâmico do gap, suas variações ou medidas padronizadas.

---

# Curto e longo prazo no ARDL

Quando as propriedades das séries permitirem, os modelos também podem ser representados na forma de **Error Correction Model (ECM)**:

$$
\Delta Y_t =
\alpha
+
\sum_i\gamma_i\Delta Y_{t-i}
+
\sum_j\delta_j\Delta X_{t-j}
+
\lambda ECT_{t-1}
+
\varepsilon_t
$$

O **Bounds Test** é utilizado para verificar a existência de uma relação de equilíbrio de longo prazo.

O coeficiente do termo de correção de erro indica a velocidade de ajuste após desvios do equilíbrio.

---

# Dados

A base combina séries referentes a:

- inadimplência PF no SFN;
- inadimplência do Itaú;
- Selic;
- inflação;
- atividade econômica;
- renda e mercado de trabalho;
- taxas de juros do crédito PF;
- condições financeiras das famílias;
- mercado de apostas esportivas.

As séries passam por tratamento de frequência, transformação e testes de estacionariedade antes da estimação.

A variável de **bets** é utilizada como variável complementar e sua associação com a inadimplência **não é interpretada automaticamente como causal**.

---

# Estrutura do repositório

```text
Projetos-V-Macro/
│
├── Base de dados/
├── Bibliografia/
├── Códigos/
│
├── Output das estatísticas descritivas/
├── Output do ARDL - modelo final/
├── Output do SVAR - itaú/
├── output_SFN_modelos_com_sem_bets/
├── output_SFN_modelos_reestruturados/
│
└── .gitignore
