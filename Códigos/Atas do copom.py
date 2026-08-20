# ============================================================
# EXTRAÇÃO E CLASSIFICAÇÃO DAS ATAS DO COPOM
# ============================================================
#
# Funciona em três modos:
#
#   MODO = "web"
#   MODO = "local"
#   MODO = "ambos"
#
# WEB:
#   usa a API oficial do Banco Central do Brasil
#
# LOCAL:
#   lê PDF, HTML, HTM e TXT de uma pasta
#
# Saída:
#   copom_nlp.xlsx
#
# Abas:
#   1. atas_reuniao
#   2. trimestral
#
# ============================================================


# ============================================================
# 0. BIBLIOTECAS
# ============================================================

import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd
import requests
import fitz  # PyMuPDF

from bs4 import BeautifulSoup
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


# ============================================================
# 1. CONFIGURAÇÕES
# ============================================================

MODO = "web"
# opções:
# "web"
# "local"
# "ambos"


# Período desejado
DATA_INICIAL = "2018-01-01"
DATA_FINAL = "2026-12-31"


# Arquivo final
ARQUIVO_SAIDA = Path(
    r"C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados/copom_nlp.xlsx"
)


# API oficial do BCB
API_BASE = "https://www.bcb.gov.br/api/servico/sitebcb/copom"


# ============================================================
# 2. SESSÃO HTTP
# ============================================================

session = requests.Session()

retry = Retry(
    total=5,
    backoff_factor=1,
    status_forcelist=[429, 500, 502, 503, 504],
    allowed_methods=["GET"]
)

adapter = HTTPAdapter(max_retries=retry)

session.mount("https://", adapter)

session.headers.update({
    "User-Agent": "Mozilla/5.0 - pesquisa academica COPOM"
})


# ============================================================
# 3. FUNÇÕES AUXILIARES
# ============================================================

def limpar_texto(texto):
    """
    Remove espaços excessivos.
    """
    if not texto:
        return ""

    texto = texto.replace("\xa0", " ")
    texto = re.sub(r"\s+", " ", texto)

    return texto.strip()


def sem_acento(texto):
    """
    Converte:
        inflação -> inflacao
        desaceleração -> desaceleracao
    """

    texto = str(texto).lower()

    return "".join(
        c for c in unicodedata.normalize("NFD", texto)
        if unicodedata.category(c) != "Mn"
    )


def normalizar_numero(valor):
    """
    14,25 -> 14.25
    """

    if valor is None:
        return np.nan

    try:
        return float(str(valor).replace(",", "."))
    except Exception:
        return np.nan


# ============================================================
# 4. TRANSFORMAR HTML DO BCB EM PARÁGRAFOS
# ============================================================

def html_para_paragrafos(html):
    """
    Retorna uma lista com:

        secao
        numero_paragrafo
        texto
    """

    soup = BeautifulSoup(html, "html.parser")

    container = soup.select_one("#ataconteudo")

    if container is None:
        container = soup

    resultados = []

    secao_atual = ""

    for elemento in container.find_all(
        ["h2", "h3", "h4", "p"],
        recursive=True
    ):

        texto = limpar_texto(
            " ".join(elemento.stripped_strings)
        )

        if not texto:
            continue

        if elemento.name in ["h2", "h3", "h4"]:
            secao_atual = texto
            continue

        numero = np.nan

        match = re.match(
            r"^\s*(\d{1,3})\.\s*(.*)",
            texto,
            flags=re.DOTALL
        )

        if match:
            numero = int(match.group(1))
            texto = limpar_texto(match.group(2))

        resultados.append({
            "secao": secao_atual,
            "numero_paragrafo": numero,
            "texto": texto
        })

    return resultados


# ============================================================
# 5. PDF -> TEXTO
# ============================================================

def pdf_para_texto(caminho=None, conteudo_bytes=None):

    if conteudo_bytes is not None:

        documento = fitz.open(
            stream=conteudo_bytes,
            filetype="pdf"
        )

    else:

        documento = fitz.open(str(caminho))

    paginas = []

    for pagina in documento:
        paginas.append(
            pagina.get_text("text")
        )

    documento.close()

    return "\n".join(paginas)


# ============================================================
# 6. TEXTO DE PDF -> PARÁGRAFOS
# ============================================================

def texto_para_paragrafos(texto):

    texto = texto.replace("\r", "\n")

    # tenta identificar parágrafos numerados:
    #
    # 1. texto...
    # 2. texto...
    # 3. texto...

    partes = re.split(
        r"(?=\n\s*\d{1,3}\.\s+)",
        texto
    )

    resultados = []

    for parte in partes:

        parte = limpar_texto(parte)

        if not parte:
            continue

        numero = np.nan

        match = re.match(
            r"^(\d{1,3})\.\s+(.*)",
            parte,
            flags=re.DOTALL
        )

        if match:

            numero = int(match.group(1))
            parte = limpar_texto(
                match.group(2)
            )

        resultados.append({
            "secao": "",
            "numero_paragrafo": numero,
            "texto": parte
        })

    return resultados


# ============================================================
# 7. BUSCAR LISTA DE ATAS NA API DO BCB
# ============================================================

def listar_atas_web(quantidade=1000):

    url = f"{API_BASE}/atas"

    resposta = session.get(
        url,
        params={"quantidade": quantidade},
        timeout=60
    )

    resposta.raise_for_status()

    dados = resposta.json()

    return dados.get("conteudo", [])


# ============================================================
# 8. OBTER UMA ATA ESPECÍFICA
# ============================================================

def baixar_ata_web(numero_reuniao):

    url = f"{API_BASE}/atas_detalhes"

    resposta = session.get(
        url,
        params={"nro_reuniao": numero_reuniao},
        timeout=60
    )

    resposta.raise_for_status()

    dados = resposta.json().get(
        "conteudo",
        []
    )

    if not dados:
        return None

    ata = dados[0]

    html = ata.get("textoAta")

    # ----------------------------------------------
    # Caso 1:
    # BCB disponibiliza texto HTML
    # ----------------------------------------------

    if html:

        paragrafos = html_para_paragrafos(html)

        tipo_fonte = "BCB_API_HTML"

    # ----------------------------------------------
    # Caso 2:
    # apenas PDF
    # ----------------------------------------------

    else:

        url_pdf = ata.get("urlPdfAta")

        if not url_pdf:
            return None

        pdf = session.get(
            url_pdf,
            timeout=60
        )

        pdf.raise_for_status()

        texto = pdf_para_texto(
            conteudo_bytes=pdf.content
        )

        paragrafos = texto_para_paragrafos(
            texto
        )

        tipo_fonte = "BCB_PDF"

    return {
        "nro_reuniao": ata.get("nroReuniao"),
        "data_referencia": ata.get("dataReferencia"),
        "data_publicacao": ata.get("dataPublicacao"),
        "titulo": ata.get("titulo"),
        "url_pdf": ata.get("urlPdfAta"),
        "tipo_fonte": tipo_fonte,
        "paragrafos": paragrafos
    }


# ============================================================
# 9. LER ATAS LOCAIS
# ============================================================

def ler_arquivo_local(caminho):

    extensao = caminho.suffix.lower()

    if extensao == ".pdf":

        texto = pdf_para_texto(
            caminho=caminho
        )

        paragrafos = texto_para_paragrafos(
            texto
        )

    elif extensao in [".html", ".htm"]:

        html = caminho.read_text(
            encoding="utf-8",
            errors="ignore"
        )

        paragrafos = html_para_paragrafos(
            html
        )

    elif extensao == ".txt":

        texto = caminho.read_text(
            encoding="utf-8",
            errors="ignore"
        )

        paragrafos = texto_para_paragrafos(
            texto
        )

    else:

        return None

    texto_total = " ".join(
        x["texto"]
        for x in paragrafos
    )

    # tenta descobrir o número da reunião
    match = re.search(
        r"(\d{2,3})\s*[ªa]?\s*reuniao",
        sem_acento(texto_total[:10000])
    )

    numero = (
        int(match.group(1))
        if match
        else np.nan
    )

    return {
        "nro_reuniao": numero,
        "data_referencia": None,
        "data_publicacao": None,
        "titulo": caminho.stem,
        "url_pdf": None,
        "tipo_fonte": "ARQUIVO_LOCAL",
        "arquivo": str(caminho),
        "paragrafos": paragrafos
    }


# ============================================================
# 10. PALAVRAS-CHAVE POR TEMA
# ============================================================

TOPICOS = {

    "atividade": [
        "atividade economica",
        "crescimento",
        "pib",
        "hiato do produto",
        "demanda agregada",
        "producao",
        "setores ciclicos"
    ],

    "trabalho": [
        "mercado de trabalho",
        "desemprego",
        "emprego",
        "ocupacao",
        "postos de trabalho",
        "rendimentos",
        "salarios"
    ],

    "inflacao": [
        "inflacao",
        "ipca",
        "nucleo",
        "precos",
        "desinflacao",
        "expectativas de inflacao"
    ],

    "credito": [
        "credito",
        "concessao",
        "concessoes",
        "condicoes financeiras",
        "financiamento",
        "inadimplencia",
        "endividamento"
    ],

    "incerteza": [
        "incerteza",
        "risco",
        "riscos",
        "volatilidade",
        "cautela",
        "cenario externo",
        "geopolit"
    ],

    "politica_monetaria": [
        "politica monetaria",
        "selic",
        "taxa basica",
        "aperto monetario",
        "flexibilizacao",
        "juros"
    ]
}


# ============================================================
# 11. PADRÕES PARA CLASSIFICAÇÃO
# ============================================================

PADROES = {

    # --------------------------------------------------------
    # atividade econômica
    # score positivo = fortalecimento
    # score negativo = desaceleração
    # --------------------------------------------------------

    "atividade_direcao": {

        "positivo": [
            r"aceleracao",
            r"maior dinamismo",
            r"crescimento mais forte",
            r"crescimento robusto",
            r"surpreendeu positivamente",
            r"expansao da atividade"
        ],

        "negativo": [
            r"desaceleracao",
            r"moderacao",
            r"arrefecimento",
            r"perda de dinamismo",
            r"contracao",
            r"recuo da atividade"
        ]
    },


    # --------------------------------------------------------
    # nível da atividade
    # --------------------------------------------------------

    "atividade_nivel": {

        "positivo": [
            r"atividade.*resiliente",
            r"atividade.*aquecida",
            r"dinamismo",
            r"hiato.*positivo",
            r"acima do potencial"
        ],

        "negativo": [
            r"atividade.*fraca",
            r"ociosidade",
            r"hiato.*negativo",
            r"recessao"
        ]
    },


    # --------------------------------------------------------
    # mercado de trabalho
    # positivo = mercado aquecido
    # --------------------------------------------------------

    "mercado_trabalho": {

        "positivo": [
            r"mercado de trabalho.*aquecido",
            r"mercado de trabalho.*resilien",
            r"mercado de trabalho.*dinam",
            r"aumento.*emprego",
            r"aumento.*postos de trabalho",
            r"queda.*desemprego",
            r"ganhos reais"
        ],

        "negativo": [
            r"mercado de trabalho.*enfraquec",
            r"deterioracao.*mercado de trabalho",
            r"aumento.*desemprego",
            r"queda.*emprego",
            r"reducao.*postos de trabalho"
        ]
    },


    # --------------------------------------------------------
    # inflação
    #
    # positivo = maior pressão
    # negativo = desinflação
    # --------------------------------------------------------

    "inflacao_direcao": {

        "positivo": [
            r"inflacao.*aceler",
            r"reaceleracao",
            r"pressao inflacionaria",
            r"inflacao.*persist",
            r"persistencia inflacionaria",
            r"elevacao.*inflacao"
        ],

        "negativo": [
            r"desinflacao",
            r"inflacao.*desaceler",
            r"arrefecimento.*inflacao",
            r"reducao.*inflacao",
            r"recuo.*inflacao"
        ]
    },


    # --------------------------------------------------------
    # nível da inflação
    #
    # positivo = acima da meta / pressionada
    # --------------------------------------------------------

    "inflacao_nivel": {

        "positivo": [
            r"acima da meta",
            r"acima do limite superior",
            r"expectativas.*desancor",
            r"inflacao.*elevada"
        ],

        "negativo": [
            r"abaixo da meta",
            r"compativel com a meta",
            r"em torno da meta",
            r"expectativas.*ancoradas"
        ]
    },


    # --------------------------------------------------------
    # crédito
    #
    # positivo = condições mais restritivas / estresse
    # --------------------------------------------------------

    "credito_stress": {

        "positivo": [
            r"aperto.*credito",
            r"desaceleracao.*credito",
            r"moderacao.*credito",
            r"moderacao.*concess",
            r"condicoes financeiras.*restrit",
            r"aumento.*inadimplencia",
            r"deterioracao.*credito"
        ],

        "negativo": [
            r"expansao.*credito",
            r"aumento.*concess",
            r"condicoes financeiras.*favoraveis",
            r"reducao.*inadimplencia"
        ]
    },


    # --------------------------------------------------------
    # incerteza
    #
    # positivo = maior risco/incerteza
    # --------------------------------------------------------

    "incerteza": {

        "positivo": [
            r"incerteza.*elevada",
            r"cenario.*incerto",
            r"permanece.*incerto",
            r"risco.*alta",
            r"volatilidade",
            r"tensao geopolitica"
        ],

        "negativo": [
            r"reducao.*incerteza",
            r"menor incerteza",
            r"cenario.*mais benigno",
            r"reducao.*riscos"
        ]
    }

}


# ============================================================
# 12. SELECIONAR PARÁGRAFOS DE UM TEMA
# ============================================================

def paragrafos_topico(paragrafos, topico):

    palavras = TOPICOS[topico]

    selecionados = []

    for p in paragrafos:

        txt = sem_acento(
            p["texto"]
        )

        if any(
            palavra in txt
            for palavra in palavras
        ):
            selecionados.append(p)

    return selecionados


# ============================================================
# 13. CALCULAR SCORE
# ============================================================

def calcular_score(paragrafos, padroes):

    if not paragrafos:
        return np.nan

    texto = " ".join(
        sem_acento(p["texto"])
        for p in paragrafos
    )

    positivos = 0
    negativos = 0

    for padrao in padroes["positivo"]:

        positivos += len(
            re.findall(
                padrao,
                texto,
                flags=re.IGNORECASE
            )
        )

    for padrao in padroes["negativo"]:

        negativos += len(
            re.findall(
                padrao,
                texto,
                flags=re.IGNORECASE
            )
        )

    total = positivos + negativos

    if total == 0:
        return 0.0

    return (
        positivos - negativos
    ) / total


# ============================================================
# 14. EVIDÊNCIAS
# ============================================================

def montar_evidencia(paragrafos, limite=4):

    textos = []

    for p in paragrafos[:limite]:

        numero = p.get(
            "numero_paragrafo"
        )

        texto = p["texto"]

        if pd.notna(numero):
            texto = f"§{int(numero)}: {texto}"

        textos.append(texto)

    return " || ".join(textos)


# ============================================================
# 15. EXTRAIR DECISÃO DA SELIC
# ============================================================

def extrair_decisao_selic(paragrafos):

    decisao = None
    taxa = np.nan
    evidencia = ""

    for p in paragrafos:

        texto = p["texto"]

        txt = sem_acento(texto)

        if "copom decidiu" not in txt and "comite decidiu" not in txt:
            continue

        # decisão

        if re.search(
            r"decidiu.*reduzir",
            txt
        ):
            decisao = "reduzir"

        elif re.search(
            r"decidiu.*elevar",
            txt
        ):
            decisao = "elevar"

        elif re.search(
            r"decidiu.*manter",
            txt
        ):
            decisao = "manter"

        # taxa

        match = re.search(
            r"(\d{1,2}(?:[,.]\d+)?)\s*%\s*a\.?\s*a\.?",
            texto,
            flags=re.IGNORECASE
        )

        if match:

            taxa = normalizar_numero(
                match.group(1)
            )

        evidencia = texto

        break

    return decisao, taxa, evidencia


# ============================================================
# 16. CLASSIFICAR CICLO DE JUROS
# ============================================================

def classificar_ciclo(
    decisao,
    paragrafos
):

    texto = sem_acento(
        " ".join(
            p["texto"]
            for p in paragrafos
        )
    )

    if decisao == "elevar":
        return "aperto / ciclo de alta"

    if decisao == "reduzir":
        return "flexibilização / ciclo de cortes"

    if decisao == "manter":

        if re.search(
            r"periodo prolongado.*juros|"
            r"manutencao.*periodo prolongado|"
            r"grau de aperto",
            texto
        ):

            return "pausa restritiva"

        if re.search(
            r"iniciar.*flexibilizacao|"
            r"iniciar.*reducao|"
            r"cortes.*proxima reuniao",
            texto
        ):

            return "pausa com viés de corte"

        if re.search(
            r"nova elevacao|"
            r"retomar.*aperto|"
            r"elevar.*proxima reuniao",
            texto
        ):

            return "pausa com viés de alta"

        return "manutenção"

    return "indeterminado"


# ============================================================
# 17. TENDÊNCIA DO DESEMPREGO
# ============================================================

def desemprego_tendencia(paragrafos):

    evidencia = paragrafos_topico(
        paragrafos,
        "trabalho"
    )

    texto = sem_acento(
        " ".join(
            p["texto"]
            for p in evidencia
        )
    )

    if re.search(
        r"aumento.*taxa de desemprego|"
        r"taxa de desemprego.*aument|"
        r"elevacao.*desemprego",
        texto
    ):
        return 1

    if re.search(
        r"queda.*taxa de desemprego|"
        r"taxa de desemprego.*recu|"
        r"reducao.*desemprego",
        texto
    ):
        return -1

    if re.search(
        r"desemprego.*estavel|"
        r"taxa de desemprego.*estavel",
        texto
    ):
        return 0

    return np.nan


# ============================================================
# 18. ANALISAR UMA ATA
# ============================================================

def analisar_ata(ata):

    paragrafos = ata["paragrafos"]

    decisao, selic, evidencia_selic = (
        extrair_decisao_selic(
            paragrafos
        )
    )

    ciclo = classificar_ciclo(
        decisao,
        paragrafos
    )

    atividade = paragrafos_topico(
        paragrafos,
        "atividade"
    )

    trabalho = paragrafos_topico(
        paragrafos,
        "trabalho"
    )

    inflacao = paragrafos_topico(
        paragrafos,
        "inflacao"
    )

    credito = paragrafos_topico(
        paragrafos,
        "credito"
    )

    incerteza = paragrafos_topico(
        paragrafos,
        "incerteza"
    )

    politica = paragrafos_topico(
        paragrafos,
        "politica_monetaria"
    )

    resultado = {

        "nro_reuniao":
            ata.get("nro_reuniao"),

        "data_referencia":
            ata.get("data_referencia"),

        "data_publicacao":
            ata.get("data_publicacao"),

        "titulo":
            ata.get("titulo"),

        "tipo_fonte":
            ata.get("tipo_fonte"),

        "url_pdf":
            ata.get("url_pdf"),


        # ---------------------------
        # SELIC
        # ---------------------------

        "selic_meta":
            selic,

        "decisao_selic":
            decisao,

        "ciclo_juros":
            ciclo,


        # ---------------------------
        # ATIVIDADE
        # ---------------------------

        "atividade_direcao":
            calcular_score(
                atividade,
                PADROES["atividade_direcao"]
            ),

        "atividade_nivel":
            calcular_score(
                atividade,
                PADROES["atividade_nivel"]
            ),


        # ---------------------------
        # MERCADO DE TRABALHO
        # ---------------------------

        "mercado_trabalho_score":
            calcular_score(
                trabalho,
                PADROES["mercado_trabalho"]
            ),

        "desemprego_tendencia":
            desemprego_tendencia(
                paragrafos
            ),


        # ---------------------------
        # INFLAÇÃO
        # ---------------------------

        "inflacao_direcao":
            calcular_score(
                inflacao,
                PADROES["inflacao_direcao"]
            ),

        "inflacao_nivel":
            calcular_score(
                inflacao,
                PADROES["inflacao_nivel"]
            ),


        # ---------------------------
        # CRÉDITO
        # ---------------------------

        "credito_stress":
            calcular_score(
                credito,
                PADROES["credito_stress"]
            ),


        # ---------------------------
        # INCERTEZA
        # ---------------------------

        "incerteza_score":
            calcular_score(
                incerteza,
                PADROES["incerteza"]
            ),


        # ---------------------------
        # EVIDÊNCIAS TEXTUAIS
        # ---------------------------

        "evidencia_selic":
            evidencia_selic,

        "evidencia_atividade":
            montar_evidencia(
                atividade
            ),

        "evidencia_trabalho":
            montar_evidencia(
                trabalho
            ),

        "evidencia_inflacao":
            montar_evidencia(
                inflacao
            ),

        "evidencia_credito":
            montar_evidencia(
                credito
            ),

        "evidencia_incerteza":
            montar_evidencia(
                incerteza
            ),

        "evidencia_politica_monetaria":
            montar_evidencia(
                politica
            )
    }

    return resultado


# ============================================================
# 19. COLETAR ATAS DA WEB
# ============================================================

resultados = []


if MODO in ["web", "ambos"]:

    print("Buscando lista de atas no BCB...")

    lista = listar_atas_web(
        quantidade=1000
    )

    for i, item in enumerate(lista, start=1):

        data_ref = pd.to_datetime(
            item.get("dataReferencia"),
            errors="coerce"
        )

        if pd.isna(data_ref):
            continue

        if data_ref < pd.Timestamp(DATA_INICIAL):
            continue

        if data_ref > pd.Timestamp(DATA_FINAL):
            continue

        numero = item["nroReuniao"]

        print(
            f"Baixando reunião {numero}..."
        )

        try:

            ata = baixar_ata_web(
                numero
            )

            if ata:

                resultado = analisar_ata(
                    ata
                )

                resultados.append(
                    resultado
                )

        except Exception as erro:

            print(
                f"Erro na reunião {numero}: {erro}"
            )


# ============================================================
# 20. LER ARQUIVOS LOCAIS
# ============================================================

if MODO in ["local", "ambos"]:

    extensoes = [
        "*.pdf",
        "*.html",
        "*.htm",
        "*.txt"
    ]

    arquivos = []

    for ext in extensoes:

        arquivos.extend(
            PASTA_ATAS.glob(ext)
        )

    for arquivo in arquivos:

        print(
            f"Lendo {arquivo.name}..."
        )

        try:

            ata = ler_arquivo_local(
                arquivo
            )

            if ata:

                resultado = analisar_ata(
                    ata
                )

                resultados.append(
                    resultado
                )

        except Exception as erro:

            print(
                f"Erro em {arquivo.name}: {erro}"
            )


# ============================================================
# 21. DATAFRAME FINAL
# ============================================================

df = pd.DataFrame(
    resultados
)


if df.empty:

    raise RuntimeError(
        "Nenhuma ata foi processada."
    )


df["data_referencia"] = pd.to_datetime(
    df["data_referencia"],
    errors="coerce"
)

df["data_publicacao"] = pd.to_datetime(
    df["data_publicacao"],
    errors="coerce"
)


# ------------------------------------------------------------
# Remover duplicatas
# ------------------------------------------------------------

df = (
    df
    .sort_values(
        ["data_referencia", "nro_reuniao"]
    )
    .drop_duplicates(
        subset=["nro_reuniao"],
        keep="first"
    )
    .reset_index(drop=True)
)


# ============================================================
# 22. CRIAR VARIÁVEL TRIMESTRAL
# ============================================================

df["trimestre"] = (
    df["data_referencia"]
    .dt.to_period("Q")
    .astype(str)
)


variaveis_score = [

    "atividade_direcao",
    "atividade_nivel",
    "mercado_trabalho_score",
    "desemprego_tendencia",
    "inflacao_direcao",
    "inflacao_nivel",
    "credito_stress",
    "incerteza_score"
]


# ============================================================
# 23. AGREGAR POR TRIMESTRE
# ============================================================

df_trimestral = (
    df
    .groupby("trimestre", as_index=False)
    .agg({

        "nro_reuniao": "count",

        "selic_meta": "last",

        "atividade_direcao": "mean",
        "atividade_nivel": "mean",

        "mercado_trabalho_score": "mean",
        "desemprego_tendencia": "mean",

        "inflacao_direcao": "mean",
        "inflacao_nivel": "mean",

        "credito_stress": "mean",

        "incerteza_score": "mean"

    })
)


df_trimestral = df_trimestral.rename(
    columns={
        "nro_reuniao":
            "numero_reunioes"
    }
)


# ============================================================
# 24. EXPORTAR PARA EXCEL
# ============================================================

ARQUIVO_SAIDA.parent.mkdir(
    parents=True,
    exist_ok=True
)


with pd.ExcelWriter(
    ARQUIVO_SAIDA,
    engine="openpyxl"
) as writer:

    df.to_excel(
        writer,
        sheet_name="atas_reuniao",
        index=False
    )

    df_trimestral.to_excel(
        writer,
        sheet_name="trimestral",
        index=False
    )


print("\n====================================")
print("PROCESSAMENTO FINALIZADO")
print("====================================")

print(
    f"\nAtas processadas: {len(df)}"
)

print(
    f"Arquivo salvo em:\n{ARQUIVO_SAIDA}"
)