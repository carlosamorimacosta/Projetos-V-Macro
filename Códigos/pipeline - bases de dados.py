from __future__ import annotations

import re
import unicodedata
import warnings
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

# ============================================================
# 0. CONFIGURAÇÃO
# ============================================================
# Troque apenas esta pasta. O código procura os arquivos pelo nome,
# portanto funciona mesmo se houver sufixos como (1), (2), etc.
BASE_DIR = Path(r"C:/Users/carlo/Downloads/Projetos V - Macro/Base de dados")
OUT_DIR = BASE_DIR / "base_consolidada_output"
OUT_DIR.mkdir(parents=True, exist_ok=True)

# Se quiser filtrar o período, use por exemplo:
# DATA_INICIO = "2018-01-01"
# DATA_FIM    = "2026-12-31"
DATA_INICIO = None
DATA_FIM = None

# A BASE_FINAL será trimestral porque os indicadores do Itaú são trimestrais.
FREQUENCIA_FINAL = "trimestral"

# Não transforme GGR anual em trimestral artificialmente.
# O GGR continuará anual e, na base trimestral, aparecerá apenas no 4º trimestre.
EXPANDIR_GGR_ANUAL = False


# ============================================================
# 1. UTILITÁRIOS GERAIS
# ============================================================
def norm_text(x) -> str:
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return ""
    s = str(x).strip().lower()
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = re.sub(r"\s+", " ", s)
    return s


def slug(s: str) -> str:
    s = norm_text(s)
    s = re.sub(r"[^a-z0-9]+", "_", s).strip("_")
    return s


def to_num_br(x):
    """Converte números brasileiros e marcadores de missing para float."""
    if x is None:
        return np.nan
    if isinstance(x, (int, float, np.integer, np.floating)):
        return float(x) if not pd.isna(x) else np.nan

    s = str(x).strip()
    if norm_text(s) in {"", "-", "--", "n.d.", "n.d", "nd", "nan", "none"}:
        return np.nan

    s = s.replace("R$", "").replace("%", "").replace(" ", "")

    # 1.234,56 -> 1234.56
    if "," in s and "." in s:
        s = s.replace(".", "").replace(",", ".")
    elif "," in s:
        s = s.replace(",", ".")

    return pd.to_numeric(s, errors="coerce")


PT_MONTHS = {
    "jan": 1, "fev": 2, "mar": 3, "abr": 4,
    "mai": 5, "jun": 6, "jul": 7, "ago": 8,
    "set": 9, "out": 10, "nov": 11, "dez": 12,
}


def parse_excel_or_text_date(x):
    """Datas Itaú/Serasa: datetime, serial Excel ou texto com mês em português."""
    if x is None or pd.isna(x):
        return pd.NaT

    if isinstance(x, (pd.Timestamp, np.datetime64)):
        return pd.Timestamp(x)

    # datetime/date do Python
    if hasattr(x, "year") and hasattr(x, "month") and hasattr(x, "day"):
        try:
            return pd.Timestamp(x)
        except Exception:
            pass

    if isinstance(x, (int, float, np.integer, np.floating)):
        # Datas Excel usuais (aprox. 1954 a 2064)
        if 20000 <= float(x) <= 60000:
            return pd.Timestamp("1899-12-30") + pd.to_timedelta(float(x), unit="D")

    s = str(x).strip().lower()
    for mon, num in PT_MONTHS.items():
        s = re.sub(rf"(?<=/){mon}(?=/)", f"{num:02d}", s)
        s = re.sub(rf"(?<=-){mon}(?=-)", f"{num:02d}", s)

    return pd.to_datetime(s, dayfirst=True, errors="coerce")


def parse_bcb_date(x):
    s = str(x).strip()
    if re.fullmatch(r"\d{2}/\d{2}/\d{4}", s):
        return pd.to_datetime(s, format="%d/%m/%Y", errors="coerce")
    if re.fullmatch(r"\d{2}/\d{4}", s):
        d = pd.to_datetime(s, format="%m/%Y", errors="coerce")
        return d + pd.offsets.MonthEnd(0) if not pd.isna(d) else pd.NaT
    return pd.to_datetime(s, dayfirst=True, errors="coerce")


def parse_pnad_period(x):
    """Ex.: 'jan-fev-mar 2012' -> 31/03/2012."""
    s = norm_text(x)
    m = re.search(r"([a-z]{3})\s+(\d{4})$", s)
    if not m:
        return pd.NaT
    mon = PT_MONTHS.get(m.group(1))
    if mon is None:
        return pd.NaT
    d = pd.Timestamp(int(m.group(2)), mon, 1)
    return d + pd.offsets.MonthEnd(0)


def read_csv_flexible(path: Path, skiprows=0) -> pd.DataFrame:
    last_error = None
    for enc in ("utf-8-sig", "utf-8", "latin1"):
        try:
            return pd.read_csv(path, sep=";", encoding=enc, skiprows=skiprows)
        except UnicodeDecodeError as e:
            last_error = e
    raise last_error


def find_file(tokens: Iterable[str], suffix: str | None = None, exclude: Iterable[str] = ()) -> Path:
    """Procura arquivo por tokens, ignorando acentos, caixa e sufixos (1)/(2)."""
    tokens_n = [norm_text(t) for t in tokens]
    exclude_n = [norm_text(t) for t in exclude]

    candidates = []
    for p in BASE_DIR.iterdir():
        if not p.is_file():
            continue
        if suffix and p.suffix.lower() != suffix.lower():
            continue
        n = norm_text(p.name)
        if all(t in n for t in tokens_n) and not any(t in n for t in exclude_n):
            candidates.append(p)

    if not candidates:
        raise FileNotFoundError(
            f"Não encontrei arquivo em {BASE_DIR} com tokens={list(tokens)} e suffix={suffix}"
        )

    # Se houver duplicatas, usa o arquivo modificado mais recentemente.
    candidates.sort(key=lambda p: p.stat().st_mtime, reverse=True)
    if len(candidates) > 1:
        warnings.warn(
            f"Mais de um arquivo corresponde a {list(tokens)}. Usando: {candidates[0].name}"
        )
    return candidates[0]


def resolve_files() -> dict[str, Path]:
    return {
        "itau_old": find_file(["series historicas", "descontinuada"], ".xlsx", exclude=["demonstrativos"]),
        "itau_new": find_file(["series historicas", "demonstrativos", "itau"], ".xlsx"),
        "serasa": find_file(["inadimplencia-do-consumidor", "serasa"], ".xlsx"),
        "bets": find_file(["ggr", "bets"], ".xlsx"),
        "selic": find_file(["selic", "meta"], ".csv"),
        "taxas_credito": find_file(["taxas medias", "operacoes", "credito"], ".csv"),
        "taxa_media_credito": find_file(["taxa media", "juros", "credito"], ".csv", exclude=["taxas medias"]),
        "comprometimento": find_file(["comprometimento", "renda", "juros"], ".csv"),
        "inadimplencia_pf": find_file(["inadimplencia", "pf", "modalidades"], ".csv"),
        "desemprego": find_file(["desemprego", "pnad"], ".csv"),
        "rendimento": find_file(["rendimento", "medio", "pnad"], ".csv"),
    }


def make_long(
    data,
    variable: str,
    values,
    unit: str,
    source: str,
    file_name: str,
    sheet: str | None,
    frequency: str,
    description: str,
    priority: int,
    estimativa=False,
) -> pd.DataFrame:
    out = pd.DataFrame({"data": data, "valor": values})
    out["variavel"] = variable
    out["unidade"] = unit
    out["fonte"] = source
    out["arquivo"] = file_name
    out["aba"] = sheet
    out["frequencia_original"] = frequency
    out["descricao"] = description
    out["prioridade"] = priority

    if np.isscalar(estimativa):
        out["estimativa"] = bool(estimativa)
    else:
        out["estimativa"] = list(estimativa)

    out["data"] = pd.to_datetime(out["data"], errors="coerce")
    out["valor"] = pd.to_numeric(out["valor"], errors="coerce")
    return out.dropna(subset=["data", "valor"])


# ============================================================
# 2. ITAÚ — 21 INDICADORES SOLICITADOS
# ============================================================
ITAU_NPL = [
    ("Itau_NPL90_Total_pct", "NPL 90 dias - Total", "Taxa NPL 90 dias - Total"),
    ("Itau_NPL90_Brasil_pct", "NPL 90 dias - Brasil", "Taxa NPL 90 dias - Brasil"),
    ("Itau_NPL90_PF_Brasil_pct", "NPL 90 dias - Pessoas Físicas - Brasil", "Taxa NPL 90 dias - Pessoas Físicas Brasil"),
    ("Itau_NPL90_MPME_Brasil_pct", "NPL 90 dias - Micro, Pequenas e Médias Empresas - Brasil", "Taxa NPL 90 dias - Micro, Pequenas e Médias Empresas Brasil"),
    ("Itau_NPL90_Grandes_Empresas_Brasil_pct", "NPL 90 dias - Grandes Empresas - Brasil", "Taxa NPL 90 dias - Grandes Empresas Brasil"),
]

ITAU_CARTEIRA = [
    ("Itau_Carteira_PF_Total_R_milhoes", "Pessoas Físicas", "Saldo da carteira PF; NÃO é taxa de inadimplência"),
    ("Itau_Carteira_Cartao_Credito_R_milhoes", "Cartão de Crédito", "Saldo de Cartão de Crédito; NÃO é taxa de inadimplência"),
    ("Itau_Carteira_Credito_Pessoal_R_milhoes", "Crédito Pessoal", "Saldo de Crédito Pessoal; NÃO é taxa de inadimplência"),
    ("Itau_Carteira_Credito_Consignado_R_milhoes", "Crédito Consignado", "Saldo de Crédito Consignado; NÃO é taxa de inadimplência"),
    ("Itau_Carteira_Veiculos_R_milhoes", "Veículos", "Saldo de Veículos; NÃO é taxa de inadimplência"),
    ("Itau_Carteira_Credito_Imobiliario_R_milhoes", "Crédito Imobiliário", "Saldo de Crédito Imobiliário; NÃO é taxa de inadimplência"),
    ("Itau_Carteira_MPME_R_milhoes", "Micro, Pequenas e Médias Empresas (*)", "Saldo da carteira MPME; NÃO é taxa de inadimplência"),
]

ITAU_NPL_CREATION = [
    ("Itau_NPL_Creation_Total_R_milhoes", "NPL Creation - Total", "NPL Creation - Total"),
    ("Itau_NPL_Creation_Varejo_Brasil_R_milhoes", "NPL Creation - Varejo - Brasil", "NPL Creation - Varejo Brasil"),
    ("Itau_NPL_Creation_Atacado_Brasil_R_milhoes", "NPL Creation - Atacado - Brasil", "NPL Creation - Atacado Brasil"),
    ("Itau_NPL_Creation_America_Latina_R_milhoes", "NPL Creation - América Latina", "NPL Creation - América Latina"),
]

ITAU_COBERTURA = [
    ("Itau_Cobertura_Estagio3_PF_Brasil_pct", "Pessoas Físicas - Brasil", "Provisão Estágio 3 / Carteira Estágio 3 - PF Brasil"),
    ("Itau_Cobertura_Estagio3_PJ_Brasil_pct", "Pessoas Jurídicas - Brasil", "Provisão Estágio 3 / Carteira Estágio 3 - PJ Brasil"),
    ("Itau_Cobertura_Estagio3_Brasil_pct", "Brasil", "Provisão Estágio 3 / Carteira Estágio 3 - Brasil"),
    ("Itau_Cobertura_Estagio3_America_Latina_pct", "América Latina", "Provisão Estágio 3 / Carteira Estágio 3 - América Latina"),
    ("Itau_Cobertura_Estagio3_Total_pct", "Total", "Provisão Estágio 3 / Carteira Estágio 3 - Total"),
]

EXPECTED_ITAU = [x[0] for x in ITAU_NPL + ITAU_CARTEIRA + ITAU_COBERTURA + ITAU_NPL_CREATION]


def find_sheet_name(xls: pd.ExcelFile, variants: Iterable[str]) -> str | None:
    normalized = {norm_text(s): s for s in xls.sheet_names}

    for v in variants:
        nv = norm_text(v)
        if nv in normalized:
            return normalized[nv]

    # fallback: contenção nos dois sentidos
    for sheet in xls.sheet_names:
        ns = norm_text(sheet)
        for v in variants:
            nv = norm_text(v)
            if nv in ns or ns in nv:
                return sheet
    return None


def extract_label_row(
    df: pd.DataFrame,
    label: str,
    date_row: int,
    value_start_col: int,
    row_start: int = 0,
    row_end: int | None = None,
    occurrence: int = 0,
):
    row_end = len(df) if row_end is None else min(row_end, len(df))
    labels = df.iloc[row_start:row_end, 0].map(norm_text)
    idxs = labels[labels == norm_text(label)].index.tolist()
    if len(idxs) <= occurrence:
        return None

    idx = idxs[occurrence]
    dates_raw = df.iloc[date_row, value_start_col:]
    vals_raw = df.iloc[idx, value_start_col:]

    dates = dates_raw.map(parse_excel_or_text_date)
    vals = vals_raw.map(to_num_br)
    return dates, vals


def extract_itau(path: Path, source_name: str, priority: int) -> pd.DataFrame:
    xls = pd.ExcelFile(path)
    cache: dict[str, pd.DataFrame] = {}
    out = []

    # -------- NPL 90 dias --------
    sheet_npl = find_sheet_name(xls, ["NPL_com_TVM", "NPL_Nova segmentação"])
    if sheet_npl:
        cache[sheet_npl] = pd.read_excel(path, sheet_name=sheet_npl, header=None)
        df = cache[sheet_npl]
        for var, label, desc in ITAU_NPL:
            result = extract_label_row(
                df, label=label, date_row=1, value_start_col=1,
                row_start=8, row_end=25
            )
            if result is None:
                warnings.warn(f"{path.name}: não achei '{label}' em {sheet_npl}")
                continue
            dates, vals = result
            out.append(make_long(
                dates, var, vals * 100.0, "%", "Itaú RI", path.name, sheet_npl,
                "trimestral", desc, priority
            ))
    else:
        warnings.warn(f"{path.name}: não encontrei aba de NPL")

    # -------- Carteira e NPL Creation --------
    sheet_cart = find_sheet_name(
        xls,
        ["Carteira_Nova segm. e títul", "Carteira_Nova segm. e títulos"]
    )
    if sheet_cart:
        cache[sheet_cart] = pd.read_excel(path, sheet_name=sheet_cart, header=None)
        df = cache[sheet_cart]

        for var, label, desc in ITAU_CARTEIRA:
            result = extract_label_row(
                df, label=label, date_row=1, value_start_col=1,
                row_start=2, row_end=15, occurrence=0
            )
            if result is None:
                warnings.warn(f"{path.name}: não achei '{label}' em {sheet_cart}")
                continue
            dates, vals = result
            out.append(make_long(
                dates, var, vals, "R$ milhões", "Itaú RI", path.name, sheet_cart,
                "trimestral", desc, priority
            ))

        for var, label, desc in ITAU_NPL_CREATION:
            result = extract_label_row(
                df, label=label, date_row=1, value_start_col=1,
                row_start=45, row_end=60, occurrence=0
            )
            if result is None:
                warnings.warn(f"{path.name}: não achei '{label}' em {sheet_cart}")
                continue
            dates, vals = result
            out.append(make_long(
                dates, var, vals, "R$ milhões", "Itaú RI", path.name, sheet_cart,
                "trimestral", desc, priority
            ))
    else:
        warnings.warn(f"{path.name}: não encontrei aba de carteira")

    # -------- Cobertura Estágio 3 --------
    # No arquivo novo existe a aba "Tabela 4966"; no descontinuado ela pode não existir.
    sheet_cov = find_sheet_name(xls, ["Tabela 4966"])
    if sheet_cov:
        df = pd.read_excel(path, sheet_name=sheet_cov, header=None)
        if df.shape[1] >= 3:
            group_col = df.iloc[:, 0].copy()
            group_col = group_col.where(group_col.notna(), np.nan).ffill().map(norm_text)
            cat_col = df.iloc[:, 1].map(norm_text)
            dates = df.iloc[1, 2:].map(parse_excel_or_text_date)

            target_group = norm_text("Cobertura do Estágio 3")
            for var, category, desc in ITAU_COBERTURA:
                mask = group_col.str.contains(target_group, na=False) & (cat_col == norm_text(category))
                idxs = df.index[mask].tolist()
                if not idxs:
                    warnings.warn(f"{path.name}: não achei Cobertura Estágio 3 / {category}")
                    continue
                vals = df.iloc[idxs[0], 2:].map(to_num_br)
                out.append(make_long(
                    dates, var, vals * 100.0, "%", "Itaú RI", path.name, sheet_cov,
                    "trimestral", desc, priority
                ))

    if not out:
        return pd.DataFrame()
    return pd.concat(out, ignore_index=True)


# ============================================================
# 3. SERASA — APENAS A PRIMEIRA ABA
# ============================================================
def extract_serasa(path: Path) -> pd.DataFrame:
    df = pd.read_excel(path, sheet_name=0, header=None)

    # Procura a linha de cabeçalho com "Mês".
    header_candidates = df.index[df.iloc[:, 0].map(norm_text) == "mes"].tolist()
    if not header_candidates:
        raise ValueError(f"Não encontrei a linha 'Mês' na primeira aba de {path.name}")
    h = header_candidates[0]

    data = df.iloc[h + 1:, :14].copy()
    data.columns = [
        "data",
        "Serasa_Consumidores_Inadimplentes_milhoes",
        "Serasa_Dividas_Negativadas_milhoes",
        "Serasa_Dividas_Negativadas_R_bilhoes",
        "Serasa_Dividas_por_CPF",
        "Serasa_Divida_Media_R",
        "Serasa_Ticket_Medio_R",
        "Serasa_Populacao_Adulta_Inadimplente_pct",
        "Serasa_Inadimplentes_Feminino_milhoes",
        "Serasa_Inadimplentes_Masculino_milhoes",
        "Serasa_Inadimplentes_Ate25_milhoes",
        "Serasa_Inadimplentes_26a40_milhoes",
        "Serasa_Inadimplentes_41a60_milhoes",
        "Serasa_Inadimplentes_Acima60_milhoes",
    ]

    data["data"] = data["data"].map(parse_excel_or_text_date)
    data["data"] = pd.to_datetime(data["data"], errors="coerce") + pd.offsets.MonthEnd(0)

    for c in data.columns[1:]:
        data[c] = data[c].map(to_num_br)

    # Na planilha o percentual está armazenado como fração (ex.: 0,50 = 50%).
    data["Serasa_Populacao_Adulta_Inadimplente_pct"] *= 100.0

    # Remove datas sem qualquer observação.
    data = data.dropna(subset=["data"])
    data = data.loc[data.drop(columns="data").notna().any(axis=1)].copy()

    meta = {
        "Serasa_Consumidores_Inadimplentes_milhoes": ("milhões", "Consumidores inadimplentes"),
        "Serasa_Dividas_Negativadas_milhoes": ("milhões", "Quantidade de dívidas negativadas"),
        "Serasa_Dividas_Negativadas_R_bilhoes": ("R$ bilhões", "Valor das dívidas negativadas"),
        "Serasa_Dividas_por_CPF": ("dívidas/CPF", "Número médio de dívidas por CPF inadimplente"),
        "Serasa_Divida_Media_R": ("R$", "Dívida média por CPF"),
        "Serasa_Ticket_Medio_R": ("R$", "Ticket médio das dívidas negativadas"),
        "Serasa_Populacao_Adulta_Inadimplente_pct": ("%", "% da população adulta inadimplente"),
        "Serasa_Inadimplentes_Feminino_milhoes": ("milhões", "Consumidoras inadimplentes"),
        "Serasa_Inadimplentes_Masculino_milhoes": ("milhões", "Consumidores inadimplentes"),
        "Serasa_Inadimplentes_Ate25_milhoes": ("milhões", "Inadimplentes até 25 anos"),
        "Serasa_Inadimplentes_26a40_milhoes": ("milhões", "Inadimplentes de 26 a 40 anos"),
        "Serasa_Inadimplentes_41a60_milhoes": ("milhões", "Inadimplentes de 41 a 60 anos"),
        "Serasa_Inadimplentes_Acima60_milhoes": ("milhões", "Inadimplentes acima de 60 anos"),
    }

    out = []
    sheet_name = pd.ExcelFile(path).sheet_names[0]
    for c, (unit, desc) in meta.items():
        out.append(make_long(
            data["data"], c, data[c], unit, "Serasa Experian", path.name, sheet_name,
            "mensal", desc, 70
        ))
    return pd.concat(out, ignore_index=True)


# ============================================================
# 4. GGR BETS — TODAS AS ABAS
# ============================================================
def extract_ggr(path: Path) -> pd.DataFrame:
    sheets = pd.read_excel(path, sheet_name=None, header=None)
    out = []

    known = {
        norm_text("GGR Operações Offshore"): ("Bets_GGR_Offshore_R_bilhoes", "GGR de operações offshore"),
        norm_text("GGR Operações Nacionais"): ("Bets_GGR_Nacional_R_bilhoes", "GGR de operações nacionais"),
        norm_text("Total"): ("Bets_GGR_Total_R_bilhoes", "GGR total das bets"),
    }

    for sheet_name, df in sheets.items():
        # Localiza linha com anos.
        year_row = None
        year_cols = []
        for i in range(min(len(df), 20)):
            cols = []
            for j, v in enumerate(df.iloc[i].tolist()):
                try:
                    y = int(float(v))
                    if 2000 <= y <= 2100:
                        cols.append((j, y))
                except Exception:
                    pass
            if len(cols) >= 2:
                year_row = i
                year_cols = cols
                break
        if year_row is None:
            continue

        # Detecta anos de projeção a partir de eventual nota na própria aba.
        estimate_start = None
        for v in df.astype(str).stack().tolist():
            m = re.search(r"dados de\s+(\d{4})\s+a\s+(\d{4}).*estim", norm_text(v))
            if m:
                estimate_start = int(m.group(1))
                break

        for i in range(year_row + 1, len(df)):
            label = df.iat[i, 0] if df.shape[1] > 0 else None
            nl = norm_text(label)
            if not nl:
                continue

            vals = [to_num_br(df.iat[i, j]) if j < df.shape[1] else np.nan for j, _ in year_cols]
            if not any(pd.notna(v) for v in vals):
                continue

            if nl in known:
                var, desc = known[nl]
            else:
                var = f"Bets_{slug(str(label))}"
                desc = str(label)

            years = [y for _, y in year_cols]
            dates = [pd.Timestamp(y, 12, 31) for y in years]
            estim = [bool(estimate_start and y >= estimate_start) for y in years]
            out.append(make_long(
                dates, var, vals, "R$ bilhões", "GGR Bets", path.name, sheet_name,
                "anual", desc, 60, estimativa=estim
            ))

    return pd.concat(out, ignore_index=True) if out else pd.DataFrame()


# ============================================================
# 5. BCB / SGS — TODOS OS CSVs
# ============================================================
SGS_NAME_MAP = {
    "432": "Selic_Meta_pct_aa",
    "20740": "Taxa_Juros_Livre_PF_Total_pct_aa",
    "20742": "Taxa_Juros_Credito_Pessoal_Nao_Consignado_pct_aa",
    "20748": "Taxa_Juros_Credito_Pessoal_Total_pct_aa",
    "22024": "Taxa_Juros_Cartao_Credito_Total_pct_aa",
    "29034": "Comprometimento_Renda_Servico_Divida_Ajustado_pct",
    "29035": "Comprometimento_Renda_Servico_Divida_Sem_Habitacional_Ajustado_pct",
    "29264": "Comprometimento_Renda_Juros_Divida_Sem_Ajuste_pct",
    "21084": "Inadimplencia_PF_Total_pct",
    "21112": "Inadimplencia_PF_Livre_Total_pct",
    "21113": "Inadimplencia_PF_Cheque_Especial_pct",
    "21114": "Inadimplencia_PF_Credito_Pessoal_Nao_Consignado_Total_pct",
    "21115": "Inadimplencia_PF_Nao_Consignado_Composicao_Dividas_pct",
    "21116": "Inadimplencia_PF_Consignado_Setor_Privado_pct",
    "29991": "Inadimplencia_PF_Nao_Consignado_Com_Garantias_Reais_pct",
    "29992": "Inadimplencia_PF_Nao_Consignado_Sem_Garantias_Reais_pct",
}


def extract_bcb_csv(path: Path, priority: int = 60) -> pd.DataFrame:
    df = read_csv_flexible(path)
    if df.empty or df.shape[1] < 2:
        return pd.DataFrame()

    date_col = df.columns[0]
    dates = df[date_col].map(parse_bcb_date)

    sample = df[date_col].dropna().astype(str).head(30)
    daily = sample.str.fullmatch(r"\d{2}/\d{2}/\d{4}").mean() > 0.5
    freq = "diaria" if daily else "mensal"

    out = []
    for col in df.columns[1:]:
        m = re.match(r"\s*(\d+)\s*-\s*(.*)", str(col))
        if m:
            code = m.group(1)
            desc = m.group(2).strip()
            var = SGS_NAME_MAP.get(code, f"BCB_SGS_{code}")
        else:
            code = ""
            desc = str(col).strip()
            var = f"BCB_{slug(desc)}"

        nc = norm_text(col)
        if "% a.a" in nc or "% a.a." in nc:
            unit = "% a.a."
        elif "%" in str(col):
            unit = "%"
        else:
            unit = "conforme SGS"

        vals = df[col].map(to_num_br)
        out.append(make_long(
            dates, var, vals, unit, "BCB/SGS", path.name, None,
            freq, desc, priority
        ))

    return pd.concat(out, ignore_index=True) if out else pd.DataFrame()


# ============================================================
# 6. IBGE / PNAD CONTÍNUA — ARQUIVOS WIDE
# ============================================================
def extract_pnad(path: Path, variable: str, unit: str, description: str) -> pd.DataFrame:
    # Linha 1 é apenas o título; linha 2 contém os períodos.
    df = read_csv_flexible(path, skiprows=1)
    if df.empty:
        return pd.DataFrame()

    first_col = df.columns[0]
    row = df.loc[df[first_col].map(norm_text) == "brasil"]
    if row.empty:
        raise ValueError(f"Não encontrei a linha Brasil em {path.name}")
    row = row.iloc[0]

    dates, values = [], []
    for col in df.columns[1:]:
        d = parse_pnad_period(col)
        if pd.isna(d):
            continue
        dates.append(d)
        values.append(to_num_br(row[col]))

    return make_long(
        dates, variable, values, unit, "IBGE/PNAD Contínua", path.name, None,
        "mensal - trimestre móvel", description, 70
    )


# ============================================================
# 7. CONTROLE DE DUPLICATAS E AGREGAÇÕES
# ============================================================
def resolve_duplicates(base: pd.DataFrame):
    keys = ["data", "variavel"]
    dup_mask = base.duplicated(keys, keep=False)
    duplicates = base.loc[dup_mask].sort_values(keys + ["prioridade"]).copy()

    # Prioridade maior vence. Para Itaú: arquivo novo > descontinuado.
    clean = (
        base.sort_values(keys + ["prioridade"])
            .drop_duplicates(keys, keep="last")
            .sort_values(["data", "variavel"])
            .reset_index(drop=True)
    )
    return clean, duplicates


def aggregation_method(variable: str, source: str) -> str:
    # Taxas BCB são médias dentro do período.
    if source == "BCB/SGS":
        return "mean"
    # Itaú, Serasa e PNAD são observações de fechamento/snapshot.
    return "last"


def aggregate_period(base: pd.DataFrame, period: str) -> pd.DataFrame:
    parts = []
    for var, g in base.groupby("variavel", sort=False):
        g = g.sort_values("data").copy()
        source = g["fonte"].iloc[-1]
        method = aggregation_method(var, source)
        g["periodo"] = g["data"].dt.to_period(period)

        if method == "mean":
            z = g.groupby("periodo", as_index=False)["valor"].mean()
        else:
            z = g.groupby("periodo", as_index=False).tail(1)[["periodo", "valor"]]

        z["variavel"] = var
        z["data"] = z["periodo"].dt.to_timestamp(how="end").dt.normalize()
        parts.append(z[["data", "variavel", "valor"]])

    if not parts:
        return pd.DataFrame(columns=["data"])

    long_agg = pd.concat(parts, ignore_index=True)
    wide = (
        long_agg.pivot_table(index="data", columns="variavel", values="valor", aggfunc="last")
                .sort_index()
                .reset_index()
    )
    wide.columns.name = None
    return wide


def filter_dates(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if DATA_INICIO:
        out = out[out["data"] >= pd.Timestamp(DATA_INICIO)]
    if DATA_FIM:
        out = out[out["data"] <= pd.Timestamp(DATA_FIM)]
    return out


# ============================================================
# 8. DICIONÁRIO E EXPORTAÇÃO
# ============================================================
def build_dictionary(base: pd.DataFrame) -> pd.DataFrame:
    cols = ["variavel", "descricao", "unidade", "fonte", "frequencia_original", "arquivo", "aba", "prioridade"]
    d = (
        base.sort_values(["variavel", "prioridade"])
            .drop_duplicates("variavel", keep="last")[cols]
            .sort_values("variavel")
            .reset_index(drop=True)
    )
    d["agregacao_para_mensal_trimestral_anual"] = [
        aggregation_method(v, f) for v, f in zip(d["variavel"], d["fonte"])
    ]
    return d


def format_excel(writer, sheet_name: str, df: pd.DataFrame):
    ws = writer.sheets[sheet_name]
    wb = writer.book
    fmt_date = wb.add_format({"num_format": "dd/mm/yyyy"})
    fmt_header = wb.add_format({"bold": True, "bg_color": "#D9EAF7", "border": 1})

    ws.freeze_panes(1, 1)
    if len(df.columns):
        ws.autofilter(0, 0, max(len(df), 1), len(df.columns) - 1)
        ws.set_row(0, None, fmt_header)

    for j, col in enumerate(df.columns):
        width = min(max(len(str(col)) + 2, 12), 42)
        if col == "data":
            ws.set_column(j, j, 13, fmt_date)
        elif col in {"descricao", "arquivo", "aba"}:
            ws.set_column(j, j, 38)
        else:
            ws.set_column(j, j, width)


def export_outputs(
    base_long: pd.DataFrame,
    base_mensal: pd.DataFrame,
    base_trimestral: pd.DataFrame,
    base_anual: pd.DataFrame,
    dictionary: pd.DataFrame,
    duplicates: pd.DataFrame,
):
    # Parquet
    base_long.to_parquet(OUT_DIR / "base_long.parquet", index=False)
    base_mensal.to_parquet(OUT_DIR / "base_mensal.parquet", index=False)
    base_trimestral.to_parquet(OUT_DIR / "BASE_FINAL.parquet", index=False)
    base_anual.to_parquet(OUT_DIR / "base_anual.parquet", index=False)

    # Excel organizado
    xlsx_path = OUT_DIR / "BASE_CONSOLIDADA.xlsx"
    with pd.ExcelWriter(xlsx_path, engine="xlsxwriter", datetime_format="dd/mm/yyyy") as writer:
        base_trimestral.to_excel(writer, sheet_name="Base_Trimestral", index=False)
        base_mensal.to_excel(writer, sheet_name="Base_Mensal", index=False)
        base_anual.to_excel(writer, sheet_name="Base_Anual", index=False)
        base_long.to_excel(writer, sheet_name="Base_Long", index=False)
        dictionary.to_excel(writer, sheet_name="Dicionario", index=False)
        duplicates.to_excel(writer, sheet_name="Duplicatas_Originais", index=False)

        for sheet_name, df in {
            "Base_Trimestral": base_trimestral,
            "Base_Mensal": base_mensal,
            "Base_Anual": base_anual,
            "Base_Long": base_long,
            "Dicionario": dictionary,
            "Duplicatas_Originais": duplicates,
        }.items():
            format_excel(writer, sheet_name, df)

    return xlsx_path


# ============================================================
# 9. PIPELINE PRINCIPAL
# ============================================================
def main():
    files = resolve_files()

    print("\nArquivos identificados:")
    for k, p in files.items():
        print(f"  {k:20s} -> {p.name}")

    bases = []

    # Itaú: prioridade do arquivo novo > descontinuado.
    bases.append(extract_itau(files["itau_old"], "Itaú descontinuado", priority=90))
    bases.append(extract_itau(files["itau_new"], "Itaú demonstrativos", priority=100))

    # Serasa: SOMENTE primeira aba.
    bases.append(extract_serasa(files["serasa"]))

    # Bets: TODAS as abas.
    bases.append(extract_ggr(files["bets"]))

    # BCB/SGS: todos os dados dos CSVs.
    bases.append(extract_bcb_csv(files["selic"], priority=70))
    bases.append(extract_bcb_csv(files["taxas_credito"], priority=70))
    # 20740 aparece também no arquivo acima; prioridade menor aqui para evitar conflito.
    bases.append(extract_bcb_csv(files["taxa_media_credito"], priority=65))
    bases.append(extract_bcb_csv(files["comprometimento"], priority=70))
    bases.append(extract_bcb_csv(files["inadimplencia_pf"], priority=70))

    # IBGE/PNAD.
    bases.append(extract_pnad(
        files["desemprego"],
        "Desemprego_PNAD_pct", "%",
        "Taxa de desocupação - PNAD Contínua, trimestre móvel"
    ))
    bases.append(extract_pnad(
        files["rendimento"],
        "Rendimento_Medio_PNAD_R", "R$",
        "Rendimento médio - PNAD Contínua, trimestre móvel"
    ))

    bases = [b for b in bases if b is not None and not b.empty]
    base_raw = pd.concat(bases, ignore_index=True)

    # Padronização e filtro.
    base_raw["data"] = pd.to_datetime(base_raw["data"], errors="coerce")
    base_raw = base_raw.dropna(subset=["data", "variavel", "valor"])
    base_raw = filter_dates(base_raw)

    # Resolve sobreposições: arquivo novo do Itaú prevalece.
    base_long, duplicates = resolve_duplicates(base_raw)

    # Colunas temporais úteis.
    base_long["ano"] = base_long["data"].dt.year
    base_long["mes"] = base_long["data"].dt.month
    base_long["trimestre"] = base_long["data"].dt.to_period("Q").astype(str)

    # Bases wide em diferentes frequências.
    base_mensal = aggregate_period(base_long, "M")
    base_trimestral = aggregate_period(base_long, "Q")
    base_anual = aggregate_period(base_long, "Y")

    if not base_mensal.empty:
        base_mensal.insert(1, "Ano", base_mensal["data"].dt.year)
        base_mensal.insert(2, "Mes", base_mensal["data"].dt.month)

    if not base_trimestral.empty:
        base_trimestral.insert(1, "Ano", base_trimestral["data"].dt.year)
        base_trimestral.insert(2, "Trimestre", base_trimestral["data"].dt.quarter)

    if not base_anual.empty:
        base_anual.insert(1, "Ano", base_anual["data"].dt.year)

    dictionary = build_dictionary(base_long)

    # Validação dos 21 indicadores do Itaú.
    found_itau = set(base_long.loc[base_long["variavel"].str.startswith("Itau_"), "variavel"])
    missing_itau = [v for v in EXPECTED_ITAU if v not in found_itau]

    print("\nValidação Itaú:")
    print(f"  Indicadores esperados: {len(EXPECTED_ITAU)}")
    print(f"  Indicadores encontrados: {len(set(EXPECTED_ITAU) & found_itau)}")
    if missing_itau:
        print("  NÃO encontrados:")
        for v in missing_itau:
            print("   -", v)
    else:
        print("  OK: todos os 21 indicadores foram encontrados.")

    print(f"\nObservações duplicadas antes da prioridade: {len(duplicates)}")
    print(f"Observações finais em long: {len(base_long)}")
    print(f"Variáveis finais: {base_long['variavel'].nunique()}")
    print(f"Período: {base_long['data'].min().date()} a {base_long['data'].max().date()}")

    xlsx = export_outputs(
        base_long, base_mensal, base_trimestral, base_anual,
        dictionary, duplicates
    )

    print("\nArquivos gerados:")
    print(" ", xlsx)
    print(" ", OUT_DIR / "BASE_FINAL.parquet")
    print(" ", OUT_DIR / "base_long.parquet")
    print(" ", OUT_DIR / "base_mensal.parquet")
    print(" ", OUT_DIR / "base_anual.parquet")


if __name__ == "__main__":
    main()