import json
import urllib.error
import urllib.request

from sutra_transforms import transform, Input, previous_output, get_secret

MODEL = "grok-4"
API_URL = "https://api.x.ai/v1/chat/completions"
MAX_PER_RUN = 20
PDF_MIME = "application/pdf"

ARTEN = ["position", "unterposition", "summenzeile", "textzeile", "titel"]

POSITION_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "properties": {
        "pfad":        {"type": "string", "description": "Positionsnummer/-pfad wie auf der Rechnung, z. B. '1', '1.2', '1.2.1'. Leer wenn keine."},
        "parent_pfad": {"type": "string", "description": "Pfad der übergeordneten Position (z. B. '1' für '1.2'); leer auf oberster Ebene."},
        "ebene":       {"type": "integer", "description": "Verschachtelungstiefe, 1 = oberste Ebene."},
        "art":         {"type": "string", "enum": ARTEN, "description": "Zeilenart: position/unterposition = Warenzeile; summenzeile/textzeile/titel = Struktur."},
        "beschreibung":{"type": "string"},
        "menge":       {"type": "string", "description": "Menge als Dezimalzahl-String (Punkt als Dezimaltrenner), leer wenn keine."},
        "einheit":     {"type": "string", "description": "Stk|m|m²|m³|kg|h|Pauschal o. ä., leer wenn keine."},
        "einzelpreis": {"type": "string", "description": "Einzelpreis netto als Dezimalzahl-String, leer wenn keiner."},
        "rabatt":      {"type": "string"},
        "zeilen_netto":{"type": "string"},
        "zeilen_brutto":{"type": "string"},
        "lieferanten_artikelnr": {"type": "string"},
        "ean":         {"type": "string"},
    },
    "required": ["pfad", "parent_pfad", "ebene", "art", "beschreibung", "menge",
                 "einheit", "einzelpreis", "rabatt", "zeilen_netto", "zeilen_brutto",
                 "lieferanten_artikelnr", "ean"],
}

PARAMETERS = {
    "type": "object",
    "additionalProperties": False,
    "properties": {
        "is_invoice": {"type": "boolean", "description": "true NUR wenn das Dokument eine RECHNUNG (oder Gutschrift) ist; false bei Lieferschein/Auftragsbestätigung/Versandavis/Sonstigem."},
        "doc_number": {"type": "string", "description": "Rechnungsnummer."},
        "doc_date":   {"type": "string", "description": "Rechnungsdatum wie geschrieben."},
        "faelligkeit":{"type": "string", "description": "Fälligkeitsdatum wie geschrieben, leer wenn keines."},
        "order_ref":  {"type": "string", "description": "Bestell-/Auftragsnummer, auf die sich die Rechnung bezieht; leer wenn keine."},
        "netto":      {"type": "string", "description": "Rechnungsbetrag netto (Kopf) als Dezimalzahl-String."},
        "steuer":     {"type": "string", "description": "Steuerbetrag als Dezimalzahl-String."},
        "brutto":     {"type": "string", "description": "Rechnungsbetrag brutto als Dezimalzahl-String."},
        "steuersatz": {"type": "string", "description": "Steuersatz in Prozent als Dezimalzahl-String, z. B. '20'."},
        "waehrung":   {"type": "string", "description": "ISO-4217-Code, z. B. 'EUR'."},
        "positionen": {"type": "array", "items": POSITION_SCHEMA},
    },
    "required": ["is_invoice", "doc_number", "doc_date", "faelligkeit", "order_ref",
                 "netto", "steuer", "brutto", "steuersatz", "waehrung", "positionen"],
}

TOOL = {"type": "function",
        "function": {"name": "extract_rechnung",
                     "description": "Extrahiere die strukturierten Rechnungsdaten (Kopf + Positionen als Baum) aus dem PDF.",
                     "parameters": PARAMETERS}}

PROMPT = ("Du extrahierst Rechnungsdaten aus dem angehängten PDF einer Werft. "
          "Rufe die Funktion `extract_rechnung` auf. Bestimme zuerst, ob es eine "
          "Rechnung/Gutschrift ist (is_invoice). Bilde die Positionen als Baum ab: "
          "Titel-/Sammelzeilen als art='titel' mit ihren Unterpositionen "
          "(parent_pfad zeigt auf den übergeordneten Pfad). Gib fehlende Werte als "
          "leeren String zurück.")


def _pdf_attachments(row):
    out = []
    for a in (row.get("anhaenge") or []):
        mime = (a.get("mime") or "").lower()
        fn = (a.get("filename") or "").lower()
        if (mime == PDF_MIME or fn.endswith(".pdf")) and a.get("sha256") and a.get("b64"):
            out.append((a["sha256"], a["b64"]))
    return out


def _extract_one(api_key, mime, b64):
    payload = {
        "model": MODEL,
        "max_tokens": 4096,
        "tools": [TOOL],
        "tool_choice": {"type": "function", "function": {"name": "extract_rechnung"}},
        "messages": [{"role": "user", "content": [
            {"type": "text", "text": PROMPT},
            {"type": "image_url", "image_url": {"url": f"data:{mime};base64,{b64}"}},
        ]}],
    }
    req = urllib.request.Request(
        API_URL, data=json.dumps(payload).encode("utf-8"),
        headers={"content-type": "application/json", "authorization": f"Bearer {api_key}"},
        method="POST")
    with urllib.request.urlopen(req, timeout=120) as resp:
        out = json.loads(resp.read().decode("utf-8"))
    return parse_tool_args(out)


def parse_tool_args(api_response):
    for choice in api_response.get("choices", []):
        for tc in (choice.get("message") or {}).get("tool_calls") or []:
            fn = tc.get("function") or {}
            if fn.get("name") == "extract_rechnung":
                try:
                    return json.loads(fn.get("arguments") or "{}")
                except json.JSONDecodeError:
                    return None
    return None


def to_parsed_row(email_pk, sha256, args):
    if not args or not args.get("is_invoice"):
        return None
    s = lambda k: (args.get(k) or "").strip()
    positionen = []
    for p in (args.get("positionen") or []):
        ps = lambda k: (p.get(k) or "").strip()
        positionen.append({
            "pfad": ps("pfad"), "parent_pfad": ps("parent_pfad"),
            "ebene": p.get("ebene") or 0, "art": ps("art") or "position",
            "beschreibung": ps("beschreibung"), "menge": ps("menge"),
            "einheit": ps("einheit"), "einzelpreis": ps("einzelpreis"),
            "rabatt": ps("rabatt"), "zeilen_netto": ps("zeilen_netto"),
            "zeilen_brutto": ps("zeilen_brutto"),
            "lieferanten_artikelnr": ps("lieferanten_artikelnr"), "ean": ps("ean"),
        })
    return {
        "pk": sha256, "email_pk": email_pk, "quell_pdf": sha256,
        "doc_number": s("doc_number"), "doc_date": s("doc_date"),
        "faelligkeit": s("faelligkeit"), "order_ref": s("order_ref"),
        "netto": s("netto"), "steuer": s("steuer"), "brutto": s("brutto"),
        "steuersatz": s("steuersatz"), "waehrung": s("waehrung") or "EUR",
        "positionen": positionen,
    }


@transform(
    inputs={"raw": Input(kind="transform", transform="email-raw")},
    external_systems=["xai"],
)
def parse_invoice(raw):
    done = {r.get("pk") for r in previous_output()}
    pending = []
    for r in raw:
        for sha256, b64 in _pdf_attachments(r):
            if sha256 not in done:
                pending.append((r.get("pk"), sha256, b64))
    pending = pending[:MAX_PER_RUN]
    if not pending:
        return []

    api_key = get_secret("xai", "api-key")
    rows = []
    for email_pk, sha256, b64 in pending:
        try:
            args = _extract_one(api_key, PDF_MIME, b64)
        except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError, OSError):
            continue
        row = to_parsed_row(email_pk, sha256, args)
        if row is not None:
            rows.append(row)
    return rows
