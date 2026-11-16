import csv
import io

from sutra_transforms import transform, Input, Output

MONATE = ["Januar", "Februar", "März", "April", "Mai", "Juni",
          "Juli", "August", "September", "Oktober", "November", "Dezember"]
MONAT_NUM = {m: i for i, m in enumerate(MONATE, start=1)}

KATEGORIEN = {"Linienflug", "Charter", "Überführung", "Testflug", "Schulung"}


def kategorie_of(einsatz: str) -> str:
    e = (einsatz or "").strip()
    return e if e in KATEGORIEN else "Linienflug"


def parse_de_decimal(raw) -> float:
    s = (str(raw) if raw is not None else "").strip()
    if not s or s in ("-", "--"):
        return 0.0
    s = s.replace(".", "").replace(",", ".")
    try:
        return float(s)
    except ValueError:
        return 0.0


def _read_rows(data: bytes):
    text = data.decode("cp1252", errors="replace")
    return list(csv.DictReader(io.StringIO(text), delimiter=";"))


def _latest_per_year(rows):
    best = {}
    for up in rows:
        ref = up.get("media_ref")
        if not ref:
            continue
        art = (up.get("art") or "blockzeiten").strip().lower()
        if art not in ("", "blockzeiten"):
            continue
        try:
            jahr = int(up.get("jahr") or 0)
        except (TypeError, ValueError):
            jahr = 0
        key = (art, jahr)
        stamp = up.get("hochgeladen_am") or ""
        if key not in best or stamp >= best[key][0]:
            best[key] = (stamp, jahr, ref, up.get("import_id") or "")
    return list(best.values())


@transform(
    inputs={"uploads": Input(kind="media-set", dataset="flugbuch_uploads")},
    output=Output(kind="object-type", object_type="monatswert", primary_key="wert-id"),
)
def parse_flugbuch(uploads):
    out = []
    for _stamp, jahr, ref, import_id in _latest_per_year(uploads.rows()):
        for row in _read_rows(uploads.bytes(ref)):
            kz = (row.get("Kennzeichen") or "").strip().upper()
            if not kz or "-" not in kz:
                continue
            muster = (row.get("Muster") or "").strip()
            bezeichnung = f"{kz} - {muster}" if muster else kz
            kategorie = kategorie_of(row.get("Einsatz"))
            for monat_name, mm in MONAT_NUM.items():
                blockzeit = parse_de_decimal(row.get(monat_name))
                out.append({
                    "wert_id":     f"{kz}-{jahr}-{mm}",
                    "kennzeichen": kz,
                    "import_id":   import_id,
                    "bezeichnung": bezeichnung,
                    "jahr":        jahr,
                    "monat":       mm,
                    "datum":       f"{jahr}-{mm:02d}-01",
                    "blockzeit":   blockzeit,
                    "kategorie":   kategorie,
                    "herkunft":    "flugbuch-aggregat",
                })
    return out
