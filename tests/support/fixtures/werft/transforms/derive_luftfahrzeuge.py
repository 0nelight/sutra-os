from sutra_transforms import transform, Input, Output

NICHTGEWERBLICH = {"D-EWRF", "D-EWRT"}


def _g(row, *keys):
    for k in keys:
        v = row.get(k)
        if v not in (None, ""):
            return v
    return ""


@transform(
    inputs={"werte": Input(kind="object-type", object_type="monatswert")},
    output=Output(kind="object-type", object_type="luftfahrzeug", primary_key="kennzeichen"),
)
def derive_luftfahrzeuge(werte):
    by_kz = {}
    for r in werte:
        kz = str(_g(r, "kennzeichen")).strip()
        if not kz:
            continue
        if kz not in by_kz:
            bez = str(_g(r, "bezeichnung")).strip()
            kategorie = str(_g(r, "kategorie")).strip()
            muster = bez.split(" - ", 1)[1] if " - " in bez else ""
            by_kz[kz] = {
                "kennzeichen":    kz,
                "bezeichnung":    bez,
                "kategorie":      kategorie,
                "ist_gewerblich": kategorie in ("Linienflug", "Charter") and kz not in NICHTGEWERBLICH,
                "muster":         muster,
            }
    return list(by_kz.values())
