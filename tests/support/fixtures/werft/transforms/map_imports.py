from sutra_transforms import transform, Input, Output


@transform(
    inputs={"uploads": Input(kind="dataset", dataset="flugbuch_uploads")},
    output=Output(kind="object-type", object_type="flugbuch-import", primary_key="import-id"),
)
def map_imports(uploads):
    return [{
        "import_id":      u.get("import_id", ""),
        "dateiname":      u.get("dateiname", ""),
        "jahr":           u.get("jahr"),
        "art":            u.get("art", "blockzeiten"),
        "hochgeladen_am": u.get("hochgeladen_am", ""),
        "status":         "verarbeitet",
        "quelle":         u.get("media_ref", ""),
    } for u in uploads]
