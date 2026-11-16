from sutra_transforms import transform, Input, Output


@transform(
    inputs={"raw": Input(kind="transform", transform="email-raw")},
    output=Output(kind="object-type", object_type="work-order-pdf", primary_key="pk"),
)
def wo_pdf_from_raw(raw):
    rows, seen = [], set()
    for r in raw:
        email_pk = r.get("pk")
        if not email_pk:
            continue
        for att in (r.get("anhaenge") or []):
            h = att.get("sha256")
            if not h:
                continue
            pk = f"{email_pk}|{h}"
            if pk in seen:
                continue
            seen.add(pk)
            rows.append({
                "pk": pk,
                "email_pk": email_pk,
                "filename": att.get("filename") or "",
                "mime": att.get("mime") or "application/octet-stream",
                "quell_pdf": h,
            })
    return rows
