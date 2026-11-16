from sutra_transforms import transform, Input, Output

EMAIL_COLS = ("pk", "message_id", "subject", "from_addr", "body_text")


@transform(
    inputs={"raw": Input(kind="transform", transform="email-raw")},
    output=Output(kind="object-type", object_type="email", primary_key="pk"),
)
def email_from_raw(raw):
    return [{c: r.get(c, "") for c in EMAIL_COLS} for r in raw if r.get("pk")]
