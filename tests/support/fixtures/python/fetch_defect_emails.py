import email
import email.policy
import imaplib

from sutra_transforms import transform, get_secret, put_media_item

MAILBOX = "INBOX"


def _body_text(m):
    if m.is_multipart():
        for p in m.walk():
            if p.get_content_type() == "text/plain" and not p.get_filename():
                return (p.get_content() or "").strip()
        return ""
    return (m.get_content() or "").strip()


@transform(inputs={}, external_systems=["defect-imap"])
def fetch_defect_emails():
    host = get_secret("defect-imap", "host")
    port = int(get_secret("defect-imap", "port"))
    user = get_secret("defect-imap", "user")
    pwd = get_secret("defect-imap", "password")

    M = imaplib.IMAP4(host, port)
    M.login(user, pwd)
    M.select(MAILBOX)
    _, data = M.uid("search", None, "ALL")

    rows = []
    for uid in (data[0] or b"").split():
        _, fetched = M.uid("fetch", uid, "(RFC822)")
        m = email.message_from_bytes(fetched[0][1], policy=email.policy.default)
        anhaenge = []
        for p in m.walk():
            fn = p.get_filename()
            if not fn:
                continue
            payload = p.get_payload(decode=True) or b""
            mime = p.get_content_type()
            sha256 = put_media_item(payload, mime=mime)
            anhaenge.append({"filename": str(fn), "mime": mime,
                             "sha256": sha256, "size": len(payload)})
        rows.append({
            "pk": "{}:{}".format(MAILBOX, uid.decode()),
            "message_id": str(m["Message-ID"] or "").strip(),
            "subject": str(m["Subject"] or ""),
            "from_addr": str(m["From"] or ""),
            "body_text": _body_text(m),
            "anhaenge": anhaenge,
        })
    M.logout()
    return rows
