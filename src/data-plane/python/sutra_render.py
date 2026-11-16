
import base64
import datetime
import io
import json
import signal
import sys
import traceback

MAX_SOURCE_CHARS = 1_000_000
MAX_OUTPUT_CHARS = 32_000_000
RENDER_SECONDS = 60

PAGE_SIZES = {
    "a1": "A1", "a2": "A2", "a3": "A3", "a4": "A4", "a5": "A5",
    "letter": "letter", "legal": "legal", "tabloid": "11in 17in",
}

BASE_CSS = """
html { font-family: "DejaVu Sans", "Liberation Sans", sans-serif; font-size: 10pt; }
body { margin: 0; color: #1c1c1c; }
h1 { font-size: 18pt; margin: 0 0 .6em; }
h2 { font-size: 14pt; margin: 1.2em 0 .4em; }
h3 { font-size: 11.5pt; margin: 1em 0 .3em; }
p { margin: 0 0 .5em; line-height: 1.45; }
table { border-collapse: collapse; width: 100%; margin: .6em 0; }
th, td { border: 1px solid #c8c8c8; padding: 3pt 5pt; text-align: left;
         vertical-align: top; }
th { background: #f0f0f0; font-weight: 600; }
td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
thead { display: table-header-group; }
tr { break-inside: avoid; }
.page-break { break-after: page; }
hr { border: 0; border-top: 1px solid #c8c8c8; margin: 1em 0; }
"""


def _fmt_number(value, decimals=2):
    if value is None or value == "":
        return ""
    try:
        n = float(value)
    except (TypeError, ValueError):
        return str(value)
    s = "{:,.{d}f}".format(n, d=decimals)
    return s.replace(",", " ").replace(".", ",")


def _as_date(value):
    if isinstance(value, (datetime.datetime, datetime.date)):
        return value
    if value is None or value == "":
        return None
    text = str(value).strip().replace("Z", "+00:00")
    try:
        return datetime.datetime.fromisoformat(text)
    except ValueError:
        for pattern in ("%Y-%m-%d", "%d.%m.%Y", "%Y-%m-%dT%H:%M:%S"):
            try:
                return datetime.datetime.strptime(text, pattern)
            except ValueError:
                continue
    return None


MONATE = ("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
          "August", "September", "Oktober", "November", "Dezember")
TAGE = ("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag",
        "Samstag", "Sonntag")


def f_datum(value, pattern="%d.%m.%Y"):
    d = _as_date(value)
    if not d:
        return ""
    if "%B" in pattern or "%A" in pattern:
        pattern = (pattern.replace("%B", MONATE[d.month - 1])
                          .replace("%A", TAGE[d.weekday()]))
    return d.strftime(pattern)


def f_zahl(value, decimals=2):
    return _fmt_number(value, decimals)


def f_waehrung(value, symbol="€", decimals=2):
    s = _fmt_number(value, decimals)
    return "{} {}".format(s, symbol) if s else ""


def f_prozent(value, decimals=1):
    s = _fmt_number(value, decimals)
    return "{} %".format(s) if s else ""


def f_summe(rows, key):
    total = 0.0
    for row in rows or []:
        try:
            total += float(row.get(key) or 0)
        except (TypeError, ValueError, AttributeError):
            pass
    return total


def _environment(fmt):
    from jinja2 import StrictUndefined
    from jinja2.sandbox import ImmutableSandboxedEnvironment

    env = ImmutableSandboxedEnvironment(
        autoescape=(fmt in ("html", "markdown")),
        undefined=StrictUndefined,
        trim_blocks=True,
        lstrip_blocks=True,
        keep_trailing_newline=True,
    )
    env.filters.update({
        "datum": f_datum, "zahl": f_zahl, "waehrung": f_waehrung,
        "prozent": f_prozent, "summe": f_summe,
    })
    return env


def _margin_box(position, content):
    if not content:
        return ""
    return "@%s { content: %s; font-size: 8pt; color: #555; }" % (
        position, json.dumps(content),
    )


def page_css(page):
    page = page or {}
    size = PAGE_SIZES.get(str(page.get("size") or "a4").lower(), "A4")
    orientation = str(page.get("orientation") or "portrait").lower()
    if orientation == "landscape":
        size += " landscape"
    margin = page.get("margin") or "18mm 16mm"
    boxes = "".join([
        _margin_box("top-center", page.get("header")),
        _margin_box("bottom-center", page.get("footer")),
    ])
    base = "" if page.get("base-css") is False else BASE_CSS
    return "@page { size: %s; margin: %s; %s }\n%s\n%s" % (
        size, margin, boxes, base, page.get("css") or "",
    )


class RenderLimit(Exception):
    pass


def _on_alarm(signum, frame):
    raise RenderLimit("the template ran longer than %d s" % RENDER_SECONDS)


def render_template(source, fmt, context):
    if len(source) > MAX_SOURCE_CHARS:
        raise RenderLimit("the template is longer than %d characters" % MAX_SOURCE_CHARS)
    template = _environment(fmt).from_string(source)
    previous = signal.signal(signal.SIGALRM, _on_alarm)
    signal.alarm(RENDER_SECONDS)
    try:
        parts, size = [], 0
        for chunk in template.generate(**context):
            size += len(chunk)
            if size > MAX_OUTPUT_CHARS:
                raise RenderLimit("the output exceeds %d characters" % MAX_OUTPUT_CHARS)
            parts.append(chunk)
        return "".join(parts)
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, previous)


def render_body(source, fmt, context, literal=False):
    text = source if literal else render_template(source, fmt, context)
    if fmt == "markdown":
        import markdown
        return markdown.markdown(text, extensions=["tables", "fenced_code"])
    if fmt == "text":
        return "<pre>%s</pre>" % (
            text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        )
    return text


def to_pdf(html, page):
    from weasyprint import CSS, HTML

    buffer = io.BytesIO()
    HTML(string=html, base_url=None).write_pdf(
        buffer, stylesheets=[CSS(string=page_css(page))]
    )
    return base64.b64encode(buffer.getvalue()).decode("ascii")


_MM = {"a3": (297, 420), "a4": (210, 297), "a5": (148, 210), "letter": (215.9, 279.4),
       "legal": (215.9, 355.6), "tabloid": (279.4, 431.8)}


def _mm_list(margin):
    parts = [p for p in str(margin or "18mm 16mm").split() if p.endswith("mm")]
    vals = [float(p[:-2]) for p in parts] or [18.0, 16.0]
    if len(vals) == 1:
        vals = vals * 4
    elif len(vals) == 2:
        vals = vals * 2
    elif len(vals) == 3:
        vals = [vals[0], vals[1], vals[2], vals[1]]
    return vals[:4]


class _DocxBuilder:

    BLOCKS = {"p", "h1", "h2", "h3", "h4", "h5", "h6", "li", "blockquote", "pre", "dt", "dd"}

    def __init__(self, doc):
        self.doc = doc
        self.para = None
        self.marks = []
        self.lists = []
        self.tables = []
        self.skip = 0
        self.href = None
        self.block_style = []

    def _style(self, name):
        try:
            self.doc.styles[name]
            return name
        except KeyError:
            return None

    def _container(self):
        if self.tables and self.tables[-1]["cell"] is not None:
            return self.tables[-1]["cell"]
        return self.doc

    def _new_para(self, style=None):
        c = self._container()
        if self.tables and self.tables[-1]["cell"] is not None and self.tables[-1]["fresh"]:
            self.para = c.paragraphs[0]
            self.tables[-1]["fresh"] = False
        else:
            self.para = c.add_paragraph()
        st = style and self._style(style)
        if st:
            self.para.style = st
        return self.para

    def _ensure_para(self):
        if self.para is None:
            style = self.block_style[-1] if self.block_style else None
            self._new_para(style)
        return self.para

    def start(self, tag, attrs):
        a = dict(attrs)
        if self.skip or tag in ("style", "script", "head", "title"):
            self.skip += 1
            return
        cls = a.get("class") or ""
        if tag in ("h1", "h2", "h3", "h4", "h5", "h6"):
            self._new_para("Heading %s" % tag[1])
        elif tag == "p":
            self._new_para(self.block_style[-1] if self.block_style else None)
        elif tag == "blockquote":
            self.block_style.append("Quote")
            self.para = None
        elif tag == "pre":
            self._new_para()
            self.marks.append("code")
        elif tag in ("ul", "ol"):
            self.lists.append(tag)
            self.para = None
        elif tag == "li":
            depth = len(self.lists)
            kind = "List Number" if (self.lists and self.lists[-1] == "ol") else "List Bullet"
            name = kind if depth <= 1 else "%s %d" % (kind, min(depth, 3))
            self._new_para(name if self._style(name) else kind)
        elif tag == "br":
            self._ensure_para().add_run().add_break()
        elif tag == "hr":
            self.para = None
            self._new_para()
            self.para = None
        elif tag == "div" and "page-break" in cls:
            from docx.enum.text import WD_BREAK
            self._ensure_para().add_run().add_break(WD_BREAK.PAGE)
            self.para = None
        elif tag in ("strong", "b"):
            self.marks.append("b")
        elif tag in ("em", "i"):
            self.marks.append("i")
        elif tag == "u":
            self.marks.append("u")
        elif tag in ("s", "del", "strike"):
            self.marks.append("s")
        elif tag == "code":
            self.marks.append("code")
        elif tag == "a":
            self.href = a.get("href")
        elif tag == "table":
            self.para = None
            self.tables.append({"rows": [], "cell": None, "fresh": False})
        elif tag == "tr" and self.tables:
            self.tables[-1]["rows"].append([])
        elif tag in ("td", "th") and self.tables:
            self.tables[-1]["rows"][-1].append({"th": tag == "th", "html": []})
            self.tables[-1]["collect"] = True
        elif tag == "img":
            self._image(a.get("src") or "")
        elif tag in ("div", "section", "dl", "article"):
            self.para = None
        if self.tables and self.tables[-1].get("collect") and tag not in ("td", "th"):
            self.tables[-1]["rows"][-1][-1]["html"].append(("start", tag, attrs))

    def end(self, tag):
        if self.skip:
            if tag in ("style", "script", "head", "title"):
                self.skip -= 1
            return
        if self.tables and self.tables[-1].get("collect") and tag not in ("td", "th"):
            self.tables[-1]["rows"][-1][-1]["html"].append(("end", tag, None))
            return
        if tag in self.BLOCKS or tag in ("div", "section", "dl"):
            self.para = None
            if tag == "pre" and "code" in self.marks:
                self.marks.remove("code")
        if tag == "blockquote" and self.block_style:
            self.block_style.pop()
        elif tag in ("ul", "ol") and self.lists:
            self.lists.pop()
        elif tag in ("strong", "b") and "b" in self.marks:
            self.marks.remove("b")
        elif tag in ("em", "i") and "i" in self.marks:
            self.marks.remove("i")
        elif tag == "u" and "u" in self.marks:
            self.marks.remove("u")
        elif tag in ("s", "del", "strike") and "s" in self.marks:
            self.marks.remove("s")
        elif tag == "code" and "code" in self.marks:
            self.marks.remove("code")
        elif tag == "a":
            self.href = None
        elif tag in ("td", "th") and self.tables:
            self.tables[-1]["collect"] = False
        elif tag == "table" and self.tables:
            self._flush_table(self.tables.pop())

    def data(self, text):
        if self.skip:
            return
        if self.tables and self.tables[-1].get("collect"):
            self.tables[-1]["rows"][-1][-1]["html"].append(("data", text, None))
            return
        if not text.strip() and self.para is None:
            return
        if self.para is None:
            text = text.lstrip()
        run = self._ensure_para().add_run(text)
        run.bold = "b" in self.marks or None
        run.italic = "i" in self.marks or None
        run.underline = ("u" in self.marks or bool(self.href)) or None
        if "s" in self.marks:
            run.font.strike = True
        if "code" in self.marks:
            run.font.name = "Courier New"

    def _image(self, src):
        if not src.startswith("data:image/") or ";base64," not in src:
            return
        from docx.shared import Mm
        raw = base64.b64decode(src.split(";base64,", 1)[1])
        try:
            self._ensure_para().add_run().add_picture(io.BytesIO(raw), width=Mm(120))
        except Exception:
            pass
        self.para = None

    def _flush_table(self, t):
        rows = [r for r in t["rows"] if r]
        if not rows:
            return
        cols = max(len(r) for r in rows)
        outer = self.tables[-1]["cell"] if self.tables else None
        host = outer if outer is not None else self.doc
        table = host.add_table(rows=len(rows), cols=cols)
        if self._style("Table Grid"):
            table.style = "Table Grid"
        for i, r in enumerate(rows):
            for j, cell in enumerate(r):
                target = table.cell(i, j)
                sub = _DocxBuilder(self.doc)
                sub.tables = [{"rows": [], "cell": target, "fresh": True}]
                if cell["th"]:
                    sub.marks.append("b")
                for kind, x, attrs in cell["html"]:
                    if kind == "start":
                        sub.start(x, attrs)
                    elif kind == "end":
                        sub.end(x)
                    else:
                        sub.data(x)
        self.para = None


def to_docx(html, page):
    from html.parser import HTMLParser
    from docx import Document
    from docx.enum.section import WD_ORIENT
    from docx.shared import Mm

    page = page or {}
    doc = Document()
    sec = doc.sections[0]
    w, h = _MM.get(str(page.get("size") or "a4").lower(), _MM["a4"])
    if str(page.get("orientation") or "portrait").lower() == "landscape":
        w, h = h, w
        sec.orientation = WD_ORIENT.LANDSCAPE
    sec.page_width, sec.page_height = Mm(w), Mm(h)
    top, right, bottom, left = _mm_list(page.get("margin"))
    sec.top_margin, sec.right_margin = Mm(top), Mm(right)
    sec.bottom_margin, sec.left_margin = Mm(bottom), Mm(left)
    if page.get("header"):
        sec.header.paragraphs[0].text = str(page["header"])
    if page.get("footer"):
        sec.footer.paragraphs[0].text = str(page["footer"])

    builder = _DocxBuilder(doc)

    class P(HTMLParser):
        def handle_starttag(self, tag, attrs):
            builder.start(tag, attrs)

        def handle_startendtag(self, tag, attrs):
            builder.start(tag, attrs)
            if tag not in ("br", "img", "hr", "col", "meta"):
                builder.end(tag)

        def handle_endtag(self, tag):
            builder.end(tag)

        def handle_data(self, data):
            builder.data(data)

    P(convert_charrefs=True).feed(html)
    out = io.BytesIO()
    doc.save(out)
    return base64.b64encode(out.getvalue()).decode("ascii")


def _mark_safe(context, names):
    if not names:
        return context
    from markupsafe import Markup

    out = dict(context)
    for n in names:
        if isinstance(out.get(n), str):
            out[n] = Markup(out[n])
    return out


def run(job):
    fmt = str(job.get("format") or "html").lower()
    targets = set(job.get("targets") or ["html"])
    page = job.get("page") or {}
    context = _mark_safe(job.get("context") or {}, job.get("raw-html") or [])
    body = render_body(job["source"], fmt, context, bool(job.get("literal")))

    document = body
    if fmt != "html" or "pdf" in targets:
        document = (
            "<!doctype html><html><head><meta charset=\"utf-8\">"
            "<style>%s</style></head><body>%s</body></html>"
            % (page_css(page), body)
        )

    result = {"ok": True, "format": fmt}
    if "html" in targets or not targets & {"pdf", "docx"}:
        result["html"] = document if fmt != "html" else body
    if "pdf" in targets:
        result["pdf_b64"] = to_pdf(document, page)
    if "docx" in targets:
        result["docx_b64"] = to_docx(body, page)
    return result


SANDBOX_HINT = (
    "the sandbox refuses the Python object model (__class__, __dict__, mro, …) on"
    " purpose — test a value's type with `is number`, `is string`, `is mapping`,"
    " `is sequence`, `is none`"
)


def body_line(exc):
    lineno = getattr(exc, "lineno", None)
    if lineno is None:
        for frame in traceback.extract_tb(exc.__traceback__):
            if frame.filename == "<template>":
                lineno = frame.lineno
    return lineno


def describe_error(exc, job):
    name = job.get("name") or "template"
    lineno = body_line(exc)
    where = name if lineno is None else "%s, line %d" % (name, lineno)
    text = "%s: %s: %s" % (where, type(exc).__name__, exc)
    if lineno is not None:
        lines = str(job.get("source") or "").splitlines()
        if 0 < lineno <= len(lines):
            text += "\n  %d | %s" % (lineno, lines[lineno - 1].strip())
    if type(exc).__name__ == "SecurityError":
        text += "\n  " + SANDBOX_HINT
    return {"ok": False, "error": text, "template": name, "line": lineno}


def main():
    job = json.loads(sys.stdin.read() or "{}")
    try:
        out = run(job)
    except Exception as exc:
        out = describe_error(exc, job)
    sys.stdout.write(json.dumps(out))


if __name__ == "__main__":
    main()
