import base64
import html
import io
import os
import statistics
import subprocess

from sutra_runtime import span

MODES = ("AUTO", "ELECTRONIC", "SCAN")
FORMATS = ("TEXT", "MARKDOWN", "HTML")
LAYOUT_INFO = (None, "no_overlay", "crop_tables")
DEFAULT_LANGUAGES = ("deu",)
MIN_TEXT_CHARS = 24
OCR_DPI = 300
OCR_TIMEOUT_S = 180
VISION_EDGE_PX = 1568
MEDIA_IMAGE_BYTES_MAX = 12 * 1024 * 1024


class MediaToolMissing(RuntimeError):
    pass


def _pdfplumber():
    try:
        import pdfplumber
    except ImportError as e:
        raise MediaToolMissing(
            "pdfplumber is unavailable in the transform sandbox — the image "
            "must `pip install pdfplumber` into the data-plane image") from e
    return pdfplumber


def kind(data: bytes) -> str:
    if data[:4] == b"%PDF":
        return "pdf"
    if data[:8] == b"\x89PNG\r\n\x1a\n" or data[:3] == b"\xff\xd8\xff" \
            or data[:4] in (b"GIF8", b"II*\x00", b"MM\x00*") \
            or (data[:4] == b"RIFF" and data[8:12] == b"WEBP"):
        return "image"
    raise ValueError("media item is neither a PDF nor an image")


def _check(mode, fmt=None):
    if mode not in MODES:
        raise ValueError(f"mode {mode!r} — one of {', '.join(MODES)}")
    if fmt is not None and fmt not in FORMATS:
        raise ValueError(f"format {fmt!r} — one of {', '.join(FORMATS)}")


def _pages(n, start_page, end_page):
    return list(range(n))[start_page or 0:end_page]


class _Doc:

    def __init__(self, data: bytes):
        self.kind = kind(data)
        self._data = data
        self._pdf = None
        self._img = None

    def __enter__(self):
        if self.kind == "pdf":
            self._pdf = _pdfplumber().open(io.BytesIO(self._data))
        else:
            from PIL import Image
            self._img = Image.open(io.BytesIO(self._data)).convert("RGB")
        return self

    def __exit__(self, *exc):
        if self._pdf is not None:
            self._pdf.close()

    def count(self):
        return len(self._pdf.pages) if self._pdf is not None else 1

    def page(self, i):
        return self._pdf.pages[i] if self._pdf is not None else None

    def size(self, i):
        if self._pdf is not None:
            p = self._pdf.pages[i]
            return float(p.width), float(p.height), "pt"
        return float(self._img.width), float(self._img.height), "px"

    def electronic(self, i):
        p = self.page(i)
        if p is None:
            return False
        return sum(1 for c in p.chars if not c.get("text", "").isspace()) >= MIN_TEXT_CHARS

    def render(self, i, dpi=None, fit=None):
        if self._pdf is None:
            img = self._img
            if fit:
                img = img.copy()
                img.thumbnail(fit)
            return img
        p = self._pdf.pages[i]
        if fit:
            dpi = 72.0 * min(fit[0] / float(p.width), fit[1] / float(p.height))
        return p.to_image(resolution=dpi or OCR_DPI).original.convert("RGB")


def _encode(img, fmt="PNG"):
    out = io.BytesIO()
    img.save(out, format="JPEG" if fmt.upper() in ("JPG", "JPEG") else fmt.upper())
    return out.getvalue()


def _tesseract(img, languages, scripts, out):
    langs = "+".join(list(languages or DEFAULT_LANGUAGES)
                     + [f"script/{s}" for s in (scripts or [])])
    argv = ["tesseract", "stdin", "stdout", "-l", langs, "--dpi", str(OCR_DPI)]
    if out != "txt":
        argv.append(out)
    env = dict(os.environ, OMP_THREAD_LIMIT="1")
    try:
        with span('ocr', languages=langs):
            r = subprocess.run(argv, input=_encode(img), capture_output=True,
                               timeout=OCR_TIMEOUT_S, env=env)
    except FileNotFoundError as e:
        raise MediaToolMissing(
            "tesseract is unavailable in the transform sandbox — the data-plane "
            "image must install tesseract-ocr and its language packs") from e
    if r.returncode != 0:
        raise RuntimeError(f"tesseract failed ({langs}): "
                           + r.stderr.decode("utf-8", "replace").strip())
    return r.stdout.decode("utf-8", "replace")


def _box(x0, top, x1, bottom, w, h):
    return {"left": round(x0 / w, 4), "top": round(top / h, 4),
            "width": round((x1 - x0) / w, 4), "height": round((bottom - top) / h, 4)}


def _classify(blocks):
    sizes = [b["_size"] for b in blocks if b["block_type"] == "TEXT" and b["_size"]]
    median = statistics.median(sizes) if sizes else 0
    for b in blocks:
        if b["block_type"] != "TEXT":
            continue
        bb = b["bounding_box"]
        if bb["top"] + bb["height"] <= 0.06:
            b["block_type"] = "PAGE_HEADER"
        elif bb["top"] >= 0.94:
            b["block_type"] = "PAGE_FOOTER"
        elif median and b["_size"] >= 1.25 * median and b["content"].count("\n") < 2:
            b["block_type"] = "TITLE"


def _table_text(rows):
    return "\n".join(" | ".join(r) for r in rows)


def _table_markdown(rows):
    width = max(len(r) for r in rows)
    rows = [r + [""] * (width - len(r)) for r in rows]
    esc = [[c.replace("|", "\\|").replace("\n", " ") for c in r] for r in rows]
    head, body = esc[0], esc[1:]
    return "\n".join(["| " + " | ".join(head) + " |", "|" + "---|" * width]
                     + ["| " + " | ".join(r) + " |" for r in body])


def _table_html(rows):
    return "<table>" + "".join(
        "<tr>" + "".join(f"<td>{html.escape(c)}</td>" for c in r) + "</tr>"
        for r in rows) + "</table>"


def _tables(page):
    try:
        found = page.find_tables()
    except Exception:
        return []
    out = []
    for t in found:
        rows = [[(c or "").strip() for c in r] for r in (t.extract() or [])]
        if len(rows) >= 2 and max(len(r) for r in rows) >= 2:
            out.append({"bbox": t.bbox, "rows": rows})
    return out


def _lines(words):
    lines = []
    for w in sorted(words, key=lambda w: (w["top"], w["x0"])):
        tol = 0.5 * (w.get("size") or 10)
        if lines and abs(lines[-1][0]["top"] - w["top"]) <= tol:
            lines[-1].append(w)
        else:
            lines.append([w])
    return [sorted(ln, key=lambda w: w["x0"]) for ln in lines]


def _segments(line):
    segs = [[line[0]]]
    for w in line[1:]:
        prev = segs[-1][-1]
        if w["x0"] - prev["x1"] > 3 * (w.get("size") or 10):
            segs.append([w])
        else:
            segs[-1].append(w)
    return [{"x0": min(w["x0"] for w in s), "x1": max(w["x1"] for w in s),
             "top": min(w["top"] for w in s), "bottom": max(w["bottom"] for w in s),
             "size": statistics.mean(w.get("size") or 10 for w in s),
             "text": " ".join(w["text"] for w in s), "words": s} for s in segs]


def _words(ws, w, h):
    return [{"text": x["text"], "bounding_box": _box(x["x0"], x["top"], x["x1"], x["bottom"], w, h)}
            for x in ws]


def _electronic_blocks(page):
    page = page.dedupe_chars()
    W, H = float(page.width), float(page.height)
    tables = _tables(page)

    def in_table(w):
        cx, cy = (w["x0"] + w["x1"]) / 2, (w["top"] + w["bottom"]) / 2
        return any(x0 <= cx <= x1 and t0 <= cy <= t1
                   for x0, t0, x1, t1 in (t["bbox"] for t in tables))

    alle = page.extract_words(extra_attrs=["size"])
    words = [w for w in alle if not in_table(w)]
    groups = []
    for line in _lines(words):
        for s in _segments(line):
            gap = 0.8 * s["size"]
            home = next((g for g in reversed(groups)
                         if 0 <= s["top"] - g["bottom"] <= gap + 0.6 * s["size"]
                         and s["x0"] < g["x1"] and s["x1"] > g["x0"]
                         and abs(s["size"] - g["size"]) <= 1.5), None)
            if home is None:
                groups.append(dict(s, lines=[s["text"]], sizes=[s["size"]], words=list(s["words"])))
            else:
                home["lines"].append(s["text"])
                home["sizes"].append(s["size"])
                home["words"] += s["words"]
                home.update(x0=min(home["x0"], s["x0"]), x1=max(home["x1"], s["x1"]),
                            bottom=max(home["bottom"], s["bottom"]))
    blocks = [{"block_type": "TEXT", "content": "\n".join(g["lines"]), "table_html": None,
               "bounding_box": _box(g["x0"], g["top"], g["x1"], g["bottom"], W, H),
               "confidence": 1.0, "words": _words(g["words"], W, H),
               "_size": statistics.mean(g["sizes"])} for g in groups]

    def of_table(t):
        x0, t0, x1, t1 = t["bbox"]
        return [w for w in alle
                if x0 <= (w["x0"] + w["x1"]) / 2 <= x1 and t0 <= (w["top"] + w["bottom"]) / 2 <= t1]

    blocks += [{"block_type": "TABLE", "content": _table_text(t["rows"]),
                "table_html": _table_html(t["rows"]), "_rows": t["rows"],
                "bounding_box": _box(*t["bbox"], W, H), "confidence": 1.0,
                "words": _words(of_table(t), W, H), "_size": None}
               for t in tables]
    return blocks, []


def _ocr_blocks(img, languages, scripts):
    W, H = float(img.width), float(img.height)
    groups = {}
    for row in _tesseract(img, languages, scripts, "tsv").splitlines()[1:]:
        f = row.split("\t")
        if len(f) < 12 or f[0] != "5" or not f[11].strip() or float(f[10]) < 0:
            continue
        key = (int(f[2]), int(f[3]))
        left, top, w, h = (int(x) for x in f[6:10])
        g = groups.setdefault(key, {"lines": {}, "conf": [], "heights": [], "words": [],
                                    "x0": left, "top": top, "x1": left + w, "bottom": top + h})
        g["lines"].setdefault(int(f[4]), []).append(f[11].strip())
        g["words"].append({"text": f[11].strip(),
                           "bounding_box": _box(left, top, left + w, top + h, W, H)})
        g["conf"].append(float(f[10]))
        g["heights"].append(h)
        g.update(x0=min(g["x0"], left), top=min(g["top"], top),
                 x1=max(g["x1"], left + w), bottom=max(g["bottom"], top + h))
    langs = list(languages or DEFAULT_LANGUAGES)
    blocks = [{"block_type": "TEXT",
               "content": "\n".join(" ".join(ws) for _, ws in sorted(g["lines"].items())),
               "table_html": None,
               "bounding_box": _box(g["x0"], g["top"], g["x1"], g["bottom"], W, H),
               "confidence": round(statistics.mean(g["conf"]) / 100.0, 3),
               "words": g["words"],
               "_size": statistics.median(g["heights"])}
              for _, g in sorted(groups.items())]
    return blocks, langs


def _refuse_electronic_image(doc, mode):
    if mode == "ELECTRONIC" and doc.kind == "image":
        raise ValueError("mode ELECTRONIC needs a PDF with a text layer, not an image")


def _page_blocks(doc, i, mode, languages, scripts):
    _refuse_electronic_image(doc, mode)
    electronic = mode == "ELECTRONIC" or (mode == "AUTO" and doc.electronic(i))
    if electronic:
        blocks, langs = _electronic_blocks(doc.page(i))
    else:
        blocks, langs = _ocr_blocks(doc.render(i, dpi=OCR_DPI), languages, scripts)
    _classify(blocks)
    blocks.sort(key=lambda b: (b["bounding_box"]["top"], b["bounding_box"]["left"]))
    for n, b in enumerate(blocks):
        b.update(block_index=n, block_id=f"p{i}-b{n}", page=i, languages=langs)
    return blocks


def _public(b):
    return {k: v for k, v in b.items() if not k.startswith("_")}


def _render_blocks(blocks, fmt):
    out = []
    for b in blocks:
        t, rows = b["block_type"], b.get("_rows")
        if fmt == "MARKDOWN":
            out.append(_table_markdown(rows) if rows else
                       ("# " + b["content"].replace("\n", " ") if t == "TITLE"
                        else b["content"]))
        elif fmt == "HTML":
            tag = {"TITLE": "h1", "PAGE_HEADER": "header", "PAGE_FOOTER": "footer"}.get(t, "p")
            out.append(b["table_html"] if rows else
                       f"<{tag}>" + html.escape(b["content"]).replace("\n", "<br>")
                       + f"</{tag}>")
        else:
            out.append(b["content"])
    return "\n\n".join(out)


def page_count(data: bytes) -> int:
    with _Doc(data) as d:
        return d.count()


def page_dimensions(data: bytes):
    with _Doc(data) as d:
        return [dict(zip(("width", "height", "unit"), d.size(i)), page=i)
                for i in range(d.count())]


def extract_raw_text(data: bytes, start_page=0, end_page=None, layout=False):
    with _Doc(data) as d:
        if d.kind != "pdf":
            raise ValueError("extract_raw_text reads a PDF text layer — use ocr() on images")
        return [d.page(i).extract_text(layout=layout) or ""
                for i in _pages(d.count(), start_page, end_page)]


def ocr(data: bytes, languages=None, scripts=None, start_page=0, end_page=None,
        output_format="text"):
    if output_format not in ("text", "hocr"):
        raise ValueError(f"output_format {output_format!r} — 'text' or 'hocr'")
    out = "txt" if output_format == "text" else "hocr"
    with _Doc(data) as d:
        return [_tesseract(d.render(i, dpi=OCR_DPI), languages, scripts, out)
                for i in _pages(d.count(), start_page, end_page)]


def extract_layout_aware_text(data: bytes, mode="AUTO", languages=None, scripts=None,
                              start_page=0, end_page=None):
    _check(mode)
    with _Doc(data) as d:
        return [[_public(b) for b in _page_blocks(d, i, mode, languages, scripts)]
                for i in _pages(d.count(), start_page, end_page)]


def extract_text(data: bytes, mode="AUTO", format="TEXT", languages=None, scripts=None,
                 start_page=0, end_page=None, layout=True):
    _check(mode, format)
    with _Doc(data) as d:
        _refuse_electronic_image(d, mode)
        pages = []
        for i in _pages(d.count(), start_page, end_page):
            if format == "TEXT" and d.kind == "pdf" and (
                    mode == "ELECTRONIC" or (mode == "AUTO" and d.electronic(i))):
                pages.append(d.page(i).extract_text(layout=layout) or "")
            elif format == "TEXT":
                pages.append(_tesseract(d.render(i, dpi=OCR_DPI), languages, scripts, "txt"))
            else:
                pages.append(_render_blocks(_page_blocks(d, i, mode, languages, scripts),
                                            format))
        return pages


def convert_document_to_images(data: bytes, start_page=0, end_page=None, width=1024,
                               height=1024, output_format="PNG"):
    with _Doc(data) as d:
        return [_encode(d.render(i, fit=(width, height)), output_format)
                for i in _pages(d.count(), start_page, end_page)]


def _image_part(img):
    url = "data:image/jpeg;base64," + base64.b64encode(_encode(img, "JPEG")).decode("ascii")
    return {"type": "image_url", "image_url": {"url": url}}


def llm_parts(data: bytes, mode="AUTO", start_page=0, end_page=None, with_ocr=False,
              languages=None, scripts=None, layout=True):
    _check(mode)
    with _Doc(data) as d:
        _refuse_electronic_image(d, mode)
        n, parts, image_bytes = d.count(), [], 0
        for i in _pages(n, start_page, end_page):
            label = f"page {i + 1} of {n}"
            if d.kind == "pdf" and (mode == "ELECTRONIC" or
                                    (mode == "AUTO" and d.electronic(i))):
                text = d.page(i).extract_text(layout=layout) or ""
                if text.strip():
                    parts.append({"type": "text", "text": f"[{label}, text layer]\n{text}"})
                continue
            part = _image_part(d.render(i, fit=(VISION_EDGE_PX, VISION_EDGE_PX)))
            image_bytes += len(part["image_url"]["url"])
            if image_bytes > MEDIA_IMAGE_BYTES_MAX:
                raise ValueError(f"the page images exceed {MEDIA_IMAGE_BYTES_MAX} bytes — "
                                 "bound the pages (end_page) or read the text (ELECTRONIC)")
            parts += [{"type": "text", "text": f"[{label}, scanned image]"}, part]
            if with_ocr:
                ocr_text = _tesseract(d.render(i, dpi=OCR_DPI), languages, scripts, "txt")
                parts.append({"type": "text", "text": f"[{label}, OCR text]\n{ocr_text}"})
        return parts


def vision_parts(data: bytes, page: int, with_ocr=False, include_layout_info=None,
                 languages=None, scripts=None):
    if include_layout_info not in LAYOUT_INFO:
        raise ValueError(f"include_layout_info {include_layout_info!r} — "
                         "None, 'no_overlay' or 'crop_tables'")
    import json
    with _Doc(data) as d:
        img = d.render(page, fit=(VISION_EDGE_PX, VISION_EDGE_PX))
        parts = [_image_part(img)]
        if with_ocr:
            text = extract_text(data, "AUTO", "TEXT", languages, scripts, page, page + 1)[0]
            parts.append({"type": "text", "text": f"[text of page {page + 1}]\n{text}"})
        if include_layout_info:
            blocks = _page_blocks(d, page, "AUTO", languages, scripts)
            slim = [{"id": b["block_id"], "type": b["block_type"], "content": b["content"],
                     "box": b["bounding_box"]} for b in blocks]
            parts.append({"type": "text", "text":
                          f"[layout blocks of page {page + 1}; box = fractions of the page, "
                          "left/top from the upper-left corner]\n"
                          + json.dumps(slim, ensure_ascii=False)})
            if include_layout_info == "crop_tables":
                hi = d.render(page, dpi=OCR_DPI) if d.kind == "pdf" else img
                for b in blocks:
                    if b["block_type"] != "TABLE":
                        continue
                    bb = b["bounding_box"]
                    crop = hi.crop((int(bb["left"] * hi.width), int(bb["top"] * hi.height),
                                    int((bb["left"] + bb["width"]) * hi.width),
                                    int((bb["top"] + bb["height"]) * hi.height)))
                    crop.thumbnail((VISION_EDGE_PX, VISION_EDGE_PX))
                    parts += [{"type": "text", "text": f"[table {b['block_id']}, cropped]"},
                              _image_part(crop)]
        return parts
