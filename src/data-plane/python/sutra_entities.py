import hashlib
import json
import re
import time
from datetime import date

from sutra_llm import LanguageModel, LanguageModelBudgetWait, parallel_map, tool_args
from sutra_runtime import cap_batch

LEAVES = ("string", "integer", "double", "date", "boolean")
EXACT, NORMALIZED, RELOCATED = "exact", "normalized", "relocated"
ABSENT, VALUE_NOT_IN_QUOTE, UNLOCATED = "absent", "value_not_in_quote", "unlocated"
LOCATED = (EXACT, NORMALIZED, RELOCATED)
FORMAT = 1
TEXT_MAX = 60000

INSTRUCTIONS = (
    "Fill the tool's entities from the document below. Each line starts with its id in square "
    "brackets. For every value give the id of the line it stands in and quote that line's words "
    "literally: the shortest piece holding the value. A value the document does not state has "
    "value null and empty line and quote. Do not guess. Answer with the tool call alone.")


def _norm(s):
    return re.sub(r"[^0-9a-zäöüß]", "", str(s or "").casefold())


def _spaced(s):
    return " ".join(str(s or "").split()).casefold()


def _edges(b):
    return b["left"], b["top"], b["left"] + b["width"], b["top"] + b["height"]


def _union(boxes):
    es = [_edges(b) for b in boxes]
    x0, y0 = min(e[0] for e in es), min(e[1] for e in es)
    x1, y1 = max(e[2] for e in es), max(e[3] for e in es)
    return {"left": round(x0, 4), "top": round(y0, 4),
            "width": round(x1 - x0, 4), "height": round(y1 - y0, 4)}


def _page_lines(page, words):
    rows = []
    for w in sorted(words, key=lambda w: (w["_mid"], w["bounding_box"]["left"])):
        h = max(w["bounding_box"]["height"], 1e-6)
        if rows and abs(w["_mid"] - rows[-1]["mid"]) <= 0.5 * min(h, rows[-1]["h"]):
            rows[-1]["words"].append(w)
        else:
            rows.append({"mid": w["_mid"], "h": h, "words": [w]})
    out = []
    for n, r in enumerate(rows, 1):
        ws = sorted(r["words"], key=lambda w: w["bounding_box"]["left"])
        out.append({"id": f"p{page}-l{n}", "page": page, "words": ws,
                    "text": " ".join(w["text"] for w in ws)})
    return out


def lines_of(blocks):
    pages = {}
    for b in blocks or []:
        ws = [dict(w, _block=b["block_id"],
                   _mid=w["bounding_box"]["top"] + w["bounding_box"]["height"] / 2)
              for w in b.get("words") or [] if str(w.get("text") or "").strip()]
        if not ws and str(b.get("content") or "").strip():
            ws = [{"text": " ".join(str(b["content"]).split()), "_block": b["block_id"],
                   "bounding_box": b["bounding_box"],
                   "_mid": b["bounding_box"]["top"] + b["bounding_box"]["height"] / 2}]
        pages.setdefault(int(b["page"]), []).extend(ws)
    return [ln for p in sorted(pages) for ln in _page_lines(p, pages[p])]


def text_of(lines):
    s = "\n".join(f"[{ln['id']}] {ln['text']}" for ln in lines).replace("\x00", "")
    return s if len(s) <= TEXT_MAX else s[:TEXT_MAX] + "\n…"


def _span(line, quote):
    want, ws = _norm(quote), line["words"]
    if not want:
        return None
    best = None
    for i in range(len(ws)):
        got = ""
        for j in range(i, len(ws)):
            got += _norm(ws[j]["text"])
            if want in got:
                if best is None or j - i < len(best) - 1:
                    best = ws[i:j + 1]
                break
    return best


def locate(lines, line_id, quote):
    named = next((ln for ln in lines if ln["id"] == str(line_id or "").strip()), None)
    words = _span(named, quote) if named else None
    if words:
        return (EXACT if _spaced(quote) in _spaced(named["text"]) else NORMALIZED), named, words
    others = [(ln, _span(ln, quote)) for ln in lines]
    others = [(ln, w) for ln, w in others if w]
    return (RELOCATED, *others[0]) if len(others) == 1 else (UNLOCATED, None, None)


def _digits(s):
    return re.sub(r"\D", "", str(s))


def _number_in(value, quote):
    v = abs(float(value))
    forms = {f"{v:.2f}", f"{v:.1f}", f"{v:f}".rstrip("0").rstrip("."), str(int(v)) if v == int(v)
             else ""}
    got = _digits(quote)
    return any(f and _digits(f) in got for f in forms)


def _date_in(value, quote):
    d = date.fromisoformat(str(value))
    forms = (f"{d.day:02d}{d.month:02d}{d.year}", f"{d.day}{d.month}{d.year}",
             f"{d.day:02d}{d.month:02d}{d.year % 100:02d}", d.strftime("%Y%m%d"))
    return any(f in _digits(quote) for f in forms)


def value_in_quote(kind, value, quote):
    if kind in ("integer", "double"):
        return _number_in(value, quote)
    if kind == "date":
        return _date_in(value, quote)
    if kind == "boolean":
        return True
    return _norm(value) in _norm(quote)


def _coerce(kind, v):
    try:
        if kind == "string":
            return str(v)
        if kind == "integer":
            if isinstance(v, bool) or float(v) != int(float(v)):
                raise ValueError
            return int(float(v))
        if kind == "double":
            if isinstance(v, bool):
                raise ValueError
            return float(v)
        if kind == "date":
            return date.fromisoformat(str(v)).isoformat()
        if not isinstance(v, bool):
            raise ValueError
        return v
    except (TypeError, ValueError):
        raise ValueError(f"Cannot coerce to provided output type: {v!r} is no {kind}") from None


def _spec(s):
    s = {"type": s} if isinstance(s, str) else s
    if s.get("type") not in LEAVES + ("array", "struct"):
        raise ValueError(f"entity type {s.get('type')!r} — one of "
                         f"{list(LEAVES) + ['array', 'struct']}")
    return s


def _leaf_schema(s):
    wire = {"string": "string", "integer": "integer", "double": "number", "date": "string",
            "boolean": "boolean"}[s["type"]]
    value = {"type": [wire, "null"]}
    if s.get("description") or s["type"] == "date":
        value["description"] = (s.get("description") or "") + (
            " As YYYY-MM-DD." if s["type"] == "date" else "")
    return {"type": "object", "additionalProperties": False,
            "properties": {"value": value, "line": {"type": "string"}, "quote": {"type": "string"}},
            "required": ["value", "line", "quote"]}


def _schema(s):
    s = _spec(s)
    if s["type"] == "array":
        out = {"type": "array", "items": _schema(s["items"])}
    elif s["type"] == "struct":
        out = _struct_schema(s["fields"])
    else:
        return _leaf_schema(s)
    if s.get("description"):
        out["description"] = s["description"]
    return out


def _struct_schema(fields):
    return {"type": "object", "additionalProperties": False,
            "properties": {k: _schema(v) for k, v in fields.items()},
            "required": list(fields)}


def entity_tool(entities):
    return {"type": "function",
            "function": {"name": "entities", "description": "The entities of this document.",
                         "parameters": _struct_schema(entities)}}


def _resolved(s, got, lines, problems, path):
    s = _spec(s)
    if s["type"] == "array":
        if not isinstance(got, list):
            raise ValueError(f"Cannot coerce to provided output type: {path} is no array")
        return [_resolved(s["items"], g, lines, problems, f"{path}[{i}]")
                for i, g in enumerate(got)]
    if s["type"] == "struct":
        return _fields(s["fields"], got, lines, problems, path)
    return _field(s["type"], got, lines, problems, path)


def _fields(fields, got, lines, problems, path=""):
    if not isinstance(got, dict) or set(got) - set(fields) or set(fields) - set(got):
        raise ValueError(f"Cannot coerce to provided output type: {path or 'the answer'} must "
                         f"hold exactly {sorted(fields)}")
    return {k: _resolved(v, got[k], lines, problems, f"{path}.{k}" if path else k)
            for k, v in fields.items()}


def _field(kind, got, lines, problems, path):
    if not isinstance(got, dict):
        raise ValueError(f"Cannot coerce to provided output type: {path} is no value object")
    raw, quote, line_id = got.get("value"), got.get("quote") or "", got.get("line") or ""
    if raw is None or raw == "":
        return {"value": None, "status": ABSENT}
    value = _coerce(kind, raw)
    status, line, words = locate(lines, line_id, quote)
    out = {"value": value, "quote": quote, "line_id": line["id"] if line else (line_id or None),
           "status": status}
    if status == UNLOCATED:
        problems.append(f"{path}: the quote {quote!r} does not stand in line {line_id!r}")
        return out
    if not value_in_quote(kind, value, quote):
        out["status"] = VALUE_NOT_IN_QUOTE
        problems.append(f"{path}: the value {value!r} is not in its quote {quote!r}")
    block = max({w["_block"] for w in words}, key=[w["_block"] for w in words].count)
    return {**out, "block_id": block, "page": line["page"],
            "box": _union([w["bounding_box"] for w in words])}


def _usage(answer):
    u = (answer or {}).get("usage") or {}
    return u.get("prompt_tokens", u.get("input_tokens")) or 0, \
        u.get("completion_tokens", u.get("output_tokens")) or 0


def _ask(model, entities, prompt, text, max_tokens, correction=None):
    tool = entity_tool(entities)
    content = [{"type": "text", "text": prompt or ""}, {"type": "text", "text": INSTRUCTIONS},
               {"type": "text", "text": "DOCUMENT:\n\n" + text}]
    if correction:
        content.append({"type": "text", "text": correction})
    answer = model.chat([{"role": "user", "content": content}], tools=[tool],
                        max_tokens=max_tokens)
    got = tool_args(answer, tool["function"]["name"])
    if got is None:
        raise ValueError("Cannot coerce to provided output type: the model called no tool")
    return got, _usage(answer)


def extract_entities(blocks, entities, model, prompt=None, reask=1, max_tokens=4000):
    lines = lines_of(blocks)
    text = text_of(lines)
    tin = tout = 0
    correction, out = None, None
    for attempt in range(reask + 1):
        got, (i, o) = _ask(model, entities, prompt, text, max_tokens, correction)
        tin, tout = tin + i, tout + o
        problems = []
        out = _fields(entities, got, lines, problems)
        if not problems:
            return {"entities": out, "tokens_in": tin, "tokens_out": tout, "reasked": attempt}
        correction = ("Your previous answer failed these checks. Answer again with the tool, "
                      "quoting literally:\n" + "\n".join(problems))
    return {"entities": out, "tokens_in": tin, "tokens_out": tout, "reasked": reask}


def strategy_id(entities, prompt, model):
    kept = {"format": FORMAT, "entities": entities, "prompt": prompt or "", "model": model or ""}
    canonical = json.dumps(kept, sort_keys=True, ensure_ascii=False)
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()[:12]


def _blocks(v):
    return json.loads(v) if isinstance(v, str) else (v or [])


def _input_key(blocks):
    return hashlib.sha256(text_of(lines_of(blocks)).encode("utf-8")).hexdigest()[:16]


def documents_of(pages, id_column="media_reference", blocks_column="layout_info"):
    docs = {}
    for p in sorted(pages or [], key=lambda r: int(r.get("page_num") or 0)):
        doc = docs.setdefault(p.get(id_column), {id_column: p.get(id_column), blocks_column: []})
        doc[blocks_column] += _blocks(p.get(blocks_column))
    return list(docs.values())


def _row(key, doc_id, sid, input_key, **values):
    return {key: doc_id, "strategy_id": sid, "input_key": input_key, "entities": None,
            "error": None, "tokens_in": None, "tokens_out": None, "ms": None, "reasked": None,
            **values}


def create_entity_extraction(rows, entities, model, prompt=None, id_column="id",
                             blocks_column="layout_info", previous=None, reask=1,
                             thread_number=8, max_tokens=4000):
    sid = strategy_id(entities, prompt, getattr(model, "model", None))
    docs = [(r[id_column], _blocks(r.get(blocks_column))) for r in rows
            if r.get(id_column) is not None]
    if not docs:
        raise ValueError("no documents to read: a replace write would delete every row")
    before = {r.get(id_column): dict(r) for r in previous or []}
    fresh, failed, carried = [], [], []
    for doc_id, blocks in docs:
        if not blocks:
            continue
        ikey, old = _input_key(blocks), before.get(doc_id)
        if not old or old.get("strategy_id") != sid or old.get("input_key") != ikey:
            fresh.append((doc_id, blocks, ikey))
        elif old.get("error") or not old.get("entities"):
            failed.append((doc_id, blocks, ikey))
        else:
            carried.append(old)
    todo = cap_batch(fresh + failed)
    picked = {t[0] for t in todo}
    carried += [before[t[0]] for t in fresh + failed
                if t[0] not in picked and t[0] in before]

    def read(t):
        doc_id, blocks, ikey = t
        t0 = time.perf_counter()
        try:
            r = extract_entities(blocks, entities, model, prompt, reask, max_tokens)
            return _row(id_column, doc_id, sid, ikey,
                        entities=json.dumps(r["entities"], ensure_ascii=False),
                        tokens_in=r["tokens_in"], tokens_out=r["tokens_out"],
                        reasked=r["reasked"], ms=round((time.perf_counter() - t0) * 1000))
        except LanguageModelBudgetWait:
            raise
        except Exception as e:
            return _row(id_column, doc_id, sid, ikey, error=f"{type(e).__name__}: {e}")

    return carried + parallel_map(read, todo, max_workers=thread_number)


class _EntityExtractor:

    def __init__(self, model):
        self.model = model

    def extract_entities(self, blocks, entities, prompt=None, reask=1, max_tokens=4000):
        return extract_entities(blocks, entities, self.model, prompt, reask, max_tokens)

    def create_entity_extraction(self, rows, entities, prompt=None, id_column="id",
                                 blocks_column="layout_info", reask=1, thread_number=8,
                                 max_tokens=4000):
        from sutra_transforms import previous_output
        return create_entity_extraction(rows, entities, self.model, prompt, id_column,
                                        blocks_column, previous_output(), reask, thread_number,
                                        max_tokens)


class EntityExtractorInput(LanguageModel):

    def __init__(self, model, **kw):
        super().__init__(model=model, **kw)

    def bind(self, model):
        return _EntityExtractor(model)
