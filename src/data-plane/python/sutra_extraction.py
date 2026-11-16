import hashlib
import json
import re
import time

import sutra_media as _media
from sutra_entities import extract_entities
from sutra_llm import LanguageModel, LanguageModelBudgetWait, parallel_map
from sutra_runtime import cap_batch, cpu_map

STRATEGIES = ("raw-text", "ocr", "layout-ocr", "vlm")
PREPROCESS = {None: (False, None), "none": (False, None), "ocr": (True, None),
              "layout": (False, "no_overlay"), "layout-tables": (False, "crop_tables")}
LAYOUT_INFO = {"no_overlay": "layout", "crop_tables": "layout-tables"}
ENTITY_PAGES = 20
CRITERIA = ("text", "tables", "headings", "lists", "reading_order", "completeness")

DEFAULT_PROMPT = (
    "You convert one page of a document into GitHub-flavoured Markdown. Reproduce every "
    "piece of text on the page, in reading order, without summarising, translating or "
    "correcting it. Headings become #-headings by their level, lists stay lists, tables "
    "become Markdown tables with one row per printed row and every cell kept (merge "
    "nothing, invent nothing; an empty cell stays empty). Numbers, dates and amounts are "
    "copied character for character. Leave out nothing but purely decorative elements. "
    "Answer with the Markdown alone, no preface and no code fence around it.")

JUDGE_PROMPT = (
    "You judge how well a Markdown extraction reproduces the page shown in the image. "
    "Score each criterion from 1 (worst) to 5 (best), or null when the page has no such "
    "element: text (characters, numbers and amounts exact), tables (rows, columns and "
    "cells intact), headings (present and at the right level), lists (kept as lists), "
    "reading_order (the order a reader follows), completeness (nothing left out, nothing "
    "invented). Answer with JSON alone: {\"scores\": {\"text\": n, \"tables\": n, "
    "\"headings\": n, \"lists\": n, \"reading_order\": n, \"completeness\": n}, "
    "\"notes\": \"one or two sentences naming the worst defects\"}.")


def _usage(answer):
    u = (answer or {}).get("usage") or {}
    return (u.get("prompt_tokens", u.get("input_tokens")),
            u.get("completion_tokens", u.get("output_tokens")))


def _content(answer):
    choice = ((answer or {}).get("choices") or [{}])[0]
    c = choice.get("message", {}).get("content")
    if isinstance(c, list):
        c = "".join(p.get("text", "") for p in c if isinstance(p, dict))
    if not c and choice.get("finish_reason") == "length":
        raise ValueError("the model spent max_tokens before answering (thinking counts against "
                         "it) — raise max_tokens")
    return c or ""


def _unfence(text):
    m = re.match(r"^\s*```[a-zA-Z]*\n(.*)\n```\s*$", text, re.S)
    return m.group(1) if m else text


def _layout(data, page, mode, languages):
    with _media._Doc(data) as d:
        blocks = _media._page_blocks(d, page, mode, languages, None)
        return (_media._render_blocks(blocks, "MARKDOWN"),
                [_media._public(b) for b in blocks])


def _vlm(data, page, cfg, model):
    if model is None:
        raise ValueError("the vlm strategy needs a model")
    pre = cfg.get("preprocess")
    if pre not in PREPROCESS:
        raise ValueError(f"preprocess {pre!r} — one of {sorted(k for k in PREPROCESS if k)}")
    with_ocr, layout_info = PREPROCESS[pre]
    parts = _media.vision_parts(data, page, with_ocr, layout_info,
                                cfg.get("languages") or None)
    parts.append({"type": "text", "text": f"Extract page {page + 1} as Markdown."})
    answer = model.chat([{"role": "system", "content": cfg.get("prompt") or DEFAULT_PROMPT},
                         {"role": "user", "content": parts}],
                        max_tokens=int(cfg.get("max_tokens") or 4096))
    return _unfence(_content(answer)), _usage(answer)


def extract(data: bytes, page: int, cfg: dict, model=None) -> dict:
    s, mode = cfg.get("strategy"), cfg.get("mode") or "AUTO"
    languages = cfg.get("languages") or None
    t0 = time.perf_counter()
    blocks, tokens = None, (None, None)
    if s == "raw-text":
        md = _media.extract_raw_text(data, page, page + 1)[0]
    elif s == "ocr":
        md = _media.extract_text(data, "SCAN", "TEXT", languages, None, page, page + 1)[0]
    elif s == "layout-ocr":
        md, blocks = _layout(data, page, mode, languages)
    elif s == "vlm":
        md, tokens = _vlm(data, page, cfg, model)
        if PREPROCESS[cfg.get("preprocess")][1]:
            blocks = _layout(data, page, mode, languages)[1]
    else:
        raise ValueError(f"strategy {s!r} — one of {list(STRATEGIES)}")
    ms = round((time.perf_counter() - t0) * 1000)
    size = _media.page_dimensions(data)[page]
    return {"page": page, "markdown": md,
            "blocks": json.dumps(blocks, ensure_ascii=False) if blocks is not None else None,
            "ms": ms, "tokens_in": tokens[0], "tokens_out": tokens[1],
            "page_count": _media.page_count(data),
            "page_width": size["width"], "page_height": size["height"]}


def _json_of(text):
    m = re.search(r"\{.*\}", text, re.S)
    if not m:
        raise ValueError(f"the judge answered no JSON: {text[:200]!r}")
    return json.loads(m.group(0))


def evaluate(data: bytes, page: int, markdown: str, model) -> dict:
    if model is None:
        raise ValueError("an evaluation needs a model")
    t0 = time.perf_counter()
    parts = _media.vision_parts(data, page)
    parts.append({"type": "text", "text": f"[extraction to judge]\n{markdown}"})
    answer = model.chat([{"role": "system", "content": JUDGE_PROMPT},
                         {"role": "user", "content": parts}], max_tokens=4096)
    got = _json_of(_content(answer))
    scores = {k: got.get("scores", {}).get(k) for k in CRITERIA}
    tin, tout = _usage(answer)
    return {"page": page, "scores": json.dumps(scores), "notes": str(got.get("notes") or ""),
            "ms": round((time.perf_counter() - t0) * 1000),
            "tokens_in": tin, "tokens_out": tout}


def _entities(data, n, cfg, model):
    if model is None:
        raise ValueError("the entities strategy needs a model")
    t0 = time.perf_counter()
    mode, languages = cfg.get("mode") or "AUTO", cfg.get("languages") or None
    blocks = []
    with _media._Doc(data) as d:
        for page in range(min(n, int(cfg.get("max_pages") or ENTITY_PAGES))):
            blocks += [_media._public(b) for b in _media._page_blocks(d, page, mode, languages,
                                                                      None)]
    r = extract_entities(blocks, cfg.get("entities") or {}, model, cfg.get("prompt"),
                         max_tokens=int(cfg.get("max_tokens") or 4000))
    size = _media.page_dimensions(data)[0]
    return {"page": 0, "page_count": n, "markdown": None,
            "entities": json.dumps(r["entities"], ensure_ascii=False),
            "blocks": json.dumps(blocks, ensure_ascii=False), "reasked": r["reasked"],
            "ms": round((time.perf_counter() - t0) * 1000), "tokens_in": r["tokens_in"],
            "tokens_out": r["tokens_out"], "page_width": size["width"],
            "page_height": size["height"]}


def run(doc, model, cfg_json: str) -> dict:
    cfg = json.loads(cfg_json)
    data, page = doc.read(), int(cfg.get("page") or 0)
    n = _media.page_count(data)
    if not 0 <= page < n:
        raise ValueError(f"page {page + 1} — the document has {n} page(s)")
    if cfg.get("op") == "evaluate":
        return evaluate(data, page, cfg.get("markdown") or "", model)
    if cfg.get("strategy") == "entities":
        return _entities(data, n, cfg, model)
    return extract(data, page, cfg, model)

FINGERPRINTED = ("strategy", "mode", "languages", "model", "preprocess", "prompt",
                 "max_tokens")
EMPTY_PAGE = dict.fromkeys(
    ("extraction_result", "layout_info", "error", "tokens_in", "tokens_out", "ms",
     "page_width", "page_height"))


def strategy_id(cfg: dict) -> str:
    kept = {k: cfg[k] for k in FINGERPRINTED if cfg.get(k) not in (None, "", [])}
    canonical = json.dumps(kept, sort_keys=True, ensure_ascii=False)
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()[:12]


def _page_row(ref, page, sid, **values):
    return {"page_id": f"{ref}#{page}", "media_item_rid": ref, "media_reference": ref,
            "page_num": page,
            "strategy_id": sid, **EMPTY_PAGE, **values}


def _extract_document(media, ref, cfg, model, sid, max_pages):
    try:
        data = media.bytes(ref)
        n = _media.page_count(data)
    except _media.MediaToolMissing:
        raise
    except Exception as e:
        return [_page_row(ref, 0, sid, error=f"{type(e).__name__}: {e}")]
    rows = []
    for page in range(min(n, max_pages or n)):
        try:
            r = extract(data, page, cfg, model)
            rows.append(_page_row(ref, page, sid, extraction_result=r["markdown"],
                                  layout_info=r["blocks"], tokens_in=r["tokens_in"],
                                  tokens_out=r["tokens_out"], ms=r["ms"],
                                  page_width=r["page_width"], page_height=r["page_height"]))
        except (_media.MediaToolMissing, LanguageModelBudgetWait):
            raise
        except Exception as e:
            rows.append(_page_row(ref, page, sid, error=f"{type(e).__name__}: {e}"))
    return rows


def create_extraction(media, strategy: dict, model=None, previous=None,
                      filter_on_media_items=None, max_pages=None, thread_number=8) -> list:
    if strategy.get("strategy") == "vlm" and model is None:
        raise ValueError("the vlm strategy needs a model")
    sid = strategy_id(strategy)
    refs = media.refs()
    if filter_on_media_items is not None:
        wanted = set(filter_on_media_items)
        refs = [r for r in refs if r in wanted]
    if not refs:
        raise ValueError("no media items to extract: a replace write would delete every page")
    before = {}
    for row in previous or []:
        before.setdefault(row.get("media_reference"), []).append(dict(row))
    fresh, failed, carried = [], [], []
    for ref in refs:
        pages = before.get(ref, [])
        if not pages or any(p.get("strategy_id") != sid for p in pages):
            fresh.append(ref)
        elif any(p.get("error") for p in pages):
            failed.append(ref)
        else:
            carried += pages
    todo = cap_batch(fresh + failed)
    waiting = set(fresh + failed) - set(todo)
    carried += [p for r in waiting for p in before.get(r, [])]
    run = lambda ref: _extract_document(media, ref, strategy, model, sid, max_pages)
    done = (parallel_map(run, todo, max_workers=thread_number) if model is not None
            else cpu_map(run, todo))
    return carried + [row for rows in done for row in rows]

class _Extractor:

    def __init__(self, spec, model):
        self._spec = spec
        self._model = model

    def _run(self, media, cfg, thread_number, filter_on_media_items, max_pages):
        from sutra_transforms import previous_output
        cfg = {"strategy": "vlm", "model": self._spec.model,
               **{k: v for k, v in cfg.items() if v}}
        return create_extraction(media, cfg, self._model, previous=previous_output(),
                                 filter_on_media_items=filter_on_media_items,
                                 max_pages=max_pages, thread_number=thread_number)


class _PageExtractor(_Extractor):
    def create_extraction(self, media, with_ocr=False, prompt=None, languages=None,
                          thread_number=20, filter_on_media_items=None, max_pages=None):
        return self._run(media, {"preprocess": "ocr" if with_ocr else None, "prompt": prompt,
                                 "languages": languages},
                         thread_number, filter_on_media_items, max_pages)


class _LayoutExtractor(_Extractor):
    def create_extraction(self, media, include_layout_info="no_overlay", system_prompt=None,
                          languages=None, thread_number=20, filter_on_media_items=None,
                          max_pages=None):
        if include_layout_info not in LAYOUT_INFO:
            raise ValueError(f"include_layout_info {include_layout_info!r} — "
                             f"one of {sorted(LAYOUT_INFO)}")
        return self._run(media, {"preprocess": LAYOUT_INFO[include_layout_info],
                                 "prompt": system_prompt, "languages": languages},
                         thread_number, filter_on_media_items, max_pages)


class _ExtractorInput(LanguageModel):

    extractor = None

    def __init__(self, model, **kw):
        super().__init__(model=model, **kw)

    def bind(self, model):
        return self.extractor(self, model)


class VisionLLMDocumentsExtractorInput(_ExtractorInput):
    extractor = _PageExtractor


class VisionLLMLayoutDocumentsExtractorInput(_ExtractorInput):
    extractor = _LayoutExtractor
