
import csv
import decimal
import difflib
import fnmatch
import inspect
import io
import json
import math
import mimetypes
import os
import sys
import traceback
from dataclasses import asdict, dataclass, field
from datetime import timedelta
from typing import Any, Callable, Dict, Optional, Union

from sutra_runtime import (
    _fingerprint, _trace, cap_batch, cpu_cores, cpu_map, get_secret, get_source, prompt_hash,
    put_media_item, trace, unprocessed,
)
from sutra_llm import (
    ChatModel, LanguageModel, LanguageModelError,
    completion_column, error_column_for, parallel_map, tool_args, LanguageModelBudgetWait,
)

import sutra_media as _media
import sutra_runtime as _rt

if os.environ.get('SUTRA_CONTAINER'):
    for _k in ('PYTHONHOME', 'PYTHONPATH'):
        os.environ.pop(_k, None)



_DECIMAL_TAG = '~f'


def _from_wire(v):
    if isinstance(v, str):
        if v.startswith(_DECIMAL_TAG):
            return decimal.Decimal(v[len(_DECIMAL_TAG):])
        if v.startswith('~'):
            return v[1:]
        return v
    if isinstance(v, list):
        return [_from_wire(x) for x in v]
    if isinstance(v, dict):
        return {k: _from_wire(x) for k, x in v.items()}
    return v


def _to_wire(v):
    if isinstance(v, decimal.Decimal):
        return _DECIMAL_TAG + format(v, 'f')
    if isinstance(v, str):
        return '~' + v if v.startswith('~') else v
    if isinstance(v, list):
        return [_to_wire(x) for x in v]
    if isinstance(v, dict):
        return {k: _to_wire(x) for k, x in v.items()}
    return v


def _to_wire_rows(rows: list) -> list:
    for i, r in enumerate(rows):
        if isinstance(r, dict):
            for k, v in r.items():
                w = _to_wire(v)
                if w is not v:
                    r[k] = w
        else:
            w = _to_wire(r)
            if w is not r:
                rows[i] = w
    return rows



class _Row(dict):
    __slots__ = ()
    _aliases: Dict[str, str] = {}
    _derived: frozenset = frozenset()
    _input: str = ''

    def _lookup(self, k):
        a = self._aliases.get(k) if isinstance(k, str) else None
        if a is not None and dict.__contains__(self, a):
            return True, dict.__getitem__(self, a)
        return False, None

    def _near(self, k):
        if not isinstance(k, str):
            return None
        hits = difflib.get_close_matches(k, [c for c in self if isinstance(c, str)],
                                         n=1, cutoff=0.8)
        return hits[0] if hits else None

    def _derived_refusal(self, k):
        return KeyError(
            f"input {self._input!r} has no column {k!r}: it is a derived property, and an input "
            "holds stored properties only. Compute it here from the linked inputs, or read it "
            "in a Function.")

    def _refusal(self, k, near):
        return KeyError(
            f"input {self._input!r} has no column {k!r} — meinst du {near!r}? "
            "A stored column is named by the platform's fold (`-` → `_`, "
            "umlauts spelled out), which is the name `dataset preview` shows.")

    def __missing__(self, k):
        hit, v = self._lookup(k)
        if hit:
            return v
        if k in self._derived:
            raise self._derived_refusal(k)
        near = self._near(k)
        if near is not None:
            raise self._refusal(k, near)
        raise KeyError(k)

    def get(self, k, default=None):
        if dict.__contains__(self, k):
            return dict.__getitem__(self, k)
        hit, v = self._lookup(k)
        if hit:
            return v
        if k in self._derived:
            raise self._derived_refusal(k)
        near = self._near(k)
        if near is not None:
            raise self._refusal(k, near)
        return default

    def __contains__(self, k):
        return dict.__contains__(self, k) or self._lookup(k)[0]

    def __reduce__(self):
        return (dict, (dict(self),))


class Rows(list):

    def arrow(self):
        import pyarrow as pa
        return pa.Table.from_pylist([dict(r) if isinstance(r, dict) else {'value': r}
                                     for r in self])

    def pandas(self):
        import pandas as pd
        return pd.DataFrame.from_records([dict(r) if isinstance(r, dict) else {'value': r}
                                          for r in self])


def _input_rows(rows, name: str, aliases: Optional[Dict[str, str]], derived=None):
    gone = frozenset(derived or ())
    cls = type('Row', (_Row,), {'__slots__': (), '_aliases': dict(aliases or {}),
                                '_derived': gone, '_input': name})
    return Rows(cls((k, _from_wire(v)) for k, v in r.items() if k not in gone)
                if isinstance(r, dict) else _from_wire(r)
                for r in rows)


def _table_rows(t):
    import sutra_interchange as _ix
    if hasattr(t, 'to_pylist') and hasattr(t, 'schema'):
        return [{k: _plain_out(v, _ix) for k, v in r.items()} for r in t.to_pylist()]
    if hasattr(t, 'to_dict') and hasattr(t, 'columns') and hasattr(t, 'dtypes'):
        return [{str(k): _plain_out(v, _ix) for k, v in r.items()}
                for r in t.to_dict('records')]
    return None


def _plain_out(v, ix):
    if isinstance(v, decimal.Decimal):
        return v
    return ix.plain(v)


class _Exp:

    __slots__ = ('_op', '_args', '_steps')

    def __init__(self, op, args=(), steps=()):
        self._op, self._args, self._steps = op, tuple(args), tuple(steps)

    def __getattr__(self, op):
        if op.startswith('_'):
            raise AttributeError(op)
        return lambda *args: _Exp(self._op, self._args, self._steps + ((op, args),))

    def otherwise(self, exp):
        if self._op != 'when' or len(self._args) != 2:
            raise ValueError("otherwise() follows E.when(when_exp, then_exp)")
        return _Exp('when', self._args + (exp,), self._steps)

    def _wire(self):
        return {'E': self._op, 'args': [_exp_arg(a) for a in self._args],
                'steps': [{'step': op, 'args': [_exp_arg(a) for a in args]}
                          for op, args in self._steps]}


def _exp_arg(a):
    import datetime as _dt
    if isinstance(a, _Exp):
        return a._wire()
    if isinstance(a, _dt.timedelta):
        return a.total_seconds()
    if isinstance(a, (_dt.datetime, _dt.date)):
        return a.isoformat()
    if isinstance(a, (list, tuple, set)):
        return [_exp_arg(x) for x in a]
    if isinstance(a, dict):
        return {str(k): _exp_arg(v) for k, v in a.items()}
    return a


class _Expectations:

    def col(self, name):
        return _Exp('col', (name,))

    def group_by(self, *cols):
        return _Exp('group_by', (list(cols),))

    def __getattr__(self, op):
        if op.startswith('_'):
            raise AttributeError(op)
        return lambda *args: _Exp(op, args)


E = expectations = _Expectations()


@dataclass
class Check:

    expectation: Any
    name: str
    on_error: str = 'FAIL'

    def __post_init__(self):
        if isinstance(self.expectation, _Exp):
            self.expectation = self.expectation._wire()
        if str(self.on_error).upper() not in ('FAIL', 'WARN'):
            raise ValueError(f"Check {self.name!r}: on_error is 'FAIL' or 'WARN'")


def _checks(c):
    if c is None:
        return []
    return [c] if isinstance(c, Check) else list(c)


def _names(x):
    if x is None:
        return None
    return [x] if isinstance(x, str) else [str(v) for v in x]


@dataclass
class Input:

    kind: str
    path: Optional[str] = None
    dir_path: Optional[str] = None
    transform: Optional[str] = None
    object_type: Optional[str] = None
    datasource: Optional[str] = None
    dataset: Optional[str] = None
    media_set: Optional[str] = None
    table: Optional[str] = None
    checks: Any = None
    stop_propagating: Any = None
    stop_requiring: Any = None

    def __post_init__(self):
        self.checks = _checks(self.checks)
        self.stop_propagating = _names(self.stop_propagating)
        self.stop_requiring = _names(self.stop_requiring)


class _Param:

    type = 'string'

    def __init__(self, default=None):
        self.default = default
        self.value = default

    def _coerce(self, v):
        return v

    def _bound(self, values, name):
        p = type(self)(self.default)
        p.value = None if values.get(name, self.default) is None else self._coerce(values.get(name, self.default))
        return p

    def __repr__(self):
        return f"{type(self).__name__}(value={self.value!r})"


class StringParam(_Param):
    type = 'string'

    def _coerce(self, v):
        return str(v)


class IntegerParam(_Param):
    type = 'number'

    def _coerce(self, v):
        return int(v)


class FloatParam(_Param):
    type = 'number'

    def _coerce(self, v):
        return float(v)


class BooleanParam(_Param):
    type = 'boolean'

    def _coerce(self, v):
        return v if isinstance(v, bool) else str(v).lower() == 'true'


@dataclass
class FileStatus:

    path: str
    size: int
    modified: float


class FileSystem:

    def __init__(self, root: str):
        self._root = os.path.realpath(root)

    def _resolve(self, path: str) -> str:
        full = os.path.realpath(os.path.join(self._root, path))
        if full != self._root and not full.startswith(self._root + os.sep):
            raise ValueError(f"path {path!r} escapes the input directory")
        return full

    def ls(self, glob: str = '*'):
        out = []
        for dirpath, _dirs, files in os.walk(self._root):
            for fn in files:
                full = os.path.join(dirpath, fn)
                rel = os.path.relpath(full, self._root)
                if fnmatch.fnmatch(rel, glob) or fnmatch.fnmatch(fn, glob):
                    st = os.stat(full)
                    out.append(FileStatus(rel, st.st_size, st.st_mtime))
        return out

    def open(self, path: str, mode: str = 'r'):
        if any(c in mode for c in ('w', 'a', 'x', '+')):
            raise ValueError("input FileSystem is read-only")
        return open(self._resolve(path), mode)






def is_incremental():
    return os.environ.get('SUTRA_INCREMENTAL_RUN') == '1'


def previous_output():
    path = os.environ.get('SUTRA_PREVIOUS_OUTPUT')
    if not path:
        _trace('previous_output', rows=0)
        return []
    with open(path) as fh:
        rows = _from_wire(json.load(fh))
    if _rt._TRACE_ON:
        _trace('previous_output', **_fingerprint(rows, _rt._PK))
    return rows


def output_enum(attr):
    raw = os.environ.get('SUTRA_OUTPUT_ENUMS')
    if not raw:
        return []
    return json.loads(raw).get(attr, [])




def commission_code(raw, seps=(" / ", " - ", "/", "-", " ")):
    s = (raw or "").strip().upper()
    if not s:
        return ""
    cut = len(s)
    for sep in seps:
        i = s.find(sep)
        if i != -1:
            cut = min(cut, i)
    return s[:cut].strip()




MEDIA_RETRY_WAITS = (1, 2, 4, 8, 16)


def _urlopen_retrying(req):
    import time
    import urllib.error
    import urllib.request
    for wait in MEDIA_RETRY_WAITS + (None,):
        try:
            return urllib.request.urlopen(req, timeout=120)
        except urllib.error.HTTPError as e:
            if e.code < 500 or wait is None:
                raise
        except OSError:
            if wait is None:
                raise
        time.sleep(wait)


class MediaSet:

    def __init__(self, rows, media_dir, media_url=None, media_grants=None):
        self._rows = rows or []
        self._dir = media_dir
        self._url = media_url
        self._grants = media_grants or {}

    def rows(self):
        return self._rows

    def __iter__(self):
        return iter(self._rows)

    def refs(self):
        if self._grants:
            return list(self._grants)
        return list(dict.fromkeys(r["media_ref"] for r in self._rows if r.get("media_ref")))

    def _path(self, ref: str) -> str:
        if not self._dir:
            raise ValueError("media-set input has no landed blob directory")
        return os.path.join(self._dir, ref + ".bin")

    def open(self, ref: str):
        if self._url:
            import urllib.request
            grant = self._grants.get(ref)
            if grant is None:
                raise KeyError("media item %s is not referenced by this input" % ref)
            req = urllib.request.Request(
                self._url.rstrip("/") + "/" + ref,
                headers={"Authorization": "Bearer " + grant})
            return _urlopen_retrying(req)
        return open(self._path(ref), "rb")

    def bytes(self, ref: str) -> bytes:
        with _rt.span('media', ref=ref) as s, self.open(ref) as f:
            data = f.read()
            s['bytes'] = len(data)
            return data

    def page_texts(self, ref: str, layout: bool = True, mode: str = "AUTO",
                   languages=None):
        data = self.bytes(ref)
        try:
            with _rt.span('media-text', ref=ref, mode=mode):
                return list(_media.extract_text(data, mode=mode, languages=languages,
                                                layout=layout))
        except _media.MediaToolMissing:
            raise
        except Exception as e:
            print("MediaSet.text: could not parse %s: %s" % (ref, e), file=sys.stderr)
            trace("media-error", ref=ref, extraction='text',
                  error="%s: %s" % (type(e).__name__, e))
            return None

    def text(self, ref: str, layout: bool = True, min_chars: int = 24,
             mode: str = "AUTO", languages=None):
        pages = self.page_texts(ref, layout=layout, mode=mode, languages=languages)
        doc = "\n\n".join(p for p in (pages or []) if p.strip()).strip()
        return doc if len(doc) >= min_chars else None

    def page_count(self, ref: str) -> int:
        return _media.page_count(self.bytes(ref))

    def item(self, ref: str) -> "Media":
        return Media(ref, self)

    def get_pdf_page_dimensions(self, ref: str):
        return _media.page_dimensions(self.bytes(ref))

    def extract_raw_text(self, ref: str, start_page=0, end_page=None):
        return _media.extract_raw_text(self.bytes(ref), start_page, end_page)

    def ocr(self, ref: str, languages=None, scripts=None, start_page=0, end_page=None,
            output_format="text"):
        return _media.ocr(self.bytes(ref), languages, scripts, start_page, end_page,
                          output_format)

    def extract_text(self, ref: str, mode="AUTO", format="TEXT", languages=None,
                     scripts=None, start_page=0, end_page=None):
        return _media.extract_text(self.bytes(ref), mode, format, languages, scripts,
                                   start_page, end_page)

    def extract_layout_aware_text(self, ref: str, mode="AUTO", languages=None,
                                  scripts=None, start_page=0, end_page=None,
                                  error_handling="FAIL"):
        data = self.bytes(ref)
        return _handled(error_handling, ref, 'layout-aware', lambda: (
            _media.extract_layout_aware_text(data, mode, languages, scripts,
                                             start_page, end_page)))

    def convert_document_to_images(self, ref: str, start_page=0, end_page=None,
                                   width=1024, height=1024, output_format="PNG"):
        return _media.convert_document_to_images(self.bytes(ref), start_page, end_page,
                                                 width, height, output_format)

    def llm_parts(self, ref: str, mode="AUTO", start_page=0, end_page=None, with_ocr=False,
                  languages=None, scripts=None):
        with _rt.span('media-llm-parts', ref=ref, mode=mode):
            return _media.llm_parts(self.bytes(ref), mode, start_page, end_page, with_ocr,
                                    languages, scripts)

    def vision_parts(self, ref: str, page: int, with_ocr=False, include_layout_info=None,
                     languages=None, scripts=None):
        return _media.vision_parts(self.bytes(ref), page, with_ocr, include_layout_info,
                                   languages, scripts)










def _null_on_error(ref, what, f):
    try:
        return f()
    except _media.MediaToolMissing:
        raise
    except Exception as e:
        error = "%s: %s" % (type(e).__name__, e)
        print("media %s: %s: %s" % (what, ref, error), file=sys.stderr)
        trace("media-error", ref=ref, extraction=what, error=error)
        return None


ERROR_HANDLING = {"FAIL": lambda ref, what, f: f(), "NULL": _null_on_error}


def _handled(error_handling, ref, what, f):
    if error_handling not in ERROR_HANDLING:
        raise ValueError("error_handling must be one of %s, got %r"
                         % (sorted(ERROR_HANDLING), error_handling))
    return ERROR_HANDLING[error_handling](ref, what, f)


class Media:

    def __init__(self, ref: str, source: "MediaSet"):
        self.reference = ref
        self._source = source

    def __str__(self):
        return self.reference

    def open(self):
        return self._source.open(self.reference)

    def read(self) -> bytes:
        return self._source.bytes(self.reference)

    def text(self, **kw):
        return self._source.text(self.reference, **kw)

    def page_texts(self, **kw):
        return self._source.page_texts(self.reference, **kw)

    def page_count(self) -> int:
        return self._source.page_count(self.reference)

    def extract_raw_text(self, start_page=0, end_page=None):
        return self._source.extract_raw_text(self.reference, start_page, end_page)

    def ocr(self, **kw):
        return self._source.ocr(self.reference, **kw)

    def llm_parts(self, **kw):
        return self._source.llm_parts(self.reference, **kw)

@dataclass
class Output:

    kind: str
    object_type: Optional[str] = None
    link_type: Optional[str] = None
    dataset: Optional[str] = None
    primary_key: Optional[str] = None
    column_map: Dict[str, str] = field(default_factory=dict)
    checks: Any = None

    def __post_init__(self):
        self.checks = _checks(self.checks)


def MediaSetInput(name: Optional[str] = None, media_set: Optional[str] = None) -> Input:
    if media_set is not None:
        return Input(kind="media-set-items", media_set=media_set)
    return Input(kind="media-set", dataset=name)


@dataclass
class MediaSetOutput:
    media_set: str


class MediaSetWriter:

    def __init__(self, alias: str):
        self._alias = alias

    def put_media_item(self, data, path: str, mime: Optional[str] = None) -> str:
        mime = self._mime(path, mime)
        address = put_media_item(data, mime)
        self._note(path, address, mime)
        return address

    def open(self, path: str, mode: str = "wb", mime: Optional[str] = None):
        if mode not in ("wb", "w"):
            raise ValueError(f"media set item {path!r}: open for writing ('wb' or 'w'), "
                             f"got {mode!r}")
        mime = self._mime(path, mime)
        raw = _rt.SpoolFile(mime, lambda address: self._note(path, address, mime))
        if mode == "wb":
            return raw
        return io.TextIOWrapper(io.BufferedWriter(raw), encoding="utf-8")

    def _mime(self, path, mime):
        return mime or mimetypes.guess_type(path)[0] or "application/octet-stream"

    def _note(self, path, address, mime):
        _rt.note_media_set_item({"output": self._alias, "path": path, "address": address,
                                 "mime": mime, "size": _rt.media_item_size(address)})

    def register_media_item(self, physical_path: str, path: Optional[str] = None) -> None:
        item = path or physical_path
        _rt.note_media_set_item({"output": self._alias, "path": item,
                                 "physical": physical_path,
                                 "mime": mimetypes.guess_type(item)[0]
                                 or "application/octet-stream"})


def transform(inputs: Dict[str, Union[Input, LanguageModel]],
              output: Optional[Output] = None,
              outputs: Optional[Dict[str, Output]] = None,
              media_outputs: Optional[Dict[str, MediaSetOutput]] = None,
              external_systems: Optional[list] = None,
              incremental: Union[bool, Dict[str, str], None] = None,
              semantic_version: Optional[int] = None,
              done_key: Optional[str] = None,
              max_per_run: Optional[int] = None,
              prompt_key: Optional[list] = None,
              prompt_hash_column: Optional[str] = None) -> Callable[[Callable], Callable]:

    if output is not None and outputs is not None:
        raise ValueError(
            "declare either output= (single) or outputs= (multi), not both")

    model_sources = [spec.source for spec in inputs.values()
                     if isinstance(spec, LanguageModel) and spec.source]

    def wrap(fn):
        _rt.refuse_generator(fn, "@transform")
        fn._sutra_inputs = {k: v for k, v in inputs.items() if not isinstance(v, _Param)}
        fn._sutra_parameters = {k: v for k, v in inputs.items() if isinstance(v, _Param)}
        fn._sutra_output = output
        fn._sutra_outputs = outputs
        fn._sutra_media_outputs = dict(media_outputs or {})
        fn._sutra_external_systems = list(
            dict.fromkeys(list(external_systems or []) + model_sources))
        fn._sutra_incremental = incremental
        fn._sutra_semantic_version = semantic_version
        fn._sutra_done_key = done_key
        fn._sutra_max_per_run = max_per_run
        fn._sutra_prompt_key = list(prompt_key) if prompt_key else None
        fn._sutra_prompt_hash_column = prompt_hash_column
        fn._sutra_container = None
        return fn

    return _TransformDecorator(wrap)


class _TransformDecorator:

    def __init__(self, wrap):
        self._wrap = wrap
        self._container = None

    def with_container(self, container_image: str, container_tag: str):
        for what, v in (('container_image', container_image), ('container_tag', container_tag)):
            if not (isinstance(v, str) and v.strip()):
                raise ValueError("with_container needs a non-empty %s" % what)
        if container_tag.strip() == 'latest':
            raise ValueError("with_container: the tag 'latest' names no fixed image, "
                             "push and name a version instead")
        self._container = {'image': container_image.strip(), 'tag': container_tag.strip()}
        return self

    def __call__(self, fn):
        fn = self._wrap(fn)
        fn._sutra_container = self._container
        return fn


def configure(allowed_run_duration: Optional[timedelta] = None) -> Callable[[Callable], Callable]:
    if allowed_run_duration is not None and not (
            isinstance(allowed_run_duration, timedelta)
            and allowed_run_duration.total_seconds() > 0):
        raise ValueError("allowed_run_duration must be a positive datetime.timedelta")

    def wrap(fn):
        if allowed_run_duration is not None:
            fn._sutra_allowed_run_seconds = math.ceil(allowed_run_duration.total_seconds())
        return fn

    return wrap


def _is_function(fn) -> bool:
    return bool(getattr(fn, '_sutra_function', False))


def _function_inputs(fn) -> Dict[str, Input]:
    if getattr(fn, '_sutra_edits', None):
        raise TypeError(
            "@function(edits=[...]) on %s makes it an EDIT function: declare it with "
            ":target-type and its :source, not as a query Function" % fn.__name__)
    params = inspect.signature(fn).parameters.values()
    return {p.name: Input(kind='scalar') for p in params
            if p.kind not in (p.VAR_POSITIONAL, p.VAR_KEYWORD)}


def _inputs_of(fn) -> Dict[str, Input]:
    return _function_inputs(fn) if _is_function(fn) else fn._sutra_inputs


def _find_transform(module) -> Callable:
    matches = [v for v in vars(module).values()
               if callable(v) and (hasattr(v, '_sutra_inputs') or _is_function(v))]
    entry = getattr(module, '__sutra_entry__', None)
    if entry:
        matches = [fn for fn in matches if fn.__name__ == entry]
        if not matches:
            raise RuntimeError(f"no decorated entry point named {entry!r} in this module")
    if not matches:
        own = [n for n, v in vars(module).items()
               if inspect.isfunction(v) and not n.startswith('_')
               and getattr(v, '__module__', None) == module.__name__]
        raise RuntimeError(
            "missing decorator: a transform is entered through "
            "`@transform(inputs={...}, output=...)`, a query Function through "
            "`from functions.api import function` and `@function(sources=[...])`; "
            "a named function alone is not an entry point"
            + (f" — undecorated candidate(s): {', '.join(own)}" if own else ""))
    if len(matches) > 1:
        raise RuntimeError(
            f"multiple decorated entry points found: "
            f"{[fn.__name__ for fn in matches]}; one per module"
        )
    return matches[0]


def _materialize_input(spec: Input, injected: Optional[Dict[str, Any]] = None,
                       name: str = '') -> Any:
    kind = (injected.get('kind') if injected else None) or spec.kind
    if kind == 'scalar':
        return _from_wire(injected.get('value')) if injected else None
    if kind == 'language-model':
        g = (lambda k: injected.get(k) if injected else None)
        s = (lambda k, d=None: getattr(spec, k, d))
        chat = ChatModel(
            source=g('source') or s('source'),
            model=g('model') or s('model'),
            base_url=g('base-url') or g('base_url') or s('base_url'),
            timeout=s('timeout', 120),
            max_tokens=s('max_tokens', 1024))
        return spec.bind(chat) if hasattr(spec, 'bind') else chat
    path = (injected.get('path') if injected else None) or spec.path
    if kind == 'csv':
        if not path:
            raise ValueError("csv input missing path")
        with open(path, newline='') as f:
            return list(csv.DictReader(f))
    if kind == 'file':
        if not path:
            raise ValueError("file input missing path")
        with open(path) as f:
            return f.read()
    if kind == 'media':
        inj = injected or {}
        source = MediaSet([], inj.get('media-dir'), inj.get('media-url'), inj.get('media-grants'))
        return source.item(inj.get('address'))
    if kind in ('media-set', 'media-set-items'):
        if not path:
            raise ValueError("media-set input missing path (runner export)")
        inj = injected or {}
        with open(path) as f:
            rows = _input_rows(json.load(f), name, inj.get('aliases'))
        return MediaSet(rows, inj.get('media-dir'),
                        inj.get('media-url'), inj.get('media-grants'))
    if kind in ('transform', 'object-type', 'datasource', 'dataset'):
        if not path:
            raise ValueError(f"{kind} input missing path (runner export)")
        with open(path) as f:
            return _input_rows(json.load(f), name,
                               injected.get('aliases') if injected else None,
                               injected.get('derived-names') if injected else None)
    if kind == 'files':
        dir_path = (injected.get('path') if injected else None) or spec.dir_path
        if not dir_path:
            raise ValueError("files input missing dir-path")
        return FileSystem(dir_path)
    raise NotImplementedError(
        f"input kind {kind!r} not yet materialized by the runner"
    )


def _introspect(module_path: str) -> None:
    mod = _load(module_path)
    fn = _find_transform(mod)
    if _is_function(fn):
        if hasattr(fn, '_sutra_allowed_run_seconds'):
            raise ValueError("@configure is for a @transform: a @function's limit is :timeout-ms")
        _emit({
            'entry': 'function',
            'inputs': {name: asdict(spec) for name, spec in _function_inputs(fn).items()},
            'external_systems': getattr(fn, '_sutra_sources', []),
            'api_name': getattr(fn, '_sutra_api_name', None),
            'function_name': fn.__name__,
        })
        return
    outputs = getattr(fn, '_sutra_outputs', None)
    _emit({
        'entry': 'transform',
        'inputs': {name: asdict(spec) for name, spec in fn._sutra_inputs.items()},
        'output': asdict(fn._sutra_output) if fn._sutra_output else None,
        'outputs': ({name: asdict(spec) for name, spec in outputs.items()}
                    if outputs else None),
        'output_order': (list(outputs) if outputs else None),
        'media_outputs': {alias: spec.media_set for alias, spec
                          in getattr(fn, '_sutra_media_outputs', {}).items()},
        'external_systems': getattr(fn, '_sutra_external_systems', []),
        'parameters': {name: {'type': p.type, 'default': p.default}
                       for name, p in getattr(fn, '_sutra_parameters', {}).items()},
        'incremental': getattr(fn, '_sutra_incremental', None),
        'semantic_version': getattr(fn, '_sutra_semantic_version', None),
        'done_key': getattr(fn, '_sutra_done_key', None),
        'max_per_run': getattr(fn, '_sutra_max_per_run', None),
        'allowed_run_seconds': getattr(fn, '_sutra_allowed_run_seconds', None),
        'prompt_key': getattr(fn, '_sutra_prompt_key', None),
        'prompt_hash_column': getattr(fn, '_sutra_prompt_hash_column', None),
        'container': getattr(fn, '_sutra_container', None),
        'function_name': fn.__name__,
    })


def _prompt_key_state(fn) -> Optional[Dict[str, Any]]:
    spec = getattr(fn, '_sutra_prompt_key', None)
    if not spec:
        return None
    aliases, cols = [], []
    for entry in spec:
        alias, _, col = str(entry).partition('.')
        if not alias or not col:
            raise ValueError(
                'prompt_key entries must be "<input-alias>.<column>", got %r' % (entry,))
        if alias not in aliases:
            aliases.append(alias)
        cols.append(col)
    if len(aliases) != 1:
        raise ValueError(
            "prompt_key columns must all come from ONE input, got %s" % (aliases,))
    return {'input': aliases[0], 'columns': cols,
            'column': getattr(fn, '_sutra_prompt_hash_column', None) or 'prompt_hash',
            'version': getattr(fn, '_sutra_semantic_version', None) or 1}


def _check_prompt_stamp(rows, state) -> None:
    if not (state and rows):
        return
    col = state['column']
    missing = sum(1 for r in rows if not r.get(col))
    if missing:
        raise ValueError(
            "%d of %d output row(s) carry no %r — a prompt-keyed transform must "
            "stamp prompt_hash(row) into every row it produces, or its done-filter "
            "matches nothing and every build re-asks every question."
            % (missing, len(rows), col))


def _run(module_path: str, manifest_path: Optional[str] = None) -> None:
    mod = _load(module_path)
    fn = _find_transform(mod)
    manifest = {}
    if manifest_path:
        with open(manifest_path) as f:
            manifest = json.load(f)
    declared = inspect.signature(fn).parameters if _is_function(fn) else {}
    kwargs = {name: _materialize_input(spec, manifest.get(name), name)
              for name, spec in _inputs_of(fn).items()
              if name in manifest or name not in declared
              or declared[name].default is inspect.Parameter.empty}
    values = json.loads(os.environ.get('SUTRA_PARAMETERS') or '{}')
    kwargs.update({name: p._bound(values, name)
                   for name, p in getattr(fn, '_sutra_parameters', {}).items()})
    kwargs.update({alias: MediaSetWriter(alias)
                   for alias in getattr(fn, '_sutra_media_outputs', {})})
    _rt._MAX_PER_RUN = getattr(fn, '_sutra_max_per_run', None)
    _rt._PROMPT_KEY = _prompt_key_state(fn)
    if _rt._TRACE_ON:
        out = getattr(fn, '_sutra_output', None)
        pk = getattr(out, 'primary_key', None) if out is not None else None
        _rt._PK = pk
        for name, val in kwargs.items():
            if isinstance(val, list):
                _rt._TRACE.append(
                    {'event': 'input', 'name': name, **_fingerprint(val, pk)})
    result = fn(**kwargs)
    kwargs.clear()
    declared_outputs = getattr(fn, '_sutra_outputs', None)
    if declared_outputs is not None:
        if not isinstance(result, dict):
            raise TypeError(
                "multi-output transform must return a dict of "
                f"{{output-name: [rows]}}, got {type(result).__name__}")
        want, got = set(declared_outputs), set(result)
        if want != got:
            raise ValueError(
                f"returned outputs {sorted(map(str, got))} "
                f"!= declared {sorted(map(str, want))}")
        payload = {'outputs': {name: _as_rows(rows, name)
                               for name, rows in result.items()}}
    else:
        payload = {'rows': _as_rows(result, 'function' if _is_function(fn) else 'transform')}
    payload['protocol_version'] = PROTOCOL_VERSION
    payload['media'] = _rt.drain('media')
    payload['media_set_items'] = _rt.drain('media_set_items')
    events = _rt.drain('trace')
    if events:
        payload['trace'] = events
    if _rt.drain('more_remaining'):
        payload['incremental-complete'] = False
    observed = _rt.drain('pending_observed')
    if observed is not None:
        payload['pending-observed'] = int(observed)
    if _rt._PROMPT_KEY:
        _check_prompt_stamp(
            payload['rows'] if declared_outputs is None
            else payload['outputs'].get(next(iter(declared_outputs)), []),
            _rt._PROMPT_KEY)
    if declared_outputs is None:
        payload['rows'] = _to_wire_rows(payload['rows'])
    else:
        for rows in payload['outputs'].values():
            _to_wire_rows(rows)
    _emit(payload)


_RESULT_CHANNEL = None


def _claim_result_channel():
    global _RESULT_CHANNEL
    sys.stdout.flush()
    _RESULT_CHANNEL = os.fdopen(os.dup(1), 'w', encoding='utf-8')
    os.dup2(2, 1)
    sys.stdout = sys.stderr


def _emit(payload) -> None:
    ch = _RESULT_CHANNEL or sys.stdout
    json.dump(payload, ch)
    ch.write('\n')
    ch.flush()


def _as_rows(rows, where: str) -> list:
    if rows is None:
        return []
    if inspect.isgenerator(rows) or inspect.isasyncgen(rows):
        raise TypeError(
            f"{where} returned a generator: its result is one value, written once — "
            "return a list, not pieces")
    if not isinstance(rows, list):
        tabled = _table_rows(rows)
        if tabled is not None:
            return tabled
        raise TypeError(
            f"{where} must return a list of dicts, a pyarrow.Table or a pandas.DataFrame, "
            f"got {type(rows).__name__}")
    return rows


def _load(module_path: str):
    import importlib.util
    here = os.path.dirname(os.path.abspath(module_path))
    if here not in sys.path:
        sys.path.insert(0, here)
    spec = importlib.util.spec_from_file_location("sutra_user_transform", module_path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _error_payload(exc, module_path):
    want = os.path.basename(module_path or '')
    frames, seen, cur = [], set(), exc
    while cur is not None and id(cur) not in seen:
        seen.add(id(cur))
        frames.extend(traceback.extract_tb(cur.__traceback__))
        cur = cur.__cause__ or cur.__context__
    pick = [f for f in frames if os.path.basename(f.filename) == want] or frames
    payload = {'error': str(exc), 'type': type(exc).__name__}
    if pick:
        f = pick[-1]
        payload['where'] = '%s:%s in %s' % (
            os.path.basename(f.filename), f.lineno, f.name)
        if f.line:
            payload['where'] += ' | ' + f.line.strip()
    payload['traceback'] = ''.join(
        traceback.format_exception(type(exc), exc, exc.__traceback__))
    return payload


PROTOCOL_VERSION = '1.1'


class ProtocolVersionMismatch(RuntimeError):
    pass


def _check_protocol(host_version):
    if host_version and host_version.split('.')[0] != PROTOCOL_VERSION.split('.')[0]:
        raise ProtocolVersionMismatch(
            "the host speaks transform runtime protocol %s, this runtime %s — "
            "the worker and the runtime come from different releases"
            % (host_version, PROTOCOL_VERSION))


def main(argv):
    if len(argv) < 3 or argv[1] not in ('--introspect', '--run'):
        sys.stderr.write(
            "usage: python -m sutra_transforms (--introspect|--run) "
            "<user-module.py> [--inputs <manifest.json>]\n"
        )
        sys.exit(2)
    mode, module_path = argv[1], argv[2]
    manifest_path = None
    rest = argv[3:]
    if rest:
        if len(rest) == 2 and rest[0] == '--inputs':
            manifest_path = rest[1]
        else:
            sys.stderr.write(
                "usage: python -m sutra_transforms (--introspect|--run) "
                "<user-module.py> [--inputs <manifest.json>]\n"
            )
            sys.exit(2)
    _claim_result_channel()
    import logging
    logging.basicConfig(level=logging.INFO, stream=sys.stderr,
                        format='%(levelname)s %(name)s: %(message)s')
    try:
        _check_protocol(os.environ.get('SUTRA_PROTOCOL_VERSION'))
        if mode == '--introspect':
            _introspect(module_path)
        else:
            _run(module_path, manifest_path)
    except Exception as e:
        json.dump(_error_payload(e, module_path), sys.stderr)
        sys.stderr.write('\n')
        sys.exit(1)


if __name__ == '__main__':
    main(sys.argv)
