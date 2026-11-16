
import base64
import hashlib
import inspect
import io
import itertools
import json
import os
import shutil
import sys
import tempfile
import threading
import time
from typing import Any, Dict, Optional


_TRACE: list = []
_TRACE_ON = bool(os.environ.get('SUTRA_TRACE'))
_PK: Optional[str] = None
_MORE_REMAINING = False
_MAX_PER_RUN: Optional[int] = None
_PENDING_OBSERVED: Optional[int] = None
_PROMPT_KEY: Optional[Dict[str, Any]] = None


def _trace(event: str, **fields) -> None:
    if _TRACE_ON:
        _TRACE.append({'event': event, **fields})


def _fingerprint(rows, key=None, *, samples=3, max_len=64):
    rows = rows or []
    cols, seen = [], set()
    for r in rows:
        for c in r:
            if c not in seen:
                seen.add(c)
                cols.append(c)
    null_cols = []
    for c in cols:
        n = sum(1 for r in rows if r.get(c) is None)
        if n:
            null_cols.append([c, n])
    fp = {'rows': len(rows), 'cols': cols, 'null_cols': null_cols}
    if key is not None:
        def peek(r):
            try:
                return r.get(key)
            except KeyError:
                return dict.get(r, key.replace('-', '_'))
        sample = []
        for r in rows:
            v = peek(r)
            if v is None:
                continue
            s = str(v)
            if len(s) <= max_len and s not in sample:
                sample.append(s)
            if len(sample) >= samples:
                break
        fp['key'] = key
        fp['key_nulls'] = sum(1 for r in rows if peek(r) is None)
        fp['key_sample'] = sample
    return fp


def trace(event: str, **fields) -> None:
    _trace(event, **fields)


_T0 = time.monotonic()
_SPAN_IDS = itertools.count(1)
_SPAN_LOCK = threading.Lock()


def _span_line(kind: str, **fields) -> None:
    line = json.dumps({'sutra_span': kind, **fields}, default=str)
    with _SPAN_LOCK:
        sys.stderr.write(line + '\n')
        sys.stderr.flush()


class span:

    def __init__(self, name: str, **fields):
        self.name = name
        self.fields = fields

    def __enter__(self):
        self.id = next(_SPAN_IDS)
        self.t = time.monotonic()
        _span_line('open', id=self.id, name=self.name,
                   at_ms=round((self.t - _T0) * 1000), **self.fields)
        return self.fields

    def __exit__(self, et, e, tb):
        extra = {'failed': str(e)[:300]} if e is not None else {}
        _span_line('close', id=self.id, name=self.name,
                   ms=round((time.monotonic() - self.t) * 1000), **self.fields, **extra)
        return False


def intermediate(name: str, value):
    line = json.dumps({'sutra_intermediate': str(name), 'value': value}, default=str)
    with _SPAN_LOCK:
        sys.stderr.write(line + '\n')
        sys.stderr.flush()
    return value


def get_secret(source: str, field: Optional[str] = None):
    path = os.environ.get('SUTRA_SECRETS')
    if not path:
        raise RuntimeError(
            "no secrets available — this transform did not declare "
            ":external-systems (or is not running as an external transform)")
    with open(path) as fh:
        secrets = json.load(fh)
    if source not in secrets:
        raise KeyError(
            f"source {source!r} was not granted to this transform "
            "(declare it in :external-systems)")
    cred = secrets[source]
    return cred[field] if field is not None else cred


class SourceResponse:

    def __init__(self, url, status_code, headers, content=None, raw=None):
        self.url = url
        self.status_code = status_code
        self.headers = headers
        self.raw = raw
        self._content = content

    @property
    def content(self):
        if self._content is None:
            self._content = self.raw.read() if self.raw is not None else b''
            self.close()
        return self._content

    def iter_content(self, chunk_size=65536):
        if self._content is not None or self.raw is None:
            data = self.content
            for i in range(0, len(data), chunk_size):
                yield data[i:i + chunk_size]
            return
        while True:
            chunk = self.raw.read(chunk_size)
            if not chunk:
                break
            yield chunk
        self.close()

    def close(self):
        if self.raw is not None:
            self.raw.close()

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.close()
        return False

    @property
    def ok(self):
        return 200 <= self.status_code < 400

    @property
    def text(self):
        return self.content.decode('utf-8', errors='replace')

    def json(self):
        return json.loads(self.content)

    def raise_for_status(self):
        if not self.ok:
            raise SourceHttpError(self)
        return self


class SourceHttpError(Exception):
    def __init__(self, response):
        super().__init__(f"HTTP {response.status_code} from {response.url}: {response.text[:500]}")
        self.response = response


def _host_port(url):
    from urllib.parse import urlsplit
    u = urlsplit(url)
    return f"{u.hostname}:{u.port or (443 if u.scheme == 'https' else 80)}"


def _json_dumps(v):
    return json.dumps(v, default=str)


class SourceClient:

    def __init__(self, source):
        self._source = source

    def request(self, method, url, params=None, json=None, data=None, headers=None, timeout=30,
                stream=False):
        import urllib.error
        import urllib.parse
        import urllib.request
        if url.startswith('/'):
            url = self._source.url + url
        egress = self._source.egress
        if egress and _host_port(url) not in egress:
            raise PermissionError(
                f"{_host_port(url)} is outside source {self._source.name!r}'s egress "
                f"allowlist {sorted(egress)}")
        if params:
            url = url + ('&' if '?' in url else '?') + urllib.parse.urlencode(params, doseq=True)
        hdrs = dict(headers or {})
        body = data
        if json is not None:
            body = _json_dumps(json).encode('utf-8')
            hdrs.setdefault('Content-Type', 'application/json')
        elif isinstance(data, dict):
            body = urllib.parse.urlencode(data).encode('utf-8')
            hdrs.setdefault('Content-Type', 'application/x-www-form-urlencoded')
        elif isinstance(data, str):
            body = data.encode('utf-8')
        req = urllib.request.Request(url, data=body, headers=hdrs, method=method.upper())
        try:
            if stream:
                r = urllib.request.urlopen(req, timeout=timeout)
                return SourceResponse(url, r.status, dict(r.headers), raw=r)
            with urllib.request.urlopen(req, timeout=timeout) as r:
                return SourceResponse(url, r.status, dict(r.headers), r.read())
        except urllib.error.HTTPError as e:
            return SourceResponse(url, e.code, dict(e.headers or {}), e.read() or b'')

    def get(self, url, **kw):
        return self.request('GET', url, **kw)

    def post(self, url, **kw):
        return self.request('POST', url, **kw)

    def put(self, url, **kw):
        return self.request('PUT', url, **kw)

    def patch(self, url, **kw):
        return self.request('PATCH', url, **kw)

    def delete(self, url, **kw):
        return self.request('DELETE', url, **kw)


class HttpsConnection:
    def __init__(self, source):
        self._source = source
        self.url = source.url

    def get_client(self):
        return SourceClient(self._source)


class Source:
    def __init__(self, name, fields):
        self.name = name
        self._fields = fields
        self.url = str(fields.get('base-url') or fields.get('url') or '').rstrip('/')
        self.egress = set(fields.get('egress') or [])

    def get_secret(self, key):
        for k in (key, key.lower(), key.replace('_', '-'), key.replace('-', '_')):
            if k in self._fields:
                return self._fields[k]
        raise KeyError(f"source {self.name!r} has no secret {key!r}")

    def get_https_connection(self):
        if not self.url:
            raise ValueError(f"source {self.name!r} declares no base-url")
        return HttpsConnection(self)

    def get_client(self):
        return self.get_https_connection().get_client()

    def __repr__(self):
        return f"Source({self.name!r}, url={self.url!r})"


def get_source(name: str) -> Source:
    return Source(name, get_secret(name))


_MEDIA_ITEMS: Dict[str, Dict[str, Any]] = {}
_MEDIA_SET_ITEMS: list = []


def note_media_set_item(item: Dict[str, Any]) -> None:
    _MEDIA_SET_ITEMS.append(item)


def media_item_size(address: str) -> int:
    return _MEDIA_ITEMS[address]["size"]


def drain(name: str):
    global _MORE_REMAINING, _PENDING_OBSERVED
    if name == 'trace':
        events, _TRACE[:] = list(_TRACE), []
        return events
    if name == 'media':
        items = list(_MEDIA_ITEMS.values())
        _MEDIA_ITEMS.clear()
        return items
    if name == 'media_set_items':
        items = list(_MEDIA_SET_ITEMS)
        _MEDIA_SET_ITEMS.clear()
        return items
    if name == 'more_remaining':
        v, _MORE_REMAINING = bool(_MORE_REMAINING), False
        return v
    if name == 'pending_observed':
        v, _PENDING_OBSERVED = _PENDING_OBSERVED, None
        return v
    raise KeyError("unknown run-state channel %r" % (name,))


_SPOOL_ABOVE = 1024 * 1024


def media_spool() -> Optional[str]:
    return os.environ.get("SUTRA_MEDIA_SPOOL") or None


class SpoolFile(io.RawIOBase):

    def __init__(self, mime: str, on_close=None):
        super().__init__()
        self._mime = mime
        self._on_close = on_close
        self._hash = hashlib.sha256()
        self._size = 0
        self._failed = False
        spool = media_spool()
        if spool:
            fd, self._path = tempfile.mkstemp(prefix="m-", suffix=".bin", dir=spool)
            self._out = os.fdopen(fd, "wb")
        else:
            self._path, self._out = None, io.BytesIO()
        self.address = None

    def writable(self):
        return True

    def write(self, b):
        if isinstance(b, str):
            b = b.encode("utf-8")
        mv = memoryview(b).cast("B")
        self._hash.update(mv)
        self._out.write(mv)
        self._size += mv.nbytes
        return mv.nbytes

    def close(self):
        if self.closed:
            return
        try:
            if self._failed:
                self._discard()
            else:
                self.address = self._land()
                if self._on_close:
                    self._on_close(self.address)
        finally:
            super().close()

    def _discard(self):
        if self._path:
            self._out.close()
            os.unlink(self._path)

    def _land(self) -> str:
        h = self._hash.hexdigest()
        item = {"hash": h, "mime": self._mime, "size": self._size}
        if self._path:
            self._out.close()
            item["spool"] = os.path.basename(self._path)
        else:
            item["b64"] = base64.b64encode(self._out.getvalue()).decode("ascii")
        old = _MEDIA_ITEMS.get(h)
        if old and old.get("spool") and self._path:
            os.unlink(self._path)
        else:
            _MEDIA_ITEMS[h] = item
        return h

    def __exit__(self, kind, value, tb):
        self._failed = kind is not None
        return super().__exit__(kind, value, tb)


def put_media_item(data, mime: str = "application/octet-stream") -> str:
    if isinstance(data, str):
        data = data.encode("utf-8")
    if not hasattr(data, "read") and (len(data) <= _SPOOL_ABOVE or not media_spool()):
        b = bytes(data)
        h = hashlib.sha256(b).hexdigest()
        _MEDIA_ITEMS[h] = {"hash": h, "mime": mime, "size": len(b),
                           "b64": base64.b64encode(b).decode("ascii")}
        return h
    with SpoolFile(mime) as out:
        if hasattr(data, "read"):
            shutil.copyfileobj(data, out, 1024 * 1024)
        else:
            out.write(data)
    return out.address


def refuse_generator(f, decorator: str) -> None:
    if inspect.isgeneratorfunction(f) or inspect.isasyncgenfunction(f):
        raise TypeError(
            "%s: %s yields. A Function returns its value once; a transform writes its "
            "outputs once per build, as one transaction. Neither hands back pieces and "
            "resumes later: return the whole list (work left for a later build: "
            "cap_batch / max_per_run)." % (decorator, f.__name__))


def defer_remaining() -> None:
    global _MORE_REMAINING
    _MORE_REMAINING = True


def cap_batch(items, n: Optional[int] = None):
    if n is None:
        n = _MAX_PER_RUN
        if n is None:
            raise ValueError(
                "cap_batch(items) needs a cap: declare @transform(max_per_run=N) "
                "(preferred — the platform's cost estimate reads it) or pass n.")
    items = list(items)
    global _PENDING_OBSERVED
    _PENDING_OBSERVED = len(items)
    if len(items) > n:
        if os.environ.get('SUTRA_REFUSE_CAP'):
            raise ValueError(
                "cap_batch: %d items over a cap of %d in a run that replaces its output, because "
                "an input overwrote or removed rows. Declare that input in static_inputs."
                % (len(items), n))
        global _MORE_REMAINING
        _MORE_REMAINING = True
    return items[:n]



def _md5(s: str) -> str:
    return hashlib.md5(s.encode('utf-8')).hexdigest()


def _hash_parts(parts) -> str:
    return _md5(''.join(_md5('' if p is None else str(p)) for p in parts))


def prompt_hash(row: Dict[str, Any]) -> str:
    if not _PROMPT_KEY:
        raise ValueError(
            "prompt_hash() needs @transform(prompt_key=['<input>.<column>', …]) "
            "— without it there is nothing to hash and every row would collide.")
    cols = _PROMPT_KEY.get('columns') or []
    missing = [c for c in cols if c not in row]
    if missing:
        raise KeyError(
            "prompt_key column(s) %s absent from the input row (has %s)"
            % (missing, sorted(row)[:12]))
    return _hash_parts([_PROMPT_KEY.get('version', 1)] + [row[c] for c in cols])


def unprocessed(rows, previous):
    if not _PROMPT_KEY:
        raise ValueError(
            "unprocessed() needs @transform(prompt_key=['<input>.<column>', …])")
    col = _PROMPT_KEY.get('column') or 'prompt_hash'
    done = {r[col] for r in (previous or []) if r.get(col) and not _errored(r)}
    return [r for r in rows if prompt_hash(r) not in done]


_ERR_PREFIX = "_completion_error-"
_ERR_PREFIXES = (_ERR_PREFIX, "_completion_error_")


def _errored(row):
    for k, v in row.items():
        prefix = next((p for p in _ERR_PREFIXES if k.startswith(p)), None)
        if prefix is None:
            continue
        if v is not None:
            return True
        value_key = k[len(prefix):]
        if value_key in row and row[value_key] is None:
            return True
    return False


def cpu_cores() -> int:
    try:
        return max(1, len(os.sched_getaffinity(0)))
    except (AttributeError, OSError):
        return max(1, int(os.environ.get("SUTRA_CPU_CORES") or 1))


def _cpu_child(fn, items, k, n, w):
    import pickle
    try:
        out = [(i, True, fn(items[i])) for i in range(k, len(items), n)]
    except BaseException as e:   # noqa: BLE001 — carried to the parent, raised there
        out = [(-1, False, f"{type(e).__name__}: {e}")]
    with os.fdopen(w, "wb") as fh:
        pickle.dump(out, fh, protocol=pickle.HIGHEST_PROTOCOL)


def cpu_map(fn, items, cores: Optional[int] = None):
    import pickle
    items = list(items)
    n = min(cores or cpu_cores(), len(items))
    if n <= 1:
        return [fn(x) for x in items]
    sys.stdout.flush()
    sys.stderr.flush()
    kids = []
    for k in range(n):
        r, w = os.pipe()
        pid = os.fork()
        if pid == 0:
            os.close(r)
            code = 0
            try:
                _cpu_child(fn, items, k, n, w)
            except BaseException:   # noqa: BLE001
                code = 1
            finally:
                os._exit(code)
        os.close(w)
        kids.append((pid, r))
    out, failure = [None] * len(items), None
    for pid, r in kids:
        with os.fdopen(r, "rb") as fh:
            try:
                part = pickle.load(fh)
            except EOFError:
                part = [(-1, False, "worker process died")]
        os.waitpid(pid, 0)
        for i, ok, v in part:
            if ok:
                out[i] = v
            elif failure is None:
                failure = v
    if failure is not None:
        raise RuntimeError(f"cpu_map: {failure}")
    return out
