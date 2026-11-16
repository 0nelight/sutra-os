from __future__ import annotations

import io
from typing import Any

import collections as _collections
import collections.abc as _abc
for _n in ("Mapping", "MutableMapping", "Hashable", "Iterable", "Sequence",
           "Set", "Callable", "MutableSequence", "MutableSet"):
    if not hasattr(_collections, _n):
        setattr(_collections, _n, getattr(_abc, _n))



def _repair_rolling_cache():
    try:
        from transit import rolling_cache as rc
    except ImportError:
        return

    def clear(self):
        self.key_to_value = {}
        self.value_to_key = {}

    rc.RollingCache.clear = clear
    rc.RollingCache.is_cache_full = (
        lambda self: len(self.key_to_value) >= rc.CACHE_SIZE)


_repair_rolling_cache()

from .errors import AuthError, NotFound, QueryError, ServerError
from .paths import encoding_for_path
from .predicates import OPS
from .refs import KwName


def _Keyword():
    from transit.transit_types import Keyword
    return Keyword


def _kw(name: str):
    return _Keyword()(name)


def _encode(obj) -> bytes:
    from transit.writer import Writer
    for make, post in ((io.StringIO, lambda s: s.encode("utf-8")),
                       (io.BytesIO, lambda b: b)):
        buf = make()
        try:
            Writer(buf, "json").write(obj)
        except TypeError:
            continue
        return post(buf.getvalue())
    raise RuntimeError("transit writer accepted neither text nor byte stream")


def _decode(b: bytes):
    from transit.reader import Reader
    s = b.decode("utf-8") if isinstance(b, (bytes, bytearray)) else b
    try:
        return Reader("json").read(io.StringIO(s))
    except TypeError:
        return Reader("json").read(io.BytesIO(s.encode("utf-8")))


def _kw_name(k) -> str:
    s = getattr(k, "name", None) or str(k)
    s = str(s)
    if s.startswith("~:"):
        return s[2:]
    return s[1:] if s.startswith(":") else s


def from_wire(x: Any):
    import uuid as _uuid
    from transit.transit_types import Boolean
    Keyword = _Keyword()
    if isinstance(x, Keyword):
        return _kw_name(x)
    if isinstance(x, Boolean):
        return bool(x)
    if isinstance(x, (str, bytes, bytearray)):
        return x
    if isinstance(x, _uuid.UUID):
        return str(x)
    items = getattr(x, "items", None)
    if callable(items):
        return {from_wire(k): from_wire(v) for k, v in items()}
    if hasattr(x, "_dict"):
        return {from_wire(k): from_wire(v) for k, v in x._dict.items()}
    if not isinstance(x, (int, float, bool)) and hasattr(x, "__iter__"):
        return [from_wire(v) for v in x]
    return x


def _enc_where(where: dict):
    out = {}
    for key, val in where.items():
        if isinstance(val, dict) and val and all(k in OPS for k in val):
            out[_kw(key)] = {_kw(op): v for op, v in val.items()}
        else:
            out[_kw(key)] = _enc_where(val)
    return out


def _enc_show(show: list):
    out = []
    for item in show:
        if isinstance(item, dict):
            out.append({_kw(conn): [_kw(a) for a in attrs] for conn, attrs in item.items()})
        else:
            out.append(_kw(item))
    return out


def _enc_around(around: list) -> list:
    for hop in around:
        if not isinstance(hop, str):
            raise TypeError(f"a hop names a link side by its API name, got {hop!r}")
    return [_kw(hop) for hop in around]


_QUERY_KEYWORD_KEYS = ("find", "order-by", "sort-dir", "accuracy", "engine")
_QUERY_PLAIN_KEYS = ("limit", "offset", "count?", "explain?", "ids",
                     "object-set", "object-set-args")


def encode_query(q: dict) -> bytes:
    w = {}
    for key in _QUERY_KEYWORD_KEYS:
        if q.get(key) is not None:
            w[_kw(key)] = _kw(q[key])
    for key in _QUERY_PLAIN_KEYS:
        if q.get(key) is not None:
            w[_kw(key)] = q[key]
    if q.get("where"):
        w[_kw("where")] = _enc_where(q["where"])
    if q.get("show"):
        w[_kw("show")] = _enc_show(q["show"])
    if q.get("around"):
        w[_kw("around")] = _enc_around(q["around"])
    if q.get("aggs") is not None:
        w[_kw("aggs")] = _enc_generic(q["aggs"])
    if q.get("group-by"):
        w[_kw("group-by")] = _enc_generic(q["group-by"])
    return _encode(w)


def _enc_action_args(args_in: dict) -> dict:
    args = {}
    for k, v in (args_in or {}).items():
        if k == "values" and isinstance(v, dict):
            args[_kw("values")] = {_kw(ak): av for ak, av in v.items()}
        elif k == "markings" and isinstance(v, (list, tuple)):
            args[_kw("markings")] = [_kw(m) for m in v]
        else:
            args[_kw(k)] = v
    return args


def encode_action(a: dict) -> bytes:
    return _encode({_kw("action"): _kw(a["action"]),
                    _kw("args"): _enc_action_args(a.get("args"))})


def encode_action_batch(a: dict) -> bytes:
    return _encode({_kw("action"): _kw(a["action"]),
                    _kw("requests"): [_enc_action_args(r) for r in a.get("requests") or []]})


def _enc_generic(x):
    if isinstance(x, KwName):
        return _kw(str(x))
    if isinstance(x, dict):
        return {(_kw(k) if isinstance(k, str) else k): _enc_generic(v) for k, v in x.items()}
    if isinstance(x, (list, tuple)):
        return [_enc_generic(v) for v in x]
    return x


def encode_generic(body) -> bytes:
    return _encode(_enc_generic(body))


def encode_function(body: dict) -> bytes:
    w = {_kw("function-name"): _kw(body["function-name"])}
    if body.get("inputs") is not None:
        w[_kw("inputs")] = _enc_generic(body["inputs"])
    return _encode(w)


ENCODERS = {
    "keywordize-query": encode_query,
    "keywordize-action": encode_action,
    "keywordize-action-batch": encode_action_batch,
    "keywordize-function": encode_function,
    "generic": encode_generic,
    "bytes": encode_generic,
}


class Http:
    CONTENT = "application/transit+json"

    def __init__(self, base_url: str, token: str | None = None, session=None):
        self.base = base_url.rstrip("/")
        self.token = token
        self.branch: str | None = None
        self._session = session

    def _url(self, path: str) -> str:
        if not self.branch:
            return self.base + path
        from urllib.parse import quote
        sep = "&" if "?" in path else "?"
        return f"{self.base}{path}{sep}branch={quote(self.branch, safe='')}"

    @property
    def session(self):
        if self._session is None:
            import requests
            self._session = requests.Session()
        return self._session

    def _headers(self) -> dict:
        h = {"Content-Type": self.CONTENT, "Accept": self.CONTENT}
        if self.token:
            h["Authorization"] = f"Bearer {self.token}"
        return h

    def _encoder(self, path: str):
        return ENCODERS[encoding_for_path(path)]

    @staticmethod
    def _raise(resp):
        if resp.status_code < 400:
            return
        try:
            body = from_wire(_decode(resp.content)) if resp.content else None
        except Exception:
            body = resp.text
        if resp.status_code == 401:
            raise AuthError(body)
        if resp.status_code == 404:
            raise NotFound(body)
        if resp.status_code == 400:
            raise QueryError(body)
        raise ServerError(resp.status_code, body)

    def post(self, path: str, body):
        data = self._encoder(path)(body)
        r = self.session.post(self._url(path), data=data, headers=self._headers())
        self._raise(r)
        return from_wire(_decode(r.content)) if r.content else None

    def get(self, path: str, params: dict | None = None):
        r = self.session.get(self._url(path), params=params, headers=self._headers())
        self._raise(r)
        return from_wire(_decode(r.content)) if r.content else None

    def delete(self, path: str, params: dict | None = None):
        r = self.session.delete(self._url(path), params=params, headers=self._headers())
        self._raise(r)
        return from_wire(_decode(r.content)) if r.content else None

    def post_bytes(self, path: str, data: bytes):
        headers = {"Content-Type": "application/octet-stream", "Accept": self.CONTENT}
        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"
        r = self.session.post(self._url(path), data=data, headers=headers)
        self._raise(r)
        return from_wire(_decode(r.content)) if r.content else None

    def get_stream(self, path: str, params: dict | None = None):
        r = self.session.get(self._url(path), params=params,
                             headers={"Authorization": f"Bearer {self.token}"} if self.token else {},
                             stream=True)
        self._raise(r)
        return r
