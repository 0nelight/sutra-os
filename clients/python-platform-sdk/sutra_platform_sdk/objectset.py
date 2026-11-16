from __future__ import annotations

from typing import Any, Generic, Iterator, TypeVar

from .aggregate import Agg, AggregateResult, GroupBy
from .errors import NotFound
from .objects import OntologyObject, hydrate
from .paths import url
from .predicates import Link, Predicate, Property, _deep_merge, where_to_ir
from .refs import kw

O = TypeVar("O", bound=OntologyObject)
T = TypeVar("T", bound=OntologyObject)

MAX_PAGE = 10000
MAX_HOPS = 3


class Page:

    __slots__ = ("objects", "offset", "limit", "total")

    def __init__(self, objects: list, offset: int, limit: int, total=None):
        self.objects = objects
        self.offset = offset
        self.limit = limit
        self.total = total

    @property
    def next_offset(self):
        if len(self.objects) < self.limit:
            return None
        nxt = self.offset + self.limit
        if self.total is not None and nxt >= self.total:
            return None
        return nxt

    def __iter__(self):
        return iter(self.objects)

    def __len__(self) -> int:
        return len(self.objects)

    def __getitem__(self, i):
        return self.objects[i]

    def __repr__(self) -> str:
        return (f"<Page {len(self.objects)} objects at {self.offset}"
                f"{f' of {self.total}' if self.total is not None else ''}>")


class ObjectSet(Generic[O]):
    def __init__(self, client, type_name: str, *, where=None, ids=None, around=None,
                 order_by=None, desc=False, limit=None, offset=None, show=None,
                 object_set=None, accuracy=None, engine=None, klass=None):
        self._client = client
        self._type = type_name
        self._where = where or {}
        self._ids = ids
        self._around = list(around or [])
        self._order_by = order_by
        self._desc = desc
        self._limit = limit
        self._offset = offset
        self._show = show
        self._object_set = object_set
        self._accuracy = accuracy
        self._engine = engine
        self._klass = klass

    def _clone(self, **kw_) -> "ObjectSet[O]":
        base = dict(where=self._where, ids=self._ids, around=self._around,
                    order_by=self._order_by, desc=self._desc, limit=self._limit,
                    offset=self._offset, show=self._show, object_set=self._object_set,
                    accuracy=self._accuracy, engine=self._engine, klass=self._klass)
        base.update(kw_)
        return ObjectSet(self._client, self._type, **base)

    def where(self, *preds: Predicate) -> "ObjectSet[O]":
        w = dict(self._where)
        for p in preds:
            w = _deep_merge(w, p.to_where())
        return self._clone(where=w)

    def with_ids(self, *ids) -> "ObjectSet[O]":
        flat = list(ids[0]) if len(ids) == 1 and isinstance(ids[0], (list, tuple, set)) else list(ids)
        return self._clone(ids=flat)

    def around(self, side: Link[T] | str) -> "ObjectSet[T]":
        if len(self._around) >= MAX_HOPS:
            raise ValueError(f"at most {MAX_HOPS} hops per query (planner cap)")
        name = getattr(side, "_name", None) or side
        if not isinstance(name, str):
            raise TypeError(f"a hop names a link side by its API name, got {side!r}")
        return self._clone(around=self._around + [name])

    def order_by(self, prop, desc: bool = False) -> "ObjectSet[O]":
        name = prop._name if isinstance(prop, Property) else str(prop)
        return self._clone(order_by=name, desc=desc)

    def limit(self, n: int) -> "ObjectSet[O]":
        if n is not None and (n < 0 or n > MAX_PAGE):
            raise ValueError(f"limit must be in [0, {MAX_PAGE}]")
        return self._clone(limit=n)

    def offset(self, n: int) -> "ObjectSet[O]":
        if n is not None and n < 0:
            raise ValueError("offset must not be negative")
        return self._clone(offset=n)

    def show(self, *fields) -> "ObjectSet[O]":
        out = []
        for f in fields:
            if isinstance(f, dict):
                out.append(f)
            else:
                out.append(f._name if isinstance(f, Property) else f)
        return self._clone(show=out)

    def accuracy(self, mode: str) -> "ObjectSet[O]":
        if mode not in ("exact", "approximate"):
            raise ValueError("accuracy is 'exact' or 'approximate'")
        return self._clone(accuracy=mode)

    def engine(self, name: str) -> "ObjectSet[O]":
        if name not in ("store",):
            raise ValueError("engine is 'store'")
        return self._clone(engine=name)

    def to_ir(self) -> dict:
        if self._limit is not None or self._offset is not None:
            raise ValueError("a paged set cannot be a member of a set operation")
        if self._object_set is not None:
            raise ValueError("a set already backed by a saved object set has no local IR")
        if self._ids is not None:
            node = {kw("op"): kw("static"), kw("ids"): list(self._ids),
                    kw("root"): self._root_rid()}
        else:
            node = {kw("op"): kw("scan"), kw("root"): self._root_rid()}
        if self._where:
            node = {kw("op"): kw("filter"), kw("where"): where_to_ir(self._where),
                    kw("in"): node}
        for side in self._around:
            node = {kw("op"): kw("search-around"), kw("link"): kw(side), kw("in"): node}
        return node

    def _root_rid(self) -> str:
        return self._client._type_rid(self._type)

    def _combine(self, other: "ObjectSet[O]", op: str) -> "ObjectSet[O]":
        if not isinstance(other, ObjectSet):
            return NotImplemented
        ir = {kw("op"): kw(op), kw("in"): [self.to_ir(), other.to_ir()]}
        created = self._client.object_sets.create_temporary(ir) or {}
        return ObjectSet(self._client, self._type, object_set=created.get("rid"),
                         klass=self._klass)

    def __or__(self, other: "ObjectSet[O]") -> "ObjectSet[O]":
        return self._combine(other, "union")

    def __and__(self, other: "ObjectSet[O]") -> "ObjectSet[O]":
        return self._combine(other, "intersect")

    def __sub__(self, other: "ObjectSet[O]") -> "ObjectSet[O]":
        return self._combine(other, "subtract")

    def _build(self, count: bool = False, aggs=None, group_by=None,
               explain: bool = False) -> dict:
        q: dict = {"find": self._type}
        if self._object_set is not None:
            q["object-set"] = self._object_set
        if self._ids is not None:
            q["ids"] = list(self._ids)
        if self._where:
            q["where"] = self._where
        if self._around:
            q["around"] = list(self._around)
        if self._show:
            q["show"] = self._show
        if aggs is not None:
            q["aggs"] = aggs
        if group_by:
            q["group-by"] = group_by
        if self._order_by:
            q["order-by"] = self._order_by
            if self._desc:
                q["sort-dir"] = "desc"
        if self._limit is not None:
            q["limit"] = self._limit
        if self._offset is not None:
            if self._limit is None:
                raise ValueError("offset needs a limit — call .limit(n) or .page(n, offset)")
            q["offset"] = self._offset
        if self._accuracy:
            q["accuracy"] = self._accuracy
        if self._engine:
            q["engine"] = self._engine
        if count:
            q["count?"] = True
        if explain:
            q["explain?"] = True
        return q

    def _post(self, body: dict) -> dict:
        return self._client._http.post(url("ontology-query"), body) or {}

    def _hydrate(self, row: dict, type_name: str | None = None) -> Any:
        tname = type_name or self._type
        if self._klass and not self._around:
            return self._klass(self._client, tname, row.get("id"), row.get("values") or {})
        return hydrate(self._client, tname, row.get("id"), row.get("values") or {})

    def _fetch(self, count: bool = False) -> tuple:
        resp = self._post(self._build(count=count))
        tname = resp.get("object-type") or self._type
        rows = resp.get("rows") or []
        return [self._hydrate(r, tname) for r in rows], resp.get("count")

    def iterate(self) -> Iterator[O]:
        if self._limit is not None:
            objects, _ = self._fetch()
            return iter(objects)
        return self.paginate(MAX_PAGE)

    def __iter__(self) -> Iterator[O]:
        return self.iterate()

    def all(self) -> list[O]:
        return list(self.iterate())

    def first(self) -> O | None:
        for o in self.limit(1).iterate():
            return o
        return None

    def count(self) -> int:
        resp = self._post(self._clone(limit=0, offset=None)._build(count=True))
        if "count" in resp:
            return resp["count"]
        return len(resp.get("rows") or [])

    def page(self, size: int, offset: int = 0, total: bool = True) -> Page:
        window = self.limit(size).offset(offset)
        resp = window._post(window._build(count=total))
        tname = resp.get("object-type") or self._type
        objects = [self._hydrate(r, tname) for r in resp.get("rows") or []]
        return Page(objects, offset, size, resp.get("count"))

    def paginate(self, size: int = 1000) -> Iterator[O]:
        offset = self._offset or 0
        while True:
            page = self.page(size, offset, total=False)
            for obj in page:
                yield obj
            if page.next_offset is None:
                return
            offset = page.next_offset

    def aggregate(self, *aggs, group_by=None, order_by=None, desc: bool = False,
                  limit: int | None = None) -> AggregateResult:
        measures = list(aggs)
        if not measures:
            raise ValueError("aggregate needs at least one measure")
        dims = list(group_by or [])
        for d in dims:
            if not isinstance(d, GroupBy):
                raise TypeError(f"group_by takes GroupBy terms, got {d!r}")
        wire_aggs = [a.to_wire() for a in measures]
        base = self._clone(order_by=None, desc=False, limit=limit, offset=None)
        if order_by is not None:
            key = order_by.result_key if isinstance(order_by, (Agg, GroupBy)) else str(order_by)
            base = base._clone(order_by=key, desc=desc)
        body = base._build(aggs=wire_aggs, group_by=[d.to_wire() for d in dims])
        resp = self._post(body)
        return AggregateResult(resp.get("object-type") or self._type, resp)

    def explain(self) -> dict:
        return self._post(self._build(explain=True))

    def get(self, primary_key) -> O:
        found = self.find(primary_key)
        if found is None:
            raise NotFound(f"no {self._type} {primary_key!r}")
        return found

    def find(self, primary_key) -> O | None:
        pk_attr = getattr(self._klass, "__primary_key__", None) if self._klass else None
        if pk_attr:
            return self.where(Property(self._type, pk_attr) == primary_key).first()
        return self.with_ids([primary_key]).first()

    def __repr__(self) -> str:
        return f"<ObjectSet {self._type} where={self._where!r}>"
