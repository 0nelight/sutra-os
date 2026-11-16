from __future__ import annotations

from typing import Any

from ._naming import kebab_to_snake, snake_to_kebab
from .refs import kw

AGG_FNS = {"count", "sum", "avg", "min", "max", "cardinality"}
ATTRLESS = {"count"}
BUCKET_UNITS = {"year", "quarter", "month", "week", "day", "hour", "minute", "second"}
MAX_BUCKETS = 10000


class Agg:

    __slots__ = ("fn", "attr", "name")

    def __init__(self, fn: str, attr=None, name: str | None = None):
        if fn not in AGG_FNS:
            raise ValueError(f"unknown aggregate fn {fn!r} — one of {sorted(AGG_FNS)}")
        if attr is None and fn not in ATTRLESS:
            raise ValueError(f"aggregate {fn!r} needs an attribute")
        self.fn = fn
        self.attr = _attr_name(attr) if attr is not None else None
        self.name = snake_to_kebab(name) if name else None

    @property
    def result_key(self) -> str:
        if self.name:
            return self.name
        return f"{self.attr}-{self.fn}" if self.attr else self.fn

    def to_wire(self) -> dict:
        out = {kw("fn"): kw(self.fn)}
        if self.attr is not None:
            out[kw("attr")] = kw(self.attr)
        if self.name:
            out[kw("name")] = kw(self.name)
        return out

    def __repr__(self) -> str:
        return f"<Agg {self.result_key}>"


class GroupBy:

    __slots__ = ("kind", "attr", "unit", "width", "link", "title_attr")

    def __init__(self, kind: str, *, attr=None, unit=None, width=None,
                 link=None, title_attr=None):
        self.kind = kind
        self.attr = _attr_name(attr) if attr is not None else None
        self.unit = unit
        self.width = width
        self.link = snake_to_kebab(link) if link else None
        self.title_attr = _attr_name(title_attr) if title_attr is not None else None

    @property
    def result_key(self) -> str:
        if self.kind == "attr":
            return self.attr
        if self.kind == "bucket":
            return f"{self.attr}-{self.unit}"
        if self.kind == "width":
            return f"{self.attr}-w{self.width}"
        if self.kind == "origin":
            return "origin"
        return self.link

    def to_wire(self):
        if self.kind == "attr":
            return kw(self.attr)
        if self.kind == "bucket":
            return [kw("bucket"), kw(self.attr), kw(self.unit)]
        if self.kind == "width":
            return [kw("width"), kw(self.attr), self.width]
        if self.kind == "origin":
            return [kw("origin")]
        if self.title_attr:
            return [kw("link"), kw(self.link), kw(self.title_attr)]
        return [kw("link"), kw(self.link)]

    def __repr__(self) -> str:
        return f"<GroupBy {self.result_key}>"


def _attr_name(prop) -> str:
    name = getattr(prop, "_name", None)
    return name if name is not None else snake_to_kebab(str(prop))


def count(name: str | None = None) -> Agg:
    return Agg("count", None, name)


def sum_(attr, name: str | None = None) -> Agg:
    return Agg("sum", attr, name)


def avg(attr, name: str | None = None) -> Agg:
    return Agg("avg", attr, name)


def min_(attr, name: str | None = None) -> Agg:
    return Agg("min", attr, name)


def max_(attr, name: str | None = None) -> Agg:
    return Agg("max", attr, name)


def cardinality(attr, name: str | None = None) -> Agg:
    return Agg("cardinality", attr, name)


def by(attr) -> GroupBy:
    return GroupBy("attr", attr=attr)


def bucket(attr, unit: str) -> GroupBy:
    if unit not in BUCKET_UNITS:
        raise ValueError(f"unknown bucket unit {unit!r} — one of {sorted(BUCKET_UNITS)}")
    return GroupBy("bucket", attr=attr, unit=unit)


def width(attr, n) -> GroupBy:
    if not (isinstance(n, (int, float)) and n > 0):
        raise ValueError("width needs a positive number")
    return GroupBy("width", attr=attr, width=n)


def origin() -> GroupBy:
    return GroupBy("origin")


def link(side, title_attr=None) -> GroupBy:
    return GroupBy("link", link=getattr(side, "_name", None) or side, title_attr=title_attr)


class AggregateResult:

    def __init__(self, object_type: str, payload: dict):
        self.object_type = object_type
        self._payload = payload or {}
        keys = self._payload.get("keys") or {}
        self.group_keys = list(keys.get("group") or [])
        self.metric_keys = list(keys.get("metrics") or [])
        self.rows = list(self._payload.get("aggregations") or [])

    def __iter__(self):
        return iter(self.rows)

    def __len__(self) -> int:
        return len(self.rows)

    def __getitem__(self, i):
        return self.rows[i]

    def scalar(self, metric: str | None = None):
        if not self.rows:
            return None
        metric = snake_to_kebab(metric) if metric else None
        row = self.rows[0]
        metrics = row.get("metrics")
        if metrics is None:
            return row.get(metric) if metric else next(iter(row.values()), None)
        return metrics.get(metric) if metric else next(iter(metrics.values()), None)

    def flat(self) -> list[dict[str, Any]]:
        out = []
        for row in self.rows:
            if "group" in row or "metrics" in row:
                merged = dict(row.get("group") or {})
                merged.update(row.get("metrics") or {})
            else:
                merged = dict(row)
            out.append({kebab_to_snake(str(k)): v for k, v in merged.items()})
        return out

    def __repr__(self) -> str:
        return (f"<AggregateResult {self.object_type} "
                f"{len(self.rows)} buckets by {self.group_keys}>")
