from __future__ import annotations

import datetime as _dt
from typing import TYPE_CHECKING, Any, Generic, TypeVar

from ._naming import snake_to_kebab

if TYPE_CHECKING:
    from .objects import OntologyObject

Q = TypeVar("Q")
T = TypeVar("T", bound="OntologyObject")

OPS = {"=", "<", ">", "<=", ">=", "not=", "starts-with", "contains"}


def _deep_merge(a: dict, b: dict) -> dict:
    out = dict(a)
    for k, v in b.items():
        if k in out and isinstance(out[k], dict) and isinstance(v, dict):
            out[k] = _deep_merge(out[k], v)
        else:
            out[k] = v
    return out


def where_to_ir(where: dict) -> list:
    from .refs import kw
    terms = []
    for field, ops in sorted(where.items()):
        if not isinstance(ops, dict) or not all(o in OPS for o in ops):
            raise ValueError(
                f"{field!r} is a link sub-filter; object-set algebra takes "
                "root-attribute predicates only")
        for op, value in sorted(ops.items()):
            terms.append([kw(op), kw(field), value])
    return [kw("and")] + terms


class Predicate:

    __slots__ = ("where",)

    def __init__(self, where: dict | None = None):
        self.where = where or {}

    def __and__(self, other: "Predicate") -> "Predicate":
        return Predicate(_deep_merge(self.where, other.where))

    def to_where(self) -> dict:
        return self.where

    def to_ir(self) -> list:
        return where_to_ir(self.where)

    def __repr__(self) -> str:
        return f"Predicate({self.where!r})"

    __hash__ = None


_ACCEPTS = {
    "string": (str,), "text": (str,), "keyword": (str,), "uuid": (str,),
    "boolean": (bool,),
    "byte": (int,), "short": (int,), "integer": (int,), "long": (int,),
    "double": (int, float), "float": (int, float),
}
_STRING_OPS = {"starts-with", "contains"}


class TypeMismatch(TypeError):
    pass


class Property(Generic[Q]):

    __slots__ = ("_type", "_name", "_declared")

    def __init__(self, type_name: str, attr_name: str, declared: str | None = None):
        self._type = type_name
        self._name = attr_name
        self._declared = declared

    def _check(self, op: str, value) -> None:
        if value is None or isinstance(value, _dt.date):
            return
        if op in _STRING_OPS:
            if not isinstance(value, str):
                raise TypeMismatch(
                    f"{self._type}.{self._name} {op} needs a string, "
                    f"got {type(value).__name__} {value!r}")
            return
        accepts = _ACCEPTS.get(str(self._declared or ""))
        if accepts is None:
            return
        if isinstance(value, bool) and bool not in accepts:
            ok = False
        else:
            ok = isinstance(value, accepts)
        if not ok:
            raise TypeMismatch(
                f"{self._type}.{self._name} is declared {self._declared}, "
                f"got {type(value).__name__} {value!r} — the server would "
                f"answer this with a type error from the database")

    def _p(self, op: str, value) -> Predicate:
        self._check(op, value)
        if isinstance(value, _dt.date):
            value = value.isoformat()
        return Predicate({self._name: {op: value}})

    def __eq__(self, value: Q) -> Predicate:  # type: ignore[override]
        return self._p("=", value)

    def __lt__(self, value: Q) -> Predicate:
        return self._p("<", value)

    def __gt__(self, value: Q) -> Predicate:
        return self._p(">", value)

    def __le__(self, value: Q) -> Predicate:
        return self._p("<=", value)

    def __ge__(self, value: Q) -> Predicate:
        return self._p(">=", value)

    def ne(self, value: Q) -> Predicate:
        return self._p("not=", value)

    def starts_with(self, value: str) -> Predicate:
        return self._p("starts-with", value)

    def contains(self, value: str) -> Predicate:
        return self._p("contains", value)

    def is_null(self) -> Predicate:
        return self._p("=", None)

    __hash__ = None

    def __repr__(self) -> str:
        return f"<Property {self._type}.{self._name}>"


class Link(Generic[T]):

    __slots__ = ("_type", "_name", "_to")

    def __init__(self, type_name: str, side: str, to_type: str | None = None):
        self._type = type_name
        self._name = side
        self._to = to_type

    def __call__(self, *preds: Predicate) -> Predicate:
        sub: dict = {}
        for p in preds:
            sub = _deep_merge(sub, p.to_where())
        return Predicate({self._name: sub})

    def __repr__(self) -> str:
        return f"<Link {self._type}.{self._name}->{self._to}>"


class DynamicProps:

    __slots__ = ("_type",)

    def __init__(self, type_name: str):
        self._type = type_name

    def __getattr__(self, name: str) -> Property[Any]:
        return Property(self._type, snake_to_kebab(name))
