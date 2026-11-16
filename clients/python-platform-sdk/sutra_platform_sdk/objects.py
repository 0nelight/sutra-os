from __future__ import annotations

import datetime as _dt
from typing import TYPE_CHECKING, Any, Generic, Iterator, TypeVar, overload

from ._naming import snake_to_kebab
from .errors import NotFound
from .paths import url
from .predicates import Link, Predicate, Property

if TYPE_CHECKING:
    from .aggregate import AggregateResult
    from .objectset import ObjectSet

O = TypeVar("O", bound="OntologyObject")
T = TypeVar("T", bound="OntologyObject")
V = TypeVar("V")
Q = TypeVar("Q")


def hydrate(client, type_name: str, oid, values: dict) -> "OntologyObject":
    registry = getattr(getattr(client, "ontology", None), "objects", None)
    klass = registry.klass_for(type_name) if registry is not None else None
    return (klass or OntologyObject)(client, type_name, oid, values)


class _TypeMeta(type):

    def objects(cls: type[O]) -> ObjectSet[O]:
        from .objectset import ObjectSet
        client = cls.__dict__.get("_sutra_client")
        if client is None:
            raise RuntimeError(f"{cls.__name__} is not bound to a client: "
                               "call register_all(client) first")
        return ObjectSet(client, cls.__sutra_type__, klass=cls)

    def where(cls: type[O], *preds: Predicate) -> ObjectSet[O]:
        return cls.objects().where(*preds)

    def get(cls: type[O], primary_key: Any) -> O:
        return cls.objects().get(primary_key)

    def all(cls: type[O]) -> list[O]:
        return cls.objects().all()

    def count(cls) -> int:
        return cls.objects().count()

    def aggregate(cls, *aggs, **kw) -> AggregateResult:
        return cls.objects().aggregate(*aggs, **kw)

    def __iter__(cls: type[O]) -> Iterator[O]:
        return iter(cls.objects())


def _read(declared: str | None, v):
    if isinstance(v, str):
        if declared == "date":
            return _dt.date.fromisoformat(v[:10])
        if declared in ("timestamp", "instant"):
            return _dt.datetime.fromisoformat(v)
    return v


class Attr(Generic[V, Q]):

    def __init__(self, type_name: str, wire: str, declared: str | None = None):
        self.prop: Property[Q] = Property(type_name, wire, declared)

    @overload
    def __get__(self, obj: None, owner: Any) -> Property[Q]: ...
    @overload
    def __get__(self, obj: OntologyObject, owner: Any) -> V: ...

    def __get__(self, obj, owner):
        if obj is None:
            return self.prop
        return _read(self.prop._declared, obj._values.get(self.prop._name))


class Side:

    def __init__(self, type_name: str, side: str, to_type: str, one: bool = False):
        self.link = Link(type_name, side, to_type)
        self.one = one

    def __get__(self, obj, owner):
        if obj is None:
            return self.link
        found = obj.search_around(self.link._name)
        if self.one:
            return found[0] if found else None
        return found


class One(Side, Generic[T]):
    def __init__(self, type_name: str, side: str, to_type: str):
        super().__init__(type_name, side, to_type, one=True)

    @overload
    def __get__(self, obj: None, owner: Any) -> Link[T]: ...
    @overload
    def __get__(self, obj: OntologyObject, owner: Any) -> T | None: ...

    def __get__(self, obj, owner):
        return super().__get__(obj, owner)


class Many(Side, Generic[T]):
    @overload
    def __get__(self, obj: None, owner: Any) -> Link[T]: ...
    @overload
    def __get__(self, obj: OntologyObject, owner: Any) -> list[T]: ...

    def __get__(self, obj, owner):
        return super().__get__(obj, owner)


class OntologyObject(metaclass=_TypeMeta):

    __sutra_type__: str = ""
    __primary_key__: str | None = None
    __title_key__: str = "name"

    def __init__(self, client, type_name: str, oid: str, values: dict):
        object.__setattr__(self, "_client", client)
        object.__setattr__(self, "_type", type_name or self.__sutra_type__)
        object.__setattr__(self, "id", oid)
        object.__setattr__(self, "_values", values or {})

    def __getattr__(self, name: str):
        values = object.__getattribute__(self, "_values")
        key = snake_to_kebab(name)
        if key in values:
            return values[key]
        raise AttributeError(name)

    @property
    def values(self) -> dict:
        return self._values

    @property
    def title(self):
        return self._values.get(self.__title_key__, self.id)

    def around(self) -> dict:
        return self._client._http.post(url("instances-around"), {"ids": [self.id]})

    def search_around(self, side: str):
        body = {"find": self._type, "ids": [self.id], "around": [snake_to_kebab(side)]}
        resp = self._client._http.post(url("ontology-query"), body) or {}
        tname = resp.get("object-type")
        return [hydrate(self._client, tname, r.get("id"), r.get("values") or {})
                for r in resp.get("rows") or []]

    def __repr__(self) -> str:
        return f"<{type(self).__name__ if type(self) is not OntologyObject else self._type} {self.title!r} id={self.id}>"
