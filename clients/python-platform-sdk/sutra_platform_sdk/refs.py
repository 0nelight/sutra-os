from __future__ import annotations


class KwName(str):
    __slots__ = ()


def kw(name: str) -> KwName:
    return KwName(name)


def ref(type_name: str, pk) -> dict:
    return {"type": KwName(type_name), "pk": pk}
