from __future__ import annotations

import keyword
import re


def kebab_to_pascal(s: str) -> str:
    return "".join(p[:1].upper() + p[1:] for p in re.split(r"[-_/]", s) if p)


def kebab_to_snake(s: str) -> str:
    return s.replace("-", "_")


def snake_to_kebab(s: str) -> str:
    return s.replace("_", "-")


def pascal_to_kebab(s: str) -> str:
    s = re.sub(r"(.)([A-Z][a-z]+)", r"\1-\2", s)
    s = re.sub(r"([a-z0-9])([A-Z])", r"\1-\2", s)
    return s.lower()


_INVALID = re.compile(r"[^0-9a-zA-Z_]")


def py_identifier(s: str):
    out = _INVALID.sub("", kebab_to_snake(s))
    if not out:
        return None
    if out[0].isdigit():
        out = "_" + out
    if keyword.iskeyword(out):
        out += "_"
    return out if out.isidentifier() else None
