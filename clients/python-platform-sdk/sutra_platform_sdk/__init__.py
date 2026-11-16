from __future__ import annotations

from .actions import Actions
from .aggregate import (Agg, AggregateResult, GroupBy, avg, bucket, by,
                        cardinality, count, link, max_, min_, origin, sum_,
                        width)
from .client import ObjectSets, SutraClient, Views
from .errors import (AuthError, NotFound, QueryError, ServerError, SutraError)
from .generate import generate, generate_from_client, generate_from_export
from .objects import OntologyObject
from .objectset import ObjectSet, Page
from .package import write_package
from .predicates import DynamicProps, Link, Predicate, Property
from .refs import KwName, kw, ref
from .schema import fingerprint
from .functions import Functions

__all__ = [
    "SutraClient", "ObjectSet", "Page", "OntologyObject", "Actions", "Functions",
    "Views", "ObjectSets",
    "Property", "Link", "Predicate", "DynamicProps",
    "Agg", "GroupBy", "AggregateResult",
    "count", "sum_", "avg", "min_", "max_", "cardinality",
    "by", "bucket", "width", "origin", "link",
    "kw", "ref", "KwName",
    "generate", "generate_from_client", "generate_from_export", "fingerprint",
    "write_package",
    "OntologyFS",
    "SutraError", "AuthError", "NotFound", "QueryError", "ServerError",
]

__version__ = "0.2.0"


def __getattr__(name):
    if name == "OntologyFS":
        from .fs import OntologyFS
        return OntologyFS
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
