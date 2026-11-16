from __future__ import annotations

from contextlib import contextmanager

from . import schema as _schema
from ._naming import pascal_to_kebab
from .actions import Actions
from .objects import hydrate
from .errors import ResultTruncated
from .objectset import ObjectSet
from .paths import url
from .predicates import DynamicProps
from .transport import Http
from .functions import Functions


class Views:

    def __init__(self, client):
        self._client = client

    def list(self, area=None, mode=None):
        params = {}
        if area:
            params["area"] = area
        if mode:
            params["mode"] = mode
        return self._client._http.get(url("views"), params or None)

    def get(self, view_id: str):
        return self._client._http.get(url("view", id=view_id))


class ObjectSets:

    def __init__(self, client):
        self._client = client

    def list(self) -> dict:
        return self._client._http.get(url("object-sets")) or {}

    def create_temporary(self, ir=None, *, materialize: bool = False, **spec) -> dict:
        if (ir is None) == (not spec):
            raise ValueError("pass either an IR or a query spec, not both/neither")
        body = {"ir": ir} if ir is not None else dict(spec)
        if materialize:
            body["materialize"] = True
        return self._client._http.post(url("object-sets-temp"), body) or {}

    def delete_temporary(self, rid: str) -> dict:
        return self._client._http.delete(url("object-set-temp-by", rid=rid)) or {}


class _ObjectsAccessor:

    def __init__(self, client):
        self._client = client
        self._by_pascal: dict = {}
        self._by_type: dict = {}

    def register(self, klass):
        self._by_pascal[klass.__name__] = klass
        self._by_type[klass.__sutra_type__] = klass
        return klass

    def klass_for(self, type_name: str):
        return self._by_type.get(type_name)

    def __getattr__(self, name: str) -> ObjectSet:
        if name.startswith("_"):
            raise AttributeError(name)
        klass = self._by_pascal.get(name)
        type_name = klass.__sutra_type__ if klass else pascal_to_kebab(name)
        return ObjectSet(self._client, type_name, klass=klass)

    def __getitem__(self, type_name: str) -> ObjectSet:
        return ObjectSet(self._client, type_name)


class Builds:

    def __init__(self, client):
        self._client = client

    def create(self, *, only=None, strategy: str = "selected",
               abort_on_failure: bool = False, force_build: bool = False,
               retry_count: int = 0, retry_backoff=None, **extra):
        body = {"strategy": strategy, "abort-on-failure": abort_on_failure,
                "retry-count": retry_count, "force": force_build, **extra}
        if retry_backoff is not None:
            body["retry-backoff"] = retry_backoff
        if only is not None:
            body["only"] = list(only)
        return self._client._http.post(url("import-run"), body)

    def status(self):
        return self._client._http.get(url("import-status"))

    def active(self):
        return self._client._http.get(url("builds-active"))

    def get(self, run_id: str):
        return self._client._http.get(url("run", id=run_id))


class Sources:

    def __init__(self, client):
        self._client = client

    DEFAULT_ROWS = 50
    MAX_ROWS = 1000

    def query(self, name: str, sql: str, limit: int | None = None):
        window = limit if limit is not None else self.MAX_ROWS
        out = self._client._http.post(url("datasource-query", name=name),
                                      {"sql": sql, "limit": window})
        rows = out.get("rows") or []
        if limit is None and len(rows) >= self.MAX_ROWS:
            raise ResultTruncated(
                f"{name}: the answer filled the {self.MAX_ROWS}-row ceiling, so "
                f"there are probably more rows than you were given — page with "
                f"ORDER BY … OFFSET n ROWS FETCH NEXT {self.MAX_ROWS} ROWS ONLY, "
                f"or pass limit= to say a window is what you wanted")
        return out

    def tables(self, name: str):
        return self._client._http.get(url("datasource-tables", name=name))

    def list(self):
        return self._client._http.get(url("datasources"))


class _Orchestration:
    def __init__(self, client):
        self.builds = Builds(client)


class _Connectivity:
    def __init__(self, client):
        self.sources = Sources(client)


class _Ontology:

    def __init__(self, client):
        self.objects = _ObjectsAccessor(client)
        self.functions = Functions(client)
        self.actions = Actions(client)


class _AtView:

    def __init__(self, client, commit: str):
        self._client = client
        self._commit = commit

    def objects(self, type_name: str) -> list:
        resp = self._client._http.get(url("at-objects", commit=self._commit, type=type_name)) or {}
        return [hydrate(self._client, type_name, r.get("id"), r.get("values") or {})
                for r in resp.get("rows") or []]

    def object_type(self, type_name: str) -> dict:
        return self._client._http.get(
            url("at-object-type", commit=self._commit, type=type_name)) or {}


class SutraClient:
    def __init__(self, base_url: str, *, token: str | None = None,
                 username: str | None = None, password: str | None = None,
                 http: Http | None = None):
        self._http = http or Http(base_url, token)
        self.ontology = _Ontology(self)
        self.views = Views(self)
        self.object_sets = ObjectSets(self)
        self.orchestration = _Orchestration(self)
        self.connectivity = _Connectivity(self)
        self._schema_cache = None
        self._rid_to_name = None
        self._name_to_rid = None
        if token is None and username is not None:
            self.login(username, password)

    def login(self, username: str, password: str) -> dict:
        r = self._http.post(url("login"), {"username": username, "password": password})
        self._http.token = r["token"]
        return r

    def me(self) -> dict:
        return self._http.get(url("me"))

    def logout(self):
        return self._http.post(url("logout"), {})

    @contextmanager
    def branch(self, name: str):
        previous = self._http.branch
        self._http.branch = name
        self._schema_cache = None
        try:
            yield self
        finally:
            self._http.branch = previous
            self._schema_cache = None

    def schema(self, refresh: bool = False) -> dict:
        if self._schema_cache is None or refresh:
            self._schema_cache = self._http.get(url("schema")) or {}
            self._rid_to_name = None
            self._name_to_rid = None
        return self._schema_cache

    def object_type_spec(self, name: str) -> dict:
        return self._http.get(url("object-type-by", name=name))

    def _type_name(self, rid: str) -> str:
        if self._rid_to_name is None:
            self._rid_to_name = _schema.rid_to_name(self.schema())
        return self._rid_to_name.get(rid, rid)

    def _type_rid(self, type_name: str) -> str:
        if self._name_to_rid is None:
            self._name_to_rid = _schema.name_to_rid(self.schema())
        rid = self._name_to_rid.get(type_name)
        if rid is None:
            raise KeyError(f"no object type {type_name!r} in this ontology")
        return rid

    def props(self, type_name: str) -> DynamicProps:
        return DynamicProps(type_name)

    def at(self, commit: str | int) -> _AtView:
        return _AtView(self, str(commit))

    def blob(self, content_hash: str):
        return self._http.get_stream(url("blob", hash=content_hash))

    def upload_blob(self, data: bytes) -> dict:
        return self._http.post_bytes(url("blob-upload"), data)

    def __repr__(self) -> str:
        return f"<SutraClient {self._http.base}>"
