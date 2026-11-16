import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

SCHEMA = {
    "object-types": {
        "ri.sutra.main.object-types.aaa": {
            "object-type-name": "file",
            "attribute-list": [
                {"attribute-name": "name", "attribute-type": "string"},
                {"attribute-name": "kind", "attribute-type": "keyword"},
                {"attribute-name": "hash", "attribute-type": "string"},
                {"attribute-name": "size", "attribute-type": "number"},
            ],
            "match-keys": ["hash"],
            "symbol": "📄",
        },
        "ri.sutra.main.object-types.bbb": {
            "object-type-name": "support-case",
            "attribute-list": [
                {"attribute-name": "name", "attribute-type": "string"},
                {"attribute-name": "status", "attribute-type": "keyword"},
            ],
        },
        "ri.sutra.main.object-types.ccc": {
            "object-type-name": "person",
            "attribute-list": [{"attribute-name": "name", "attribute-type": "string"}],
        },
    },
    "actions": {
        "ri.sutra.main.actions.cf": {
            "action-name": "create-file",
            "description": "Create a file object.",
            "params": {
                "kind": {"type": "keyword"},
                "name": {"type": "string", "required": True},
                "size": {"type": "long"},
                "tags": {"type": "string", "multiple": True},
            },
        },
        "ri.sutra.main.actions.rc": {
            "action-name": "resolve-case",
            "params": {"case": {"type": "ri.sutra.main.object-types.bbb", "required": True}},
        },
    },
    "functions": {
        "ri.sutra.main.functions.f1": {
            "name": "schedule-maintenance",
            "language": "clojure",
            "source": "(fn [_] nil)",
            "param-types": {"aircraft": "string", "hours": "long"},
        },
        "ri.sutra.main.functions.f2": {
            "name": "untyped-fn",
            "language": "clojure",
            "source": "(fn [_] nil)",
        },
    },
    "link-types": {
        "ri.sutra.main.link-types.e1": {
            "link-type-name": "evidence-for",
            "from-object-type": "file",
            "to-object-type": "support-case",
            "cardinality": "many-to-many",
        },
        "ri.sutra.main.link-types.e2": {
            "link-type-name": "case-customer",
            "from-object-type": "support-case",
            "to-object-type": "person",
            "cardinality": "many-to-one",
        },
    },
}

SELF_LINK_SCHEMA = {
    "object-types": {
        "ri.sutra.main.object-types.ppp": {
            "object-type-name": "projekt",
            "attribute-list": [{"attribute-name": "name", "attribute-type": "string"}],
            "match-keys": ["name"],
        },
    },
    "link-types": {
        "ri.sutra.main.link-types.hp": {
            "link-type-name": "hauptprojekt-projekte",
            "from-object-type": "ri.sutra.main.object-types.ppp",
            "to-object-type": "ri.sutra.main.object-types.ppp",
            "cardinality": "many-to-one",
            "forward-api-name": "hauptprojekt",
            "reverse-api-name": "projekte",
        },
    },
}

ROWS = {
    "file": [
        {"id": "f1", "values": {"name": "frank_trace.PML", "kind": "trace", "hash": "h1", "size": 100}},
        {"id": "f2", "values": {"name": "shot.png", "kind": "screenshot", "hash": "h2", "size": 50}},
        {"id": "f3", "values": {"name": "kesselmeier.PML", "kind": "trace", "hash": "h3", "size": 300}},
    ],
}


_MISSING = object()


class FakeStream:
    def __init__(self, content=b"BYTES"):
        self.content = content


class FakeHttp:
    def __init__(self, schema=None, rows=None, around=None):
        self.base = "http://fake"
        self.token = "tok"
        self._schema = schema if schema is not None else SCHEMA
        self._rows = rows if rows is not None else ROWS
        self._around = around or {"objects": {}, "links": []}
        self._sets = {}
        self.calls = []

    OPS = {
        "=": lambda a, b: a == b,
        "not=": lambda a, b: a != b,
        "<": lambda a, b: a is not None and a < b,
        ">": lambda a, b: a is not None and a > b,
        "<=": lambda a, b: a is not None and a <= b,
        ">=": lambda a, b: a is not None and a >= b,
        "starts-with": lambda a, b: isinstance(a, str) and a.startswith(b),
        "contains": lambda a, b: isinstance(a, str) and b in a,
    }

    def _query(self, body):
        find = body.get("find")
        if find is None and body.get("object-set"):
            find = self._set_root(body["object-set"])
        rows = list(self._rows.get(find, []))
        if body.get("ids") is not None:
            wanted = set(body["ids"])
            rows = [r for r in rows if r["id"] in wanted]
        for field, ops in (body.get("where") or {}).items():
            for op, val in ops.items():
                if op not in self.OPS:
                    raise AssertionError(f"non-surface op {op!r}")
                rows = [r for r in rows if self.OPS[op]((r.get("values") or {}).get(field), val)]
        for hop in body.get("around") or []:
            if not isinstance(hop, str):
                raise AssertionError(f"a hop is a side name, got {hop!r}")
            find = self._far_type(find, hop)
            rows = list(self._rows.get(find, []))
        total = len(rows)
        if body.get("aggs") is not None:
            return self._aggregate(find, rows, body)
        if body.get("order-by"):
            rows.sort(key=lambda r: (r.get("values") or {}).get(body["order-by"]),
                      reverse=body.get("sort-dir") == "desc")
        if body.get("offset") is not None and body.get("limit") is None:
            raise AssertionError("offset without limit")
        if body.get("offset"):
            rows = rows[body["offset"]:]
        if body.get("limit") is not None:
            rows = rows[:body["limit"]]
        resp = {"object-type": find, "rows": rows}
        if body.get("count?"):
            resp["count"] = total
        return resp

    def _aggregate(self, find, rows, body):
        dims = body.get("group-by") or []
        aggs = body["aggs"]
        if aggs == "count" and not dims:
            return {"object-type": find, "aggregations": [{"count": len(rows)}]}
        dim_keys = [d if isinstance(d, str) else str(d) for d in dims]
        buckets = {}
        for r in rows:
            values = r.get("values") or {}
            key = tuple(values.get(d) for d in dim_keys)
            buckets.setdefault(key, []).append(values)
        metric_keys = []
        out = []
        for key, members in buckets.items():
            metrics = {}
            for a in aggs:
                fn, attr = a["fn"], a.get("attr")
                name = a.get("name") or (f"{attr}-{fn}" if attr else fn)
                nums = [m.get(attr) for m in members if m.get(attr) is not None]
                metrics[name] = (len(members) if fn == "count"
                                 else sum(nums) if fn == "sum"
                                 else (sum(nums) / len(nums) if nums else None) if fn == "avg"
                                 else min(nums, default=None) if fn == "min"
                                 else max(nums, default=None) if fn == "max"
                                 else len(set(nums)))
                if name not in metric_keys:
                    metric_keys.append(name)
            out.append({"group": dict(zip(dim_keys, key)), "metrics": metrics})
        if body.get("order-by"):
            out.sort(key=lambda b: {**b["group"], **b["metrics"]}.get(body["order-by"]),
                     reverse=body.get("sort-dir") == "desc")
        if body.get("limit") is not None:
            out = out[:body["limit"]]
        return {"object-type": find,
                "keys": {"group": dim_keys, "metrics": metric_keys},
                "aggregations": out}

    def _far_type(self, from_type, side):
        from sutra_platform_sdk.schema import name_to_rid, sides
        rid = name_to_rid(self._schema).get(from_type)
        info = sides(self._schema, rid).get(side)
        if info is None:
            raise AssertionError(f"{from_type!r} has no link side {side!r}")
        return info["far_type"]

    def _set_root(self, rid):
        stored = self._sets.get(rid) or {}
        node = stored.get("ir") or {}
        while isinstance(node, dict) and "in" in node:
            node = node["in"]
            if isinstance(node, list):
                node = node[0]
        root = node.get("root") if isinstance(node, dict) else None
        return (self._schema.get("object-types") or {}).get(root, {}).get("object-type-name")

    def _action_spec(self, action):
        for spec in (self._schema.get("actions") or {}).values():
            if (spec or {}).get("action-name") == action:
                return spec
        return {}

    def _action_params(self, body):
        params = self._action_spec(body["action"]).get("params") or {}
        held = body.get("args") or {}
        missing = [p for p, s in params.items()
                   if (s or {}).get("required") and held.get(p) in (None, "")]
        return {"params": params, "values": {}, "missing": missing, "invalid": [],
                "blocked?": bool(missing),
                "reason": f"{body['action']} braucht {','.join(missing)}" if missing else None}

    def _edits(self, action, rows):
        create = action.startswith("create-")
        kind = "add-object" if create else "modify-object"
        edits = [{"type": kind, "object-type": action.split("-", 1)[-1],
                  "object-id": r.get("id", "new-id"),
                  "primary-key": (r.get("values") or {}).get("hash")} for r in rows]
        return {"type": "edits", "edits": edits,
                "added-objects-count": len(edits) if create else 0,
                "modified-objects-count": 0 if create else len(edits),
                "deleted-objects-count": 0, "added-links-count": 0,
                "deleted-links-count": 0}

    def post(self, path, body):
        self.calls.append(("POST", path, body))
        if path.startswith("/api/instances/around"):
            return self._around
        if path.startswith("/api/ontology/query"):
            return self._query(body)
        if path.startswith("/api/ontology/object-sets/temporary"):
            rid = f"ri.sutra.main.object-sets.tmp{len(self._sets) + 1}"
            self._sets[rid] = dict(body)
            return {"rid": rid, "ir": body.get("ir"), "kind": "dynamic",
                    "lifecycle": "temporary", "temporary": True}
        if path.startswith("/api/actions/params"):
            return self._action_params(body)
        if path.startswith("/api/actions/batch"):
            rows = body.get("requests") or []
            out = {"action": body["action"], "staged": True,
                   "seqs": list(range(1, len(rows) + 1)),
                   "ids": [r.get("id", "new-id") for r in rows]}
            if "return-edits=true" in path:
                out["edits"] = self._edits(body["action"], rows)
            return out
        if path.startswith("/api/actions"):
            out = {"action": body["action"], "staged": True, "seq": 1, "id": "new-id"}
            if "return-edits=true" in path:
                out["edits"] = self._edits(body["action"], [body.get("args") or {}])
            return out
        if path == "/api/login":
            return {"token": "tok", "username": body.get("username")}
        return {}

    def get(self, path, params=None):
        self.calls.append(("GET", path, params))
        if path == "/api/schema":
            return self._schema
        return {}

    def delete(self, path, params=None):
        self.calls.append(("DELETE", path, params))
        from urllib.parse import unquote
        rid = unquote(path.rsplit("/", 1)[-1])
        return {"deleted": self._sets.pop(rid, _MISSING) is not _MISSING}

    def get_stream(self, path, params=None):
        self.calls.append(("STREAM", path, params))
        return FakeStream()


@pytest.fixture
def fake_http():
    return FakeHttp()


@pytest.fixture
def client(fake_http):
    from sutra_platform_sdk import SutraClient
    return SutraClient("http://fake", http=fake_http)
