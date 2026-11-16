import contextlib
import json
import sys
import traceback

from sutra_runtime import get_source
from sutra_transforms import _from_wire, _to_wire


class NeedData(BaseException):
    def __init__(self, request):
        super().__init__(request)
        self.request = request


class UserFacingError(Exception):
    pass


_MANIFEST = {"types": {}}
_CACHE = {}
_CLASSES = {}


def _key(request):
    return json.dumps(request, sort_keys=True, default=str)


def _fetch(request):
    k = _key(request)
    if k not in _CACHE:
        raise NeedData(request)
    return _CACHE[k]


def _type(name):
    t = _MANIFEST["types"].get(name)
    if t is None:
        raise AttributeError("no object type " + repr(name))
    return t


def _by_api(api):
    for name, t in _MANIFEST["types"].items():
        if t["api"] == api:
            return name
    return None


def object_class(name):
    if name not in _CLASSES:
        t = _type(name)
        _CLASSES[name] = type(t["api"], (_Object,), {"_t": name, "object_type": _Properties(name)})
    return _CLASSES[name]


def _wire(v):
    if isinstance(v, (_Object, _Editable)):
        return v._pk()
    if hasattr(v, "isoformat"):
        return v.isoformat()
    return _to_wire(v)


class _Pred:
    def __init__(self, ir):
        self.ir = ir

    def __and__(self, other):
        return _Pred(["and", self.ir, other.ir])

    def __or__(self, other):
        return _Pred(["or", self.ir, other.ir])

    def __invert__(self):
        return _Pred(_negate(self.ir))


_FLIP = {"=": "not=", "not=": "=", "<": ">=", ">=": "<", ">": "<=", "<=": ">"}


def _negate(ir):
    op = ir[0]
    if op in _FLIP:
        return [_FLIP[op]] + ir[1:]
    if op == "nil?":
        return ["nil?", ir[1], not ir[2]]
    if op == "and":
        return ["or"] + [_negate(x) for x in ir[1:]]
    if op == "or":
        return ["and"] + [_negate(x) for x in ir[1:]]
    raise UserFacingError("cannot negate a " + op + " filter")


class _Order:
    def __init__(self, attr, direction):
        self.attr = attr
        self.direction = direction


class _Prop:
    def __init__(self, type_name, attr):
        self._t = type_name
        self.attr = attr

    def _cmp(self, op, v):
        return _Pred([op, self.attr, _wire(v)])

    def __eq__(self, v):
        return self._cmp("=", v)

    def __ne__(self, v):
        return self._cmp("not=", v)

    def __lt__(self, v):
        return self._cmp("<", v)

    def __le__(self, v):
        return self._cmp("<=", v)

    def __gt__(self, v):
        return self._cmp(">", v)

    def __ge__(self, v):
        return self._cmp(">=", v)

    __hash__ = object.__hash__

    def is_null(self, value=True):
        return _Pred(["nil?", self.attr, bool(value)])

    def is_in(self, values):
        return _Pred(["or"] + [["=", self.attr, _wire(v)] for v in values])

    in_ = is_in
    is_member_of = is_in

    def starts_with(self, v):
        return _Pred(["starts-with", self.attr, _wire(v)])

    def contains(self, v):
        return _Pred(["contains", self.attr, _wire(v)])

    def asc(self):
        return _Order(self.attr, "asc")

    def desc(self):
        return _Order(self.attr, "desc")


class _Properties:
    def __init__(self, type_name):
        self._t = type_name

    def __getattr__(self, name):
        attr = _type(self._t)["props"].get(name)
        if attr is None:
            raise AttributeError(_type(self._t)["api"] + " has no property " + repr(name))
        return _Prop(self._t, attr)


class _Count:
    def __init__(self, n):
        self.n = n

    def compute(self):
        return self.n


class ObjectSet:
    def __init__(self, type_name, where=None, order=None, origin=None, single=False):
        self._t = type_name
        self._where = where
        self._order = order or []
        self._origin = origin
        self._single = single

    def where(self, *preds):
        irs = ([self._where] if self._where else []) + [p.ir for p in preds]
        w = irs[0] if len(irs) == 1 else ["and"] + irs
        return ObjectSet(self._t, w, self._order, self._origin, self._single)

    def order_by(self, *orders):
        os = [[o.attr, o.direction] if isinstance(o, _Order) else [o.attr, "asc"] for o in orders]
        return ObjectSet(self._t, self._where, self._order + os, self._origin, self._single)

    def _request(self, limit):
        return {"op": "search", "type": self._t, "where": self._where,
                "order": self._order or None, "limit": limit, "origin": self._origin}

    def _rows(self, limit=None):
        if self._origin is not None and self._origin.get("id") is None:
            return []
        return [_object(self._t, r) for r in _fetch(self._request(limit))]

    def iterate(self):
        return iter(self._rows())

    def __iter__(self):
        return self.iterate()

    def all(self):
        return self._rows()

    def take(self, n):
        return self._rows(n)

    def first(self):
        rows = self._rows(1)
        return rows[0] if rows else None

    def count(self):
        return _Count(len(self._rows()))

    def is_empty(self):
        return not self._rows(1)

    def get(self, pk=None):
        if pk is None:
            if not self._single:
                raise TypeError("get() needs a primary key on a multi-link or object set")
            return self.first()
        if self._origin is None and self._where is None:
            row = _fetch({"op": "get", "type": self._t, "pk": _wire(pk)})
            return _object(self._t, row) if row else None
        for o in self._rows():
            if o._pk() == _wire(pk):
                return o
        return None


def _object(type_name, row):
    if row is None:
        return None
    o = object.__new__(object_class(type_name))
    object.__setattr__(o, "_id", row.get("id"))
    object.__setattr__(o, "_raw", row.get("values") or {})
    return o


class _Object:
    _t = None

    def _pk(self):
        pk = _type(self._t)["pk"]
        return self._raw.get(pk) if pk else self._id

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        t = _type(self._t)
        if name in t["props"]:
            return _from_wire(self._raw.get(t["props"][name]))
        if name in t["links"]:
            ln = t["links"][name]
            return ObjectSet(ln["far"], origin={"type": self._t, "id": self._id, "link": ln["link"]},
                             single=not ln["many"])
        raise AttributeError(t["api"] + " has no property or link " + repr(name))

    def __setattr__(self, name, value):
        raise TypeError(_type(self._t)["api"] + " objects are read-only; edit them through "
                        "PlatformClient().ontology.edits().objects." + _type(self._t)["api"] + ".edit(obj)")

    def __eq__(self, other):
        return isinstance(other, (_Object, _Editable)) and other._t == self._t and other._pk() == self._pk()

    def __hash__(self):
        return hash((self._t, str(self._pk())))

    def __repr__(self):
        return _type(self._t)["api"] + "(" + repr(self._pk()) + ")"


class OntologyEdit:
    def __init__(self, record):
        self.record = record

    def __repr__(self):
        return "OntologyEdit(" + repr(self.record) + ")"


class _State:
    def __init__(self, type_name, pk, obj_id=None, base=None, created=False):
        self.t = type_name
        self.pk = pk
        self.id = obj_id
        self.base = base
        self.created = created
        self.deleted = False
        self.values = {}

    def ref(self):
        return {"type": self.t, "pk": self.pk, "id": self.id}

    def raw(self):
        if self.base is None and not self.created:
            row = _fetch({"op": "get", "type": self.t, "pk": self.pk})
            if row is None:
                raise UserFacingError(_type(self.t)["api"] + " " + repr(self.pk) + " does not exist")
            self.id = row.get("id")
            self.base = row.get("values") or {}
        return dict(self.base or {}, **self.values)


class _EditableLinks:
    def __init__(self, edits, state, name):
        self._edits = edits
        self._state = state
        self._name = name
        self._ln = _type(state.t)["links"][name]

    def _record(self, op, target):
        if target is not None and not isinstance(target, (_Object, _Editable)):
            raise TypeError("a link takes an object, not " + repr(target))
        if target is not None and target._t != self._ln["far"]:
            raise TypeError(self._name + " links to " + _type(self._ln["far"])["api"] + ", not " +
                            _type(target._t)["api"])
        self._edits._links.append({"kind": "link", "op": op, "link": self._ln["link"],
                                   "source": self._state, "target": target})

    def add(self, obj):
        self._record("add", obj)

    def remove(self, obj):
        self._record("remove", obj)

    def set(self, obj):
        self._record("set", obj)

    def clear(self):
        self._record("clear", None)

    def _current(self):
        base = []
        if not self._state.created:
            self._state.raw()
            base = ObjectSet(self._ln["far"], origin={"type": self._state.t, "id": self._state.id,
                                                      "link": self._ln["link"]})._rows()
        keys = [(o._t, o._pk()) for o in base]
        objs = list(base)
        for e in self._edits._links:
            if e["source"] is not self._state or e["link"] != self._ln["link"]:
                continue
            if e["op"] in ("set", "clear"):
                keys, objs = [], []
            if e["op"] in ("add", "set"):
                k = (e["target"]._t, e["target"]._pk())
                if k not in keys:
                    keys.append(k)
                    objs.append(e["target"])
            if e["op"] == "remove":
                k = (e["target"]._t, e["target"]._pk())
                objs = [o for o, kk in zip(objs, keys) if kk != k]
                keys = [kk for kk in keys if kk != k]
        return objs

    def __iter__(self):
        return iter(self._current())

    def iterate(self):
        return iter(self._current())

    def all(self):
        return self._current()

    def get(self, pk=None):
        cur = self._current()
        if pk is None:
            return cur[0] if cur else None
        return next((o for o in cur if o._pk() == _wire(pk)), None)


class _Editable:
    def __init__(self, edits, state):
        object.__setattr__(self, "_edits", edits)
        object.__setattr__(self, "_state", state)
        object.__setattr__(self, "_t", state.t)

    def _pk(self):
        return self._state.pk

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        t = _type(self._t)
        if name in t["props"]:
            v = _from_wire(self._state.raw().get(t["props"][name]))
            return tuple(v) if isinstance(v, list) else v
        if name in t["links"]:
            return _EditableLinks(self._edits, self._state, name)
        raise AttributeError(t["api"] + " has no property or link " + repr(name))

    def __setattr__(self, name, value):
        t = _type(self._t)
        attr = t["props"].get(name)
        if attr is None:
            raise AttributeError(t["api"] + " has no property " + repr(name))
        if self._state.deleted:
            raise UserFacingError(t["api"] + " " + repr(self._state.pk) + " was deleted in this function")
        if attr == t["pk"] and not self._state.created:
            raise UserFacingError("the primary key of an existing " + t["api"] + " cannot be modified")
        self._state.values[attr] = _wire(list(value) if isinstance(value, tuple) else value)

    def __eq__(self, other):
        return isinstance(other, (_Object, _Editable)) and other._t == self._t and other._pk() == self._pk()

    def __hash__(self):
        return hash((self._t, str(self._pk())))

    def __repr__(self):
        return "Editable" + _type(self._t)["api"] + "(" + repr(self._pk()) + ")"


class _TypeEdits:
    def __init__(self, edits, type_name):
        self._edits = edits
        self._t = type_name

    def _state(self, obj_or_pk, create=False):
        if isinstance(obj_or_pk, _Editable):
            if obj_or_pk._t != self._t:
                raise TypeError("expected a " + _type(self._t)["api"])
            return obj_or_pk._state
        if isinstance(obj_or_pk, _Object):
            if obj_or_pk._t != self._t:
                raise TypeError("expected a " + _type(self._t)["api"] + ", got " + _type(obj_or_pk._t)["api"])
            k = (self._t, str(obj_or_pk._pk()))
            if k not in self._edits._states:
                self._edits._add(k, _State(self._t, obj_or_pk._pk(), obj_or_pk._id, obj_or_pk._raw))
            return self._edits._states[k]
        pk = _wire(obj_or_pk)
        k = (self._t, str(pk))
        if k in self._edits._states:
            s = self._edits._states[k]
            if create and not s.deleted:
                raise UserFacingError(_type(self._t)["api"] + " " + repr(pk) + " already exists in this function")
            return s
        self._edits._add(k, _State(self._t, pk, created=create))
        return self._edits._states[k]

    def edit(self, obj_or_pk):
        return _Editable(self._edits, self._state(obj_or_pk))

    def create(self, pk, **props):
        s = self._state(pk, create=True)
        e = _Editable(self._edits, s)
        pk_attr = _type(self._t)["pk"]
        if pk_attr:
            s.values[pk_attr] = s.pk
        for k, v in props.items():
            setattr(e, k, v)
        return e

    def delete(self, obj_or_pk):
        self._state(obj_or_pk).deleted = True


class _EditNamespace:
    def __init__(self, edits):
        self._edits = edits

    def __getattr__(self, api):
        name = _by_api(api)
        if name is None:
            raise AttributeError("no object type " + repr(api))
        return _TypeEdits(self._edits, name)


class OntologyEdits:
    def __init__(self):
        self._states = {}
        self._order = []
        self._links = []
        self.objects = _EditNamespace(self)

    def _add(self, k, state):
        self._states[k] = state
        self._order.append(k)

    def get_edits(self):
        states = [self._states[k] for k in self._order]
        gone = {id(s) for s in states if s.created and s.deleted}
        out = []
        for s in states:
            if s.created and not s.deleted:
                out.append({"kind": "create", "type": s.t, "pk": s.pk, "values": dict(s.values)})
        for s in states:
            if not s.created and not s.deleted and s.values:
                out.append({"kind": "modify", "type": s.t, "pk": s.pk, "id": s.id, "values": dict(s.values)})
        for e in self._links:
            t = e["target"]
            if id(e["source"]) in gone or (isinstance(t, _Editable) and id(t._state) in gone):
                continue
            target = None
            if isinstance(t, _Editable):
                target = t._state.ref()
            elif isinstance(t, _Object):
                target = {"type": t._t, "pk": t._pk(), "id": t._id}
            out.append({"kind": "link", "op": e["op"], "link": e["link"],
                        "source": e["source"].ref(), "target": target})
        for s in states:
            if s.deleted and not s.created:
                out.append({"kind": "delete", "type": s.t, "pk": s.pk, "id": s.id})
        return [OntologyEdit(r) for r in out]


class _Struct(dict):
    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError:
            raise AttributeError(name)


def _decode_arg(v):
    if isinstance(v, dict):
        if "$object" in v:
            o = v["$object"]
            return _object(o["type"], o)
        return _Struct({k: _decode_arg(x) for k, x in v.items()})
    if isinstance(v, list):
        return [_decode_arg(x) for x in v]
    return _from_wire(v)


def _type_names(types):
    names = []
    for t in types or []:
        n = getattr(t, "_t", None) or _by_api(getattr(t, "__name__", str(t)))
        if n is None:
            raise UserFacingError("@function(edits=[...]) names " + repr(t) + ", which is not an object type")
        names.append(n)
    return names


def _run(payload):
    namespace = {"__name__": "function"}
    with contextlib.redirect_stdout(sys.stderr):
        exec(compile(payload["source"], "function.py", "exec"), namespace)
    fn = namespace.get(payload["entry"])
    if fn is None or not getattr(fn, "_sutra_function", False):
        return {"error": {"message": "no @function-decorated function named " + repr(payload["entry"])}}
    args = {k: _decode_arg(v) for k, v in (payload.get("args") or {}).items()}
    with contextlib.redirect_stdout(sys.stderr):
        edits = fn(**args)
    if not isinstance(edits, list) or not all(isinstance(e, OntologyEdit) for e in edits):
        return {"error": {"message": payload["entry"] + " must return list[OntologyEdit] "
                          "(return ontology_edits.get_edits())"}}
    return {"edits": [e.record for e in edits],
            "declared": _type_names(getattr(fn, "_sutra_edits", None))}


def _main():
    payload = json.load(sys.stdin)
    _MANIFEST.clear()
    _MANIFEST.update(payload["manifest"])
    _CACHE.clear()
    for request, response in payload.get("cache") or []:
        _CACHE[_key(request)] = response
    out = sys.stdout
    try:
        result = _run(payload)
    except NeedData as need:
        result = {"needs": [need.request]}
    except UserFacingError as e:
        result = {"error": {"user_facing": str(e)}}
    except Exception as e:
        traceback.print_exc()
        result = {"error": {"message": type(e).__name__ + ": " + str(e)}}
    json.dump(result, out, default=str)
    out.flush()

