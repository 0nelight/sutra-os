from __future__ import annotations


def object_types(schema: dict) -> dict:
    return schema.get("object-types") or {}


def link_types(schema: dict) -> dict:
    return schema.get("link-types") or {}


def name_to_rid(schema: dict) -> dict:
    return {spec["object-type-name"]: rid for rid, spec in object_types(schema).items()}


def rid_to_name(schema: dict) -> dict:
    return {rid: spec["object-type-name"] for rid, spec in object_types(schema).items()}


def link_index(schema: dict) -> dict:
    names = rid_to_name(schema)
    out = {}
    for rid, spec in link_types(schema).items():
        out[spec["link-type-name"]] = {
            "rid": rid,
            "from": names.get(spec.get("from-object-type"), spec.get("from-object-type")),
            "to": names.get(spec.get("to-object-type"), spec.get("to-object-type")),
            "cardinality": spec.get("cardinality"),
            "forward": side_api_name(rid, spec, "to"),
            "reverse": side_api_name(rid, spec, "from"),
        }
    return out


def side_api_name(rid: str, spec: dict, direction: str) -> str:
    own = spec.get("forward-api-name" if direction == "to" else "reverse-api-name")
    return token(own or spec.get("link-type-name") or rid)


_FAR_ONE = {"to": {"many-to-one", "one-to-one"}, "from": {"one-to-many", "one-to-one"}}


def _side_entries(schema: dict, type_rid: str) -> list:
    spec = object_types(schema).get(type_rid) or {}
    names = rid_to_name(schema)
    me = {type_rid, spec.get("object-type-name")} | set(spec.get("implements") or [])
    out = []
    for rid, lt in link_types(schema).items():
        for direction, near, far in (("to", "from-object-type", "to-object-type"),
                                     ("from", "to-object-type", "from-object-type")):
            if lt.get(near) not in me:
                continue
            out.append((side_api_name(rid, lt, direction), {
                "link_rid": rid,
                "link_type_name": lt.get("link-type-name"),
                "far_type": names.get(lt.get(far), lt.get(far)),
                "cardinality": ("one" if token(lt.get("cardinality")) in _FAR_ONE[direction]
                                else "many"),
            }))
    return out


def ambiguous_sides(schema: dict, type_rid: str) -> list:
    seen: dict = {}
    for name, _ in _side_entries(schema, type_rid):
        seen[name] = seen.get(name, 0) + 1
    return sorted(n for n, c in seen.items() if c > 1)


def sides(schema: dict, type_rid: str) -> dict:
    bad = set(ambiguous_sides(schema, type_rid))
    return dict(sorted((n, info) for n, info in _side_entries(schema, type_rid)
                       if n not in bad))


def attributes(type_spec: dict) -> list:
    return [(a["attribute-name"], a.get("attribute-type", "string"))
            for a in type_spec.get("attribute-list") or []]


def primary_key(type_spec: dict, backing: dict | None = None):
    name = token(type_spec.get("object-type-name"))
    pk = ((backing or {}).get("object-types") or {}).get(name, {}).get("primary-key")
    if pk:
        return token(pk)
    mk = type_spec.get("match-keys")
    return token(mk[0]) if mk else None


def is_required(spec) -> bool:
    return (spec or {}).get("required") in (True, "true")


def action_types(schema: dict) -> dict:
    names = rid_to_name(schema)
    out = {}
    for key, spec in (schema.get("actions") or {}).items():
        spec = dict(spec or {})
        name = token(spec.get("action-name") or spec.get("name") or key)
        on = token(spec.get("on")).lstrip(":")
        if on:
            spec["on"] = names.get(on, on)
        spec["params"] = {p: _named_param(ps, names)
                          for p, ps in (spec.get("params") or {}).items()}
        out[name] = spec
    return out


def _named_param(spec, names: dict) -> dict:
    spec = dict(spec or {})
    t = token(spec.get("type"))
    if t in names:
        spec["type"] = names[t]
    return spec


def action_params(action_spec: dict) -> list:
    params = (action_spec or {}).get("params") or {}
    items = list(params.items())
    return sorted(items, key=lambda kv: (not is_required(kv[1]), kv[0]))


def function_name(spec: dict):
    return (spec or {}).get("function-name") or (spec or {}).get("name")


def functions(schema: dict) -> list:
    out = list((schema.get("functions") or {}).values())
    out += [t for t in (schema.get("transforms") or {}).values() if function_name(t)]
    return out


def function_params(fn_spec: dict) -> list:
    spec = fn_spec or {}
    raw = spec.get("param-types") or spec.get("inputs") or {}
    return sorted((k, v.get("kind") if isinstance(v, dict) else v)
                  for k, v in raw.items())


def token(value) -> str:
    if value is None:
        return ""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (list, tuple)):
        return token(value[0]) if value else ""
    return str(value)


def fingerprint(schema: dict) -> str:
    import hashlib

    parts = []
    for spec in sorted(object_types(schema).values(),
                       key=lambda s: token(s.get("object-type-name"))):
        attrs = ",".join(f"{token(n)}:{token(t)}" for n, t in attributes(spec))
        keys = ",".join(token(k) for k in spec.get("match-keys") or [])
        parts.append(f"O|{token(spec.get('object-type-name'))}|{attrs}|{keys}")
    for name, info in sorted(link_index(schema).items(), key=lambda kv: token(kv[0])):
        parts.append(f"L|{token(name)}|{token(info['from'])}|{token(info['to'])}"
                     f"|{token(info['cardinality'])}"
                     f"|{token(info['forward'])}|{token(info['reverse'])}")
    for name, spec in sorted(action_types(schema).items(), key=lambda kv: token(kv[0])):
        ps = ",".join(f"{token(p)}:{token((s or {}).get('type'))}"
                      f":{token(is_required(s))}"
                      for p, s in action_params(spec))
        parts.append(f"A|{token(name)}|{ps}")
    for spec in sorted(functions(schema), key=lambda s: token(function_name(s))):
        parts.append(f"F|{token(function_name(spec))}|"
                     + ",".join(f"{token(p)}:{token(t)}" for p, t in function_params(spec)))
    return hashlib.sha256("\n".join(parts).encode("utf-8")).hexdigest()[:16]
