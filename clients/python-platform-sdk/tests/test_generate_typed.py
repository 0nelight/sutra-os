import copy
import decimal
import inspect

import pytest

from conftest import SCHEMA
from sutra_platform_sdk.generate import generate, py_type
from sutra_platform_sdk.schema import action_params, fingerprint


@pytest.fixture
def generated():
    ns: dict = {}
    exec(generate(SCHEMA, None, ontology="c10"), ns)
    return ns


def test_required_params_come_first_and_have_no_default():
    names = [n for n, _ in action_params(SCHEMA["actions"]["ri.sutra.main.actions.cf"])]
    assert names == ["name", "kind", "size", "tags"]


def test_action_signature_is_typed(generated):
    sig = inspect.signature(generated["TypedActions"].create_file)
    assert list(sig.parameters) == ["self", "name", "kind", "size", "tags",
                                    "publish", "action_config"]
    assert sig.parameters["name"].default is inspect.Parameter.empty
    assert sig.parameters["kind"].default is None
    assert sig.parameters["tags"].annotation == "list[str] | None"
    assert sig.parameters["size"].annotation == "int | None"


def test_a_reference_param_names_the_object_type_it_takes(generated):
    sig = inspect.signature(generated["TypedActions"].resolve_case)
    assert sig.parameters["case"].annotation == "SupportCase | str"


def test_typed_action_sends_only_the_given_args(client, fake_http, generated):
    generated["register_all"](client)
    client.ontology.actions.create_file(name="x", kind="trace")
    _, path, body = fake_http.calls[-1]
    assert path == "/api/actions?publish=true"
    assert body == {"action": "create-file", "args": {"name": "x", "kind": "trace"}}


def test_missing_required_arg_fails_before_the_request(client, generated):
    generated["register_all"](client)
    with pytest.raises(TypeError):
        client.ontology.actions.create_file(kind="trace")


def test_typed_function_uses_its_declared_params(client, fake_http, generated):
    generated["register_all"](client)
    client.ontology.functions.schedule_maintenance(aircraft="D-TEST")
    _, path, body = fake_http.calls[-1]
    assert path == "/api/function"
    assert body == {"function-name": "schedule-maintenance",
                    "inputs": {"aircraft": "D-TEST"}}


def test_an_untyped_function_keeps_a_passthrough_signature(generated):
    sig = inspect.signature(generated["TypedFunctions"].untyped_fn)
    assert list(sig.parameters) == ["self", "inputs"]


def test_fingerprint_is_stable_and_shape_sensitive():
    assert fingerprint(SCHEMA) == fingerprint(dict(SCHEMA))
    renamed = {**SCHEMA, "object-types": {
        rid: ({**spec, "object-type-name": "datei"} if spec["object-type-name"] == "file" else spec)
        for rid, spec in SCHEMA["object-types"].items()}}
    assert fingerprint(renamed) != fingerprint(SCHEMA)


def test_an_added_property_is_drift_too_but_a_reordering_is_not():
    added = {**SCHEMA, "object-types": {
        rid: ({**spec, "attribute-list": spec["attribute-list"] + [
            {"attribute-name": "mtime", "attribute-type": "timestamp"}]}
            if spec["object-type-name"] == "file" else spec)
        for rid, spec in SCHEMA["object-types"].items()}}
    assert fingerprint(added) != fingerprint(SCHEMA)
    reordered = {**SCHEMA, "object-types": dict(reversed(list(SCHEMA["object-types"].items())))}
    assert fingerprint(reordered) == fingerprint(SCHEMA)


def test_register_all_refuses_a_drifted_ontology(client, generated, fake_http):
    fake_http._schema = {**SCHEMA, "actions": {}}
    with pytest.raises(RuntimeError, match="ontology drift"):
        generated["register_all"](client)
    generated["register_all"](client, verify_fingerprint=False)


def test_generated_module_compiles_and_stamps_its_fingerprint():
    src = generate(SCHEMA, None, ontology="c10")
    compile(src, "<generated>", "exec")
    assert f"__ontology_fingerprint__ = {fingerprint(SCHEMA)!r}" in src


def test_numeric_annotations_follow_the_language_table():
    assert [py_type(t) for t in ("byte", "short", "integer", "long")] == ["int"] * 4
    assert py_type("decimal") == "Decimal"
    assert py_type("number") == "Decimal"
    assert [py_type(t) for t in ("float", "double")] == ["float", "float"]
    assert py_type(["decimal", 6, 3]) == "Decimal"


def test_a_decimal_param_annotation_resolves_inside_the_generated_module():
    schema = copy.deepcopy(SCHEMA)
    schema["actions"]["ri.sutra.main.actions.cf"]["params"]["preis"] = {"type": "decimal"}
    ns: dict = {}
    exec(generate(schema, None, ontology="c10"), ns)
    sig = inspect.signature(ns["TypedActions"].create_file)
    assert sig.parameters["preis"].annotation == "Decimal | float | None"
    assert ns["Decimal"] is decimal.Decimal


def test_the_model_reads_as_classes_attributes_and_methods(client, fake_http, generated):
    generated["register_all"](client)
    File, SupportCase = generated["File"], generated["SupportCase"]
    f = File.get("h2")
    assert isinstance(f, File) and f.kind == "screenshot" and f.size == 50
    assert File.where(File.kind == "trace").count() == 2
    case = SupportCase(client, "support-case", "c1", {"name": "x"})
    assert {type(e) for e in case.evidence_for} == {File}
    case.resolve_case(publish=True)
    _, path, body = fake_http.calls[-1]
    assert path == "/api/actions?publish=true"
    assert body == {"action": "resolve-case", "args": {"case": "c1"}}


def test_an_action_binds_to_its_declared_on_type_only(generated):
    assert not hasattr(generated["File"], "resolve_case")
    assert not hasattr(generated["File"], "create_file")
