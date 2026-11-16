import pytest

from sutra_platform_sdk.actions import ActionConfig, ActionMode, ReturnEditsMode


def test_apply_returns_no_edits_unless_asked(client, fake_http):
    r = client.ontology.actions.create_file(values={"name": "x", "kind": "trace"})
    assert fake_http.calls[-1][1] == "/api/actions?publish=true"
    assert r.edits is None and r.staged is True and r.seq == 1


def test_return_edits_names_what_was_created(client, fake_http):
    r = client.ontology.actions.create_file(
        values={"name": "x", "kind": "trace", "hash": "h9"},
        action_config=ActionConfig(return_edits=ReturnEditsMode.ALL))
    assert "return-edits=true" in fake_http.calls[-1][1]
    assert r.edits.added_objects_count == 1
    assert r.edits.modified_objects_count == 0
    edit = r.edits.edits[0]
    assert edit.type == "addObject"
    assert edit.object_id is not None


def test_validate_only_applies_nothing(client, fake_http):
    r = client.ontology.actions.create_file(
        values={},
        action_config=ActionConfig(mode=ActionMode.VALIDATE_ONLY))
    assert [p for _, p, _ in fake_http.calls if p.startswith("/api/actions")] \
        == ["/api/actions/params"]
    assert r.validation.result == "INVALID"
    assert r.validation.parameters["name"].required is True
    assert r.validation.parameters["name"].result == "INVALID"
    assert r.validation.parameters["kind"].result == "VALID"
    assert r.validation.submission_criteria


def test_validate_only_passes_a_complete_call(client):
    r = client.ontology.actions.create_file(
        values={}, name="x",
        action_config=ActionConfig(mode=ActionMode.VALIDATE_ONLY))
    assert r.validation.result == "VALID"
    assert r.validation.submission_criteria == ()


def test_batch_is_one_call_with_one_edit_summary(client, fake_http):
    rows = [{"values": {"name": f"f{i}", "kind": "trace"}} for i in range(3)]
    r = client.ontology.actions.batch_apply(
        "create-file", rows,
        action_config=ActionConfig(return_edits=ReturnEditsMode.ALL))
    method, path, body = fake_http.calls[-1]
    assert method == "POST" and path.startswith("/api/actions/batch")
    assert body["action"] == "create-file" and len(body["requests"]) == 3
    assert len(r.seqs) == 3
    assert r.edits.added_objects_count == 3
    assert len(r.edits.edits) == 3


def test_batch_body_keeps_one_request_per_row(client, fake_http):
    client.ontology.actions.batch_apply("resolve-case", [{"case": "c1"}, {"case": "c2"}])
    _, _, body = fake_http.calls[-1]
    assert body["requests"] == [{"case": "c1"}, {"case": "c2"}]


def test_snake_kwargs_still_become_kebab(client, fake_http):
    client.ontology.actions.batch_apply("resolve-case", [{"case_id": "c1"}])
    _, _, body = fake_http.calls[-1]
    assert body["requests"] == [{"case-id": "c1"}]
