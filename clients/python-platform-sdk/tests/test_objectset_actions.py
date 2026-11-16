from sutra_platform_sdk.predicates import Property


def test_build_query(client):
    os = client.ontology.objects.File.where(Property("file", "kind") == "trace")
    assert os._build() == {"find": "file", "where": {"kind": {"=": "trace"}}}


def test_dynamic_type_name(client):
    os = client.ontology.objects.SupportCase
    assert os._type == "support-case"


def test_iterate_hydrates(client):
    objs = client.ontology.objects.File.all()
    assert len(objs) == 3
    assert objs[0].title == "frank_trace.PML"
    assert objs[0].kind == "trace"
    assert objs[0].id == "f1"


def test_order_by_desc(client):
    objs = client.ontology.objects.File.order_by(Property("file", "size"), desc=True).all()
    sizes = [o.values["size"] for o in objs]
    assert sizes == sorted(sizes, reverse=True)


def test_count(client):
    assert client.ontology.objects.File.count() == 3


def test_get_by_primary_key(client):
    o = client.ontology.objects.File.get("f2")
    assert o is not None and o.id == "f2"


def test_action_envelope(client, fake_http):
    client.ontology.actions.create_file(values={"name": "x", "kind": "trace"})
    method, path, body = fake_http.calls[-1]
    assert method == "POST" and path == "/api/actions?publish=true"
    assert body == {"action": "create-file",
                    "args": {"values": {"name": "x", "kind": "trace"}}}


def test_action_stages_only_when_asked(client, fake_http):
    client.ontology.actions.resolve_case(case="cid", publish=False)
    assert fake_http.calls[-1][1] == "/api/actions"


def test_action_publish_and_naming(client, fake_http):
    client.ontology.actions.resolve_case(case="cid", publish=True)
    method, path, body = fake_http.calls[-1]
    assert path == "/api/actions?publish=true"
    assert body == {"action": "resolve-case", "args": {"case": "cid"}}


def test_action_takes_objects_as_arguments(client, fake_http):
    f1, f2 = client.ontology.objects.File.all()[:2]
    client.ontology.actions.resolve_case(case=f1, related=[f1, f2], publish=True)
    assert fake_http.calls[-1][2]["args"] == {"case": "f1", "related": ["f1", "f2"]}



def test_branch_context_scopes_each_request_not_the_session(client, fake_http):
    fake_http.branch = None
    seen = []
    post = fake_http.post
    fake_http.post = lambda path, body: seen.append((path, fake_http.branch)) or post(path, body)
    with client.branch("fr02"):
        client.ontology.actions.resolve_case(case="cid", publish=True)
    client.ontology.actions.resolve_case(case="cid", publish=True)
    assert seen == [("/api/actions?publish=true", "fr02"), ("/api/actions?publish=true", None)]
    assert not any("checkout" in p for _, p, _ in fake_http.calls)


def test_branch_context_restores_on_error(client, fake_http):
    fake_http.branch = "outer"
    try:
        with client.branch("fr02"):
            raise RuntimeError("boom")
    except RuntimeError:
        pass
    assert fake_http.branch == "outer"


def test_transport_appends_branch_after_an_existing_query():
    from sutra_platform_sdk.transport import Http
    h = Http("http://x")
    assert h._url("/api/actions?publish=true") == "http://x/api/actions?publish=true"
    h.branch = "fr 02"
    assert h._url("/api/actions?publish=true") == "http://x/api/actions?publish=true&branch=fr%2002"
    assert h._url("/api/me") == "http://x/api/me?branch=fr%2002"
