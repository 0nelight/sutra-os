from sutra_platform_sdk import paths, transport


def test_every_endpoint_is_addressable():
    assert len(paths.ENDPOINTS) > 200
    for key, ep in paths.ENDPOINTS.items():
        assert ep["path"].startswith("/api/"), key
        for name in ep.get("params", ()):
            assert "{" + name + "}" in ep["path"], key
    assert paths.PATHS.keys() == paths.ENDPOINTS.keys()


def test_url_percent_encodes_a_segment_value():
    assert paths.url("objects-of", type="email") == "/api/objects/email"
    branch = "scenario/rid/uuid.name"
    assert paths.url("branch-roles", name=branch) == (
        "/api/branches/scenario%2Frid%2Fuuid.name/roles"
    )


def test_url_names_the_missing_param():
    try:
        paths.url("objects-of")
    except KeyError as exc:
        assert "type" in str(exc)
    else:
        raise AssertionError("expected a KeyError")


def test_query_string_drops_none_and_empties():
    assert paths.query_string(None) == ""
    assert paths.query_string({"limit": None}) == ""
    assert paths.query_string({"limit": 10, "offset": 5}) == "?limit=10&offset=5"
    assert paths.query_string({"q": "a b"}) == "?q=a%20b"


def test_declared_query_params_match_the_ontology_reads():
    assert paths.ENDPOINTS["objects-of"]["query"] == ["limit", "offset"]
    assert paths.ENDPOINTS["ontology-query"]["query"] == ["accuracy", "engine", "explain"]


def test_encoding_is_declared_not_prefix_guessed():
    assert paths.encoding_for("ontology-query") == "keywordize-query"
    assert paths.encoding_for("actions") == "keywordize-action"
    assert paths.encoding_for("function-run") == "keywordize-function"
    assert paths.encoding_for("schema") == "generic"
    assert paths.encoding_for_path("/api/ontology/query") == "keywordize-query"
    assert paths.encoding_for_path("/api/blob/deadbeef") == "bytes"
    assert paths.encoding_for_path("/api/schema") == "generic"


def test_transport_dispatches_through_the_generated_table():
    http = transport.Http("http://localhost")
    assert http._encoder("/api/ontology/query") is transport.encode_query
    assert http._encoder("/api/actions") is transport.encode_action
    assert http._encoder("/api/function") is transport.encode_function
    assert http._encoder("/api/schema") is transport.encode_generic


def test_server_facts_ride_along():
    assert paths.ENDPOINTS["login-audit"]["auth"] == "admin"
    assert paths.ENDPOINTS["actions"]["methods"] == ["post"]
    assert paths.ENDPOINTS["actions"]["request"] == "sutra.foundation.contracts/action-wire"
    assert paths.ENDPOINTS["objects-of"]["service"] == "oss"
