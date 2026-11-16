from sutra_platform_sdk.generate import generate
from sutra_platform_sdk.predicates import Property
from conftest import SCHEMA, SELF_LINK_SCHEMA
from sutra_platform_sdk.schema import fingerprint, sides


def test_generate_source(tmp_path):
    out = tmp_path / "gen_sdk.py"
    src = generate(SCHEMA, str(out), ontology="conrad10")
    assert "class File(OntologyObject):" in src
    assert "__sutra_type__ = 'file'" in src
    assert "kind = Property('file', 'kind', 'keyword')" in src
    assert "evidence_for = Link('file', 'evidence-for', 'support-case')" in src
    assert "def register_all(client, *, verify_fingerprint: bool = True):" in src
    compile(src, str(out), "exec")


def test_generated_classes_work(tmp_path):
    src = generate(SCHEMA, str(tmp_path / "g.py"), ontology="c10")
    ns: dict = {}
    exec(src, ns)
    File = ns["File"]
    assert File.__sutra_type__ == "file"
    assert File.__primary_key__ == "hash"
    assert isinstance(File.object_type.kind, Property)
    assert (File.object_type.kind == "trace").to_where() == {"kind": {"=": "trace"}}
    assert "File" in [c.__name__ for c in ns["OBJECT_TYPES"]]


def test_fingerprint_agrees_with_the_clojure_generator():
    assert fingerprint(SCHEMA) == "087bde78ba579cb8"
    assert fingerprint(SELF_LINK_SCHEMA) == "70a66b6f9bde860c"


def test_renaming_a_side_moves_the_fingerprint():
    import copy
    renamed = copy.deepcopy(SELF_LINK_SCHEMA)
    renamed["link-types"]["ri.sutra.main.link-types.hp"]["reverse-api-name"] = "unterprojekte"
    assert fingerprint(renamed) != fingerprint(SELF_LINK_SCHEMA)


def test_every_side_is_indexed_on_its_near_type():
    assert sides(SCHEMA, "ri.sutra.main.object-types.bbb") == {
        "case-customer": {"link_rid": "ri.sutra.main.link-types.e2",
                          "link_type_name": "case-customer",
                          "far_type": "person", "cardinality": "one"},
        "evidence-for": {"link_rid": "ri.sutra.main.link-types.e1",
                         "link_type_name": "evidence-for",
                         "far_type": "file", "cardinality": "many"},
    }


def test_a_self_link_generates_two_accessors(tmp_path, fake_http):
    src = generate(SELF_LINK_SCHEMA, str(tmp_path / "p.py"), ontology="p")
    assert "hauptprojekt = Link('projekt', 'hauptprojekt', 'projekt')" in src
    assert "projekte = Link('projekt', 'projekte', 'projekt')" in src
    assert "_incoming" not in src
    ns: dict = {}
    exec(src, ns)
    Projekt = ns["Projekt"]
    obj = Projekt(None, "projekt", "p1", {"name": "x"})
    sent = []

    class Http:
        def post(self, path, body):
            sent.append(body)
            return {"object-type": "projekt", "rows": [{"id": "p0", "values": {}}]}

    class C:
        _http = Http()
    object.__setattr__(obj, "_client", C())
    assert obj.hauptprojekt.id == "p0"
    obj.projekte
    assert [b["around"] for b in sent] == [["hauptprojekt"], ["projekte"]]
    assert all(b["ids"] == ["p1"] and b["find"] == "projekt" for b in sent)


def test_a_self_link_without_side_names_generates_no_accessor(tmp_path, caplog):
    import copy
    bare = copy.deepcopy(SELF_LINK_SCHEMA)
    lt = bare["link-types"]["ri.sutra.main.link-types.hp"]
    del lt["forward-api-name"], lt["reverse-api-name"]
    src = generate(bare, str(tmp_path / "b.py"), ontology="b")
    assert "hauptprojekt_projekte" not in src
    assert "two sides share the name" in caplog.text
