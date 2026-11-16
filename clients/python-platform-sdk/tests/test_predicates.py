from sutra_platform_sdk.predicates import DynamicProps, Link, Property


def test_eq_predicate():
    p = Property("file", "kind") == "trace"
    assert p.to_where() == {"kind": {"=": "trace"}}


def test_operators():
    assert (Property("file", "size") > 5).to_where() == {"size": {">": 5}}
    assert (Property("file", "size") <= 9).to_where() == {"size": {"<=": 9}}
    assert Property("file", "kind").ne("x").to_where() == {"kind": {"not=": "x"}}
    assert Property("file", "name").starts_with("BA").to_where() == {"name": {"starts-with": "BA"}}
    assert Property("file", "name").contains("DLL").to_where() == {"name": {"contains": "DLL"}}


def test_and_merges():
    p = (Property("file", "kind") == "trace") & (Property("file", "size") > 5)
    assert p.to_where() == {"kind": {"=": "trace"}, "size": {">": 5}}


def test_link_nesting():
    sym = Property("support-case", "symptom").contains("DLL")
    p = Link("file", "evidence-for", "support-case")(sym)
    assert p.to_where() == {"evidence-for": {"symptom": {"contains": "DLL"}}}


def test_dynamic_props():
    props = DynamicProps("file")
    assert (props.ba_nr == "BA24").to_where() == {"ba-nr": {"=": "BA24"}}
