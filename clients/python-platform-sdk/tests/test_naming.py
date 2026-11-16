from sutra_platform_sdk._naming import (kebab_to_pascal, kebab_to_snake,
                                pascal_to_kebab, snake_to_kebab)


def test_kebab_to_pascal():
    assert kebab_to_pascal("support-case") == "SupportCase"
    assert kebab_to_pascal("file") == "File"
    assert kebab_to_pascal("ba-nr") == "BaNr"


def test_snake_kebab_roundtrip():
    assert kebab_to_snake("ba-nr") == "ba_nr"
    assert snake_to_kebab("ba_nr") == "ba-nr"
    assert snake_to_kebab("evidence_for") == "evidence-for"


def test_pascal_to_kebab():
    assert pascal_to_kebab("SupportCase") == "support-case"
    assert pascal_to_kebab("File") == "file"
    assert pascal_to_kebab("HTTPServer").startswith("http")
