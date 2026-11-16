import pytest

from sutra_platform_sdk.predicates import Property, TypeMismatch


def test_a_string_column_refuses_a_number():
    p = Property("angebot", "pro-ang-id", "string")
    with pytest.raises(TypeMismatch) as e:
        p == 100000
    assert "declared string" in str(e.value)
    assert (p == "100000").to_where() == {"pro-ang-id": {"=": "100000"}}


def test_a_numeric_column_refuses_a_string_and_a_bool():
    p = Property("adresse", "adr-id", "integer")
    for bad in ("12", True):
        with pytest.raises(TypeMismatch):
            p == bad
    assert (p == 12).to_where() == {"adr-id": {"=": 12}}


def test_a_float_column_takes_an_int():
    assert (Property("t", "a", "double") == 5).to_where() == {"a": {"=": 5}}


def test_string_operators_need_a_string_whatever_the_column():
    with pytest.raises(TypeMismatch):
        Property("angebot", "bezeichnung", "string").contains(5)


def test_null_and_unknown_types_are_never_refused():
    assert Property("angebot", "x", "string").is_null().to_where() == {"x": {"=": None}}
    assert (Property("t", "a", "marking") == 5).to_where() == {"a": {"=": 5}}
    assert (Property("t", "a") == 5).to_where() == {"a": {"=": 5}}
