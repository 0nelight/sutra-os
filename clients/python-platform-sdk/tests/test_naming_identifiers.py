import pytest

from sutra_platform_sdk._naming import py_identifier


@pytest.mark.parametrize("wire,expected", [
    ("anschrift-manuell?", "anschrift_manuell"),
    ("offen?", "offen"),
    ("normal-name", "normal_name"),
    ("class", "class_"),
    ("2fach", "_2fach"),
    ("???", None),
    ("", None),
])
def test_py_identifier(wire, expected):
    assert py_identifier(wire) == expected


def test_every_result_is_actually_an_identifier():
    for wire in ["a-b", "x?", "1", "if", "__", "ä-b?"]:
        out = py_identifier(wire)
        assert out is None or out.isidentifier(), (wire, out)
