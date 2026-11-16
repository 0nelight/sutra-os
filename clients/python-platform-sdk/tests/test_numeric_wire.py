from decimal import Decimal

import pytest

pytest.importorskip("transit")

from sutra_platform_sdk.transport import _decode, _encode, from_wire


def rt(v):
    return from_wire(_decode(_encode([v])))[0]


def test_a_decimal_arrives_as_decimal_not_float():
    for text in ("2.718281828", "12345678901234567.89", "10.00", "0.1"):
        got = rt(Decimal(text))
        assert isinstance(got, Decimal)
        assert got == Decimal(text)
        assert str(got) == text


def test_a_long_past_2_53_keeps_every_digit_as_an_int():
    for n in (58319870951433, 9007199254740993, -9007199254740993,
              99999999999999999999):
        got = rt(n)
        assert isinstance(got, int)
        assert got == n


def test_the_tagged_forms_the_server_writes_decode_to_exact_python():
    assert from_wire(_decode(b'["~f2.718281828"]')) == [Decimal("2.718281828")]
    assert from_wire(_decode(b'["~n58319870951433"]')) == [58319870951433]
    assert from_wire(_decode(b'["~i58319870951433"]')) == [58319870951433]


def test_a_decimal_goes_OUT_tagged_so_an_action_argument_stays_exact():
    assert _encode([Decimal("2.718281828")]) == b'["~f2.718281828"]'
    assert _encode([Decimal("10.00")]) == b'["~f10.00"]'


def test_a_string_stays_a_string():
    got = rt("58319870951433")
    assert isinstance(got, str)
    assert got == "58319870951433"
