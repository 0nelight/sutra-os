import pytest

from sutra_platform_sdk import transport

rc = pytest.importorskip("transit.rolling_cache")


def test_clear_empties_both_tables():
    c = rc.RollingCache()
    c.encache("~:a-name")
    assert c.key_to_value and c.value_to_key
    c.clear()
    assert c.key_to_value == {} and c.value_to_key == {}


def test_cache_resets_at_exactly_cache_size():
    c = rc.RollingCache()
    for i in range(rc.CACHE_SIZE):
        c.encache(f"~:name-{i}")
    assert c.size() == rc.CACHE_SIZE
    assert c.is_cache_full()
    c.encache("~:one-too-many")
    assert c.size() == 1, "the cache must roll over, not run past its code range"


def test_every_minted_code_stays_in_the_two_character_range():
    c = rc.RollingCache()
    for i in range(rc.CACHE_SIZE * 2):
        c.encache(f"~:name-{i}")
        for code in c.key_to_value:
            assert len(code) <= 3, code
            assert rc.decode_key(code) < rc.CACHE_SIZE
