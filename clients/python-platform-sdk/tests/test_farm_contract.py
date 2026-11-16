import inspect
import json
import re
import shutil
import subprocess
from pathlib import Path

import pytest

from sutra_platform_sdk.generate import generate
from sutra_platform_sdk.transport import _decode, from_wire

FIXTURES = Path(__file__).parent / "fixtures"
SDK_ROOT = Path(__file__).resolve().parents[1]


@pytest.fixture(scope="module")
def farm_src():
    schema = from_wire(_decode((FIXTURES / "farm-schema.transit.json").read_bytes()))
    return generate(schema, ontology="farm")


@pytest.fixture(scope="module")
def farm(farm_src):
    ns: dict = {}
    exec(compile(farm_src, "farm_sdk.py", "exec"), ns)
    return ns


def _required(fn) -> list:
    return [n for n, p in inspect.signature(fn).parameters.items()
            if n != "self" and p.default is inspect.Parameter.empty]


def test_an_action_binds_to_its_type_by_name(farm):
    assert _required(farm["Lot"].sell_hay) == ["bales", "buyer", "sale_nr"]
    assert {"record_harvest", "sell_harvest", "record_payment"} <= set(vars(farm["Harvest"]))
    assert "plant" in vars(farm["Field"])
    sig = inspect.signature(farm["Harvest"].sell_harvest)
    assert sig.parameters["buyer"].annotation == "Buyer | str"
    assert sig.return_annotation == "SyncApplyActionResponse"


def test_a_lifecycle_becomes_a_literal(farm):
    assert farm["HarvestStatus"].__args__ == ("planted", "harvested", "delivered", "paid")


def test_links_and_properties_are_typed_descriptors(farm_src):
    assert "    sales: Many[Sale] = Many('lot', 'sales', 'sale')" in farm_src
    assert "    field: One[Field] = One('lot', 'field', 'field')" in farm_src
    assert "    in_barn: Attr[int, int] = Attr('lot', 'in-barn', 'integer')" in farm_src
    assert "    status: Attr[HarvestStatus | None, HarvestStatus] =" in farm_src


@pytest.mark.skipif(shutil.which("pyright") is None, reason="pyright not installed")
def test_the_readme_type_checks_and_its_mistakes_do_not(farm_src, tmp_path):
    (tmp_path / "farm_sdk.py").write_text(farm_src)
    shutil.copy(FIXTURES / "readme_typing.py", tmp_path)
    (tmp_path / "pyrightconfig.json").write_text(json.dumps({"extraPaths": [str(SDK_ROOT)]}))
    out = subprocess.run(["pyright", "--outputjson", "readme_typing.py"], cwd=tmp_path,
                         capture_output=True, text=True, timeout=120)
    found = {(d["range"]["start"]["line"] + 1, d.get("rule"))
             for d in json.loads(out.stdout)["generalDiagnostics"]
             if d["severity"] == "error"}
    lines = (FIXTURES / "readme_typing.py").read_text().splitlines()
    expected = {(i, m.group(1)) for i, line in enumerate(lines, 1)
                if (m := re.search(r"# expect: (\w+)", line))}
    assert found == expected
