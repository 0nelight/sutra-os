import tomllib
from pathlib import Path

import pytest

from conftest import SCHEMA
from sutra_platform_sdk.package import dist_name, write_package
from sutra_platform_sdk.schema import fingerprint


def test_package_tree_is_installable_shaped(tmp_path):
    manifest = write_package(SCHEMA, str(tmp_path), name="beschaffung_sdk",
                             ontology="c10")
    assert (tmp_path / "pyproject.toml").exists()
    assert (tmp_path / "beschaffung_sdk" / "__init__.py").exists()
    meta = tomllib.loads((tmp_path / "pyproject.toml").read_text())
    assert meta["project"]["name"] == "beschaffung-sdk"
    assert meta["project"]["dependencies"] == ["sutra-platform-sdk>=0.2.0"]
    assert meta["tool"]["setuptools"]["packages"] == ["beschaffung_sdk"]
    assert manifest["dist"] == "beschaffung-sdk"


def test_the_version_carries_the_fingerprint(tmp_path):
    manifest = write_package(SCHEMA, str(tmp_path), name="s", ontology="c10")
    fp = fingerprint(SCHEMA)
    assert manifest["version"] == f"0.1.0+{fp}"
    meta = tomllib.loads((tmp_path / "pyproject.toml").read_text())
    assert meta["project"]["version"].endswith(f"+{fp}")


def test_the_generated_package_module_runs(tmp_path, client):
    write_package(SCHEMA, str(tmp_path), name="pkg_sdk", ontology="c10")
    source = (tmp_path / "pkg_sdk" / "__init__.py").read_text()
    ns: dict = {}
    exec(compile(source, "pkg_sdk/__init__.py", "exec"), ns)
    ns["register_all"](client)
    assert client.ontology.objects.File.count() == 3
    assert callable(ns["client"])


def test_a_package_name_must_be_importable(tmp_path):
    with pytest.raises(ValueError, match="not a valid Python package name"):
        write_package(SCHEMA, str(tmp_path), name="beschaffung-sdk")


def test_dist_name_normalises_like_pep503():
    assert dist_name("beschaffung_sdk") == "beschaffung-sdk"
    assert dist_name("A.B_C") == "a-b-c"
