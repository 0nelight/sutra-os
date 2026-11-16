import pytest

pytest.importorskip("fs", reason="PyFilesystem2 (fs) not installed")


def test_listdir_groups_by_axis(client):
    from sutra_platform_sdk import OntologyFS
    ofs = OntologyFS(client, axes=["kind"], type_name="file")
    assert ofs.listdir("/") == ["screenshot", "trace"]
    assert set(ofs.listdir("/trace")) == {"frank_trace.PML", "kesselmeier.PML"}
    assert ofs.listdir("/screenshot") == ["shot.png"]


def test_openbin_streams_blob(client):
    from sutra_platform_sdk import OntologyFS
    ofs = OntologyFS(client, axes=["kind"], type_name="file")
    with ofs.openbin("/trace/frank_trace.PML") as fh:
        assert fh.read() == b"BYTES"


def test_getinfo_file_size(client):
    from sutra_platform_sdk import OntologyFS
    ofs = OntologyFS(client, axes=["kind"], type_name="file")
    info = ofs.getinfo("/trace/frank_trace.PML", namespaces=["details"])
    assert info.is_dir is False
    assert info.size == 100


def test_readonly(client):
    import fs.errors
    from sutra_platform_sdk import OntologyFS
    ofs = OntologyFS(client, axes=["kind"], type_name="file")
    with pytest.raises(fs.errors.ResourceReadOnly):
        ofs.makedir("/nope")
