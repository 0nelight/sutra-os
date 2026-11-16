import pytest

from sutra_platform_sdk import avg, bucket, by, count, link, origin, sum_
from sutra_platform_sdk.predicates import Property


def test_sort_direction_is_server_side(client, fake_http):
    objs = client.ontology.objects.File.order_by(Property("file", "size"), desc=True).all()
    _, _, body = fake_http.calls[-1]
    assert body["order-by"] == "size" and body["sort-dir"] == "desc"
    assert [o.values["size"] for o in objs] == [300, 100, 50]


def test_offset_without_limit_pages_from_there(client):
    assert client.ontology.objects.File.offset(10).all() == []


def test_page_carries_the_prelimit_total(client, fake_http):
    page = client.ontology.objects.File.page(2)
    _, _, body = fake_http.calls[-1]
    assert body["limit"] == 2 and body["offset"] == 0 and body["count?"] is True
    assert len(page) == 2 and page.total == 3 and page.next_offset == 2


def test_last_page_has_no_next_offset(client):
    assert client.ontology.objects.File.page(2, 2).next_offset is None
    assert client.ontology.objects.File.page(10).next_offset is None


def test_paginate_walks_every_row_once(client):
    ids = [o.id for o in client.ontology.objects.File.paginate(2)]
    assert ids == ["f1", "f2", "f3"]


def test_count_reads_no_rows(client, fake_http):
    assert client.ontology.objects.File.count() == 3
    _, _, body = fake_http.calls[-1]
    assert body["limit"] == 0 and body["count?"] is True


def test_around_retypes_the_set(client, fake_http):
    client.ontology.objects.File.around("evidence-for").all()
    _, _, body = fake_http.calls[-1]
    assert body["find"] == "file" and body["around"] == ["evidence-for"]


def test_hop_cap_is_the_planners(client):
    s = client.ontology.objects.File.around("evidence-for")
    s = s.around("case-customer").around("evidence-for")
    with pytest.raises(ValueError, match="at most 3 hops"):
        s.around("evidence-for")


def test_aggregate_envelope(client, fake_http):
    result = client.ontology.objects.File.aggregate(
        count(), sum_("size", name="bytes"), group_by=[by("kind")])
    _, _, body = fake_http.calls[-1]
    assert body["group-by"] == ["kind"]
    assert body["aggs"][0]["fn"] == "count"
    assert result.group_keys == ["kind"] and result.metric_keys == ["count", "bytes"]
    flat = {row["kind"]: row for row in result.flat()}
    assert flat["trace"]["count"] == 2 and flat["trace"]["bytes"] == 400


def test_aggregate_orders_buckets_by_a_result_key(client, fake_http):
    total = sum_("size", name="bytes")
    client.ontology.objects.File.aggregate(total, group_by=[by("kind")],
                                           order_by=total, desc=True, limit=1)
    _, _, body = fake_http.calls[-1]
    assert body["order-by"] == "bytes" and body["sort-dir"] == "desc" and body["limit"] == 1


def test_result_keys_are_predictable():
    assert sum_("size").result_key == "size-sum"
    assert count().result_key == "count"
    assert bucket("datum", "month").result_key == "datum-month"
    assert origin().result_key == "origin"
    assert link("gehoert-zu").result_key == "gehoert-zu"
    assert avg("size", name="mittel").result_key == "mittel"


def test_bad_aggregate_arguments_are_caught_client_side():
    with pytest.raises(ValueError):
        sum_("size", name=None) and bucket("datum", "fortnight")
    with pytest.raises(ValueError, match="needs an attribute"):
        from sutra_platform_sdk.aggregate import Agg
        Agg("sum")


def test_union_goes_through_a_temporary_object_set(client, fake_http):
    traces = client.ontology.objects.File.where(Property("file", "kind") == "trace")
    big = client.ontology.objects.File.where(Property("file", "size") > 200)
    combined = traces | big
    posts = [c for c in fake_http.calls if c[1].startswith("/api/ontology/object-sets")]
    assert len(posts) == 1
    ir = posts[0][2]["ir"]
    assert ir["op"] == "union" and len(ir["in"]) == 2
    assert ir["in"][0]["op"] == "filter"
    assert ir["in"][0]["where"] == ["and", ["=", "kind", "trace"]]
    assert ir["in"][0]["in"] == {"op": "scan", "root": "ri.sutra.main.object-types.aaa"}

    combined.all()
    _, _, body = fake_http.calls[-1]
    assert body["object-set"] == "ri.sutra.main.object-sets.tmp1"
    assert body["find"] == "file"


def test_search_around_inside_an_ir_names_the_side_not_a_rid(client):
    ir = client.ontology.objects.File.around("evidence-for").to_ir()
    assert ir == {"op": "search-around", "link": "evidence-for",
                  "in": {"op": "scan", "root": "ri.sutra.main.object-types.aaa"}}
    assert "dir" not in ir


def test_a_hop_takes_no_direction(client):
    with pytest.raises(TypeError):
        client.ontology.objects.File.around("evidence-for", "from")
    with pytest.raises(TypeError, match="link side"):
        client.ontology.objects.File.around({"link": "evidence-for", "dir": "from"})


def test_the_reverse_side_is_reached_by_its_own_name(client, fake_http):
    client.ontology.objects.SupportCase.around("evidence-for").all()
    _, _, body = fake_http.calls[-1]
    assert body["find"] == "support-case" and body["around"] == ["evidence-for"]


def test_a_self_link_hops_by_side_name():
    from conftest import FakeHttp, SELF_LINK_SCHEMA
    from sutra_platform_sdk import SutraClient
    http = FakeHttp(schema=SELF_LINK_SCHEMA, rows={"projekt": []})
    c = SutraClient("http://fake", http=http)
    for side in ("hauptprojekt", "projekte"):
        ir = c.ontology.objects.Projekt.around(side).to_ir()
        assert ir["link"] == side and "dir" not in ir
    c.ontology.objects.Projekt.around("projekte").all()
    assert http.calls[-1][2]["around"] == ["projekte"]


def test_a_paged_set_refuses_to_be_a_set_member(client):
    a = client.ontology.objects.File.limit(5)
    b = client.ontology.objects.File
    with pytest.raises(ValueError, match="paged set cannot be a member"):
        a | b


def test_link_subfilter_has_no_ir_spelling(client):
    from sutra_platform_sdk.predicates import Link
    s = client.ontology.objects.File.where(
        Link("file", "evidence-for")(Property("support-case", "status") == "open"))
    with pytest.raises(ValueError, match="link sub-filter"):
        s.to_ir()


def test_static_id_set_is_a_set_not_a_no_op(client, fake_http):
    client.ontology.objects.File.with_ids(["f1", "f3"]).all()
    _, _, body = fake_http.calls[-1]
    assert body["ids"] == ["f1", "f3"]
    assert client.ontology.objects.File.with_ids([]).all() == []


def test_temporary_set_can_be_deleted(client, fake_http):
    created = client.object_sets.create_temporary(find="file")
    assert client.object_sets.delete_temporary(created["rid"]) == {"deleted": True}


def test_create_temporary_wants_exactly_one_form(client):
    with pytest.raises(ValueError):
        client.object_sets.create_temporary()
    with pytest.raises(ValueError):
        client.object_sets.create_temporary({"op": "scan"}, find="file")
