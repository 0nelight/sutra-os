import json

from sutra_platform_sdk.replay import Replay, main


class Obj:
    def __init__(self, id, **values):
        self.id, self.values = id, values


class FakeSet:
    def __init__(self, rows):
        self.rows = rows

    def where(self, pred):
        k, v = pred
        return FakeSet([r for r in self.rows if r.values.get(k) == v])

    def limit(self, n):
        return self.rows[:n]


class Prop:
    def __init__(self, k):
        self.k = k

    def __eq__(self, v):
        return (self.k, v)


class FakeClient:
    def __init__(self, who, rows, calls):
        self.who, self.calls = who, calls
        self._http = type("H", (), {"branch": None})()
        client = self

        class Objects:
            def __getitem__(self, t):
                return FakeSet(rows[t])

        class Actions:
            def invoke(self, action, args, publish):
                calls.append(("action", client.who, client._http.branch, action, args, publish))
                return type("R", (), {"result": "ok", "id": None})()

        class Builds:
            def create(self, only, strategy):
                calls.append(("build", client.who, client._http.branch, only))
                return {"run-id": "r1", "counts": {}}

        self.ontology = type("O", (), {"objects": Objects(), "actions": Actions()})()
        self.orchestration = type("X", (), {"builds": Builds()})()

    def props(self, t):
        return type("P", (), {"__getattr__": lambda self, k: Prop(k.replace("_", "-"))})()


PLAN = {
    "base_url": "http://fake", "branch": "fr01",
    "users": {"admin": {}, "EDDA": {}},
    "days": [
        {"day": "2025-12-23", "steps": [
            {"do": "hook", "name": "vorbedingung", "args": {"n": 2}},
            {"do": "build", "only": ["shape-konsolvorschlag"]},
            {"do": "action", "as": "EDDA", "action": "tag-konsolidieren",
             "args": {"zeilen": {"$objects": {"type": "konsolvorschlag",
                                              "where": {"tag-id": "124|2025-12-15"},
                                              "in": {"zeiten-id": ["2", "1"]},
                                              "sort": "reihenfolge"}},
                      "am": "2025-12-23"},
             "skip_if_empty": "zeilen"},
            {"do": "action", "as": "EDDA", "action": "tag-konsolidieren",
             "args": {"zeilen": {"$objects": {"type": "konsolvorschlag",
                                              "where": {"tag-id": "niemand"}}}},
             "skip_if_empty": "zeilen"}]},
        {"day": "2025-12-29", "steps": [{"do": "hook", "name": "fehlt"}]}]}

ROWS = {"konsolvorschlag": [Obj("b", **{"tag-id": "124|2025-12-15", "zeiten-id": "2", "reihenfolge": 2}),
                            Obj("a", **{"tag-id": "124|2025-12-15", "zeiten-id": "1", "reihenfolge": 1}),
                            Obj("x", **{"tag-id": "124|2025-12-15", "zeiten-id": "9", "reihenfolge": 0}),
                            Obj("z", **{"tag-id": "anders", "zeiten-id": "1", "reihenfolge": 5})]}


def run(**kw):
    calls, log = [], []
    hooks = {"vorbedingung": lambda r, day, n: calls.append(("hook", day["day"], n)) or n}
    r = Replay(PLAN, connect=lambda who, cred: FakeClient(who, ROWS, calls), hooks=hooks)
    return r.run(protocol=log.append, **kw), calls, log


def test_a_day_runs_its_steps_in_order_as_the_named_user_on_the_branch():
    res, calls, log = run(day_to="2025-12-23")
    assert calls[0] == ("hook", "2025-12-23", 2)
    assert calls[1] == ("build", "admin", "fr01", ["shape-konsolvorschlag"])
    assert calls[2] == ("action", "EDDA", "fr01", "tag-konsolidieren",
                        {"zeilen": ["a", "b"], "am": "2025-12-23"}, True)
    assert len(calls) == 3, "an empty selection skips its step instead of sending []"
    assert log[3]["skipped"] and res == {"done": 4, "failed": 0}


def test_a_failing_step_is_recorded_and_the_run_says_so():
    res, _, log = run(day_from="2025-12-29")
    assert res["failed"] == 1 and "fehlt" in log[0]["error"]


def test_the_budget_stops_before_a_new_day():
    res, calls, log = run(budget=0)
    assert calls == [] and "budget" in log[0]["stopped"]


def test_main_reads_a_plan_file(tmp_path, monkeypatch):
    p = tmp_path / "plan.json"
    p.write_text(json.dumps({**PLAN, "hooks": "hooks.py",
                             "days": [{"day": "d", "steps": [{"do": "hook", "name": "eins"}]}]}))
    (tmp_path / "hooks.py").write_text("def eins(replay, day):\n    return day['day']\n")
    assert main([str(p), "--protocol", str(tmp_path / "p.jsonl")]) == 0
    assert json.loads((tmp_path / "p.jsonl").read_text())["result"] == "d"
