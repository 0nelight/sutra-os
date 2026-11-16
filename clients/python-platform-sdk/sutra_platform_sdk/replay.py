from __future__ import annotations

import argparse
import importlib.util
import json
import os
import pathlib
import sys
import time
from typing import Any, Callable


class ReplayError(Exception):
    pass


def _objects(client, sel: dict) -> list:
    t = sel["type"]
    os_ = client.ontology.objects[t]
    if sel.get("where"):
        props = client.props(t)
        for k, v in sel["where"].items():
            os_ = os_.where(getattr(props, k.replace("-", "_")) == v)
    found = list(os_.limit(sel.get("limit", 1000)))
    for k, allowed in (sel.get("in") or {}).items():
        allowed = {str(a) for a in allowed}
        found = [o for o in found if str(o.values.get(k)) in allowed]
    if sel.get("sort"):
        k = sel["sort"]
        found.sort(key=lambda o: (o.values.get(k) is None, o.values.get(k)))
    return [o.id for o in found]


def resolve(client, v: Any) -> Any:
    if isinstance(v, dict):
        if set(v) == {"$objects"}:
            return _objects(client, v["$objects"])
        return {k: resolve(client, x) for k, x in v.items()}
    if isinstance(v, list):
        return [resolve(client, x) for x in v]
    return v


def _load_hooks(path: pathlib.Path | None) -> dict[str, Callable]:
    if path is None:
        return {}
    spec = importlib.util.spec_from_file_location("replay_hooks", path)
    mod = importlib.util.module_from_spec(spec)
    sys.path.insert(0, str(path.parent))
    spec.loader.exec_module(mod)
    return {k: v for k, v in vars(mod).items() if callable(v) and not k.startswith("_")}


class Replay:
    def __init__(self, plan: dict, *, base: pathlib.Path | None = None,
                 connect: Callable[[str, dict], Any] | None = None,
                 hooks: dict[str, Callable] | None = None):
        self.plan = plan
        self.base = base or pathlib.Path.cwd()
        self._connect = connect or self._login
        self._clients: dict[str, Any] = {}
        self.hooks = hooks if hooks is not None else _load_hooks(
            self.base / plan["hooks"] if plan.get("hooks") else None)

    def _login(self, who: str, cred: dict):
        from .client import SutraClient
        pw = os.environ.get(cred["password_env"]) if cred.get("password_env") else cred.get("password")
        if pw is None and not cred.get("token_env"):
            raise ReplayError(f"no password for {who}: set ${cred.get('password_env')}")
        return SutraClient(self.plan["base_url"],
                           token=os.environ.get(cred["token_env"]) if cred.get("token_env") else None,
                           username=cred.get("username", who), password=pw)

    def client(self, who: str | None = None):
        who = who or self.plan.get("default_user") or next(iter(self.plan["users"]))
        if who not in self._clients:
            cred = (self.plan.get("users") or {}).get(who)
            if cred is None:
                raise ReplayError(f"user {who!r} is not in the plan's users")
            c = self._connect(who, cred)
            c._http.branch = self.plan.get("branch")
            self._clients[who] = c
        return self._clients[who]

    def step(self, day: dict, s: dict) -> dict:
        do = s["do"]
        if do == "build":
            r = self.client(s.get("as")).orchestration.builds.create(
                only=s.get("only"), strategy=s.get("strategy", "selected"))
            return {"run_id": (r or {}).get("run-id"), "counts": (r or {}).get("counts")}
        if do == "action":
            c = self.client(s.get("as"))
            args = resolve(c, s.get("args") or {})
            empty = s.get("skip_if_empty")
            if empty and not args.get(empty):
                return {"skipped": f"{empty} is empty"}
            r = c.ontology.actions.invoke(s["action"], args, publish=True)
            return {"result": getattr(r, "result", None), "id": getattr(r, "id", None)}
        if do == "hook":
            f = self.hooks.get(s["name"])
            if f is None:
                raise ReplayError(f"hook {s['name']!r} is not defined in {self.plan.get('hooks')}")
            return {"result": f(self, day, **(s.get("args") or {}))}
        raise ReplayError(f"unknown step {do!r} — build, action or hook")

    def run(self, *, day_from=None, day_to=None, budget: float | None = None,
            protocol: Callable[[dict], None] = lambda e: None, stop_on_error=False) -> dict:
        t0 = time.monotonic()
        done = failed = 0
        for day in self.plan["days"]:
            d = day["day"]
            if (day_from and d < day_from) or (day_to and d > day_to):
                continue
            if budget is not None and time.monotonic() - t0 > budget:
                protocol({"day": d, "stopped": f"budget of {budget}s spent"})
                break
            for i, s in enumerate(day["steps"]):
                ts = time.monotonic()
                entry = {"day": d, "step": i, "do": s["do"],
                         "what": s.get("action") or s.get("name") or s.get("only")}
                try:
                    entry.update(self.step(day, s))
                    entry["ok"] = True
                    done += 1
                except Exception as e:
                    entry.update(ok=False, error=str(e)[:600])
                    failed += 1
                entry["seconds"] = round(time.monotonic() - ts, 2)
                protocol(entry)
                if not entry["ok"] and (stop_on_error or s.get("required")):
                    return {"done": done, "failed": failed, "stopped": entry}
        return {"done": done, "failed": failed}


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(prog="sutra-replay")
    ap.add_argument("plan")
    ap.add_argument("--from", dest="day_from")
    ap.add_argument("--to", dest="day_to")
    ap.add_argument("--budget", type=float, help="seconds; no new day starts after this")
    ap.add_argument("--protocol", help="JSONL file the steps are appended to")
    ap.add_argument("--stop-on-error", action="store_true")
    a = ap.parse_args(argv)
    path = pathlib.Path(a.plan).resolve()
    plan = json.loads(path.read_text())
    out = open(a.protocol, "a") if a.protocol else None

    def protocol(e):
        line = json.dumps(e, ensure_ascii=False, default=str)
        print(line, flush=True)
        if out:
            out.write(line + "\n")
            out.flush()

    try:
        r = Replay(plan, base=path.parent).run(day_from=a.day_from, day_to=a.day_to,
                                               budget=a.budget, protocol=protocol,
                                               stop_on_error=a.stop_on_error)
    finally:
        if out:
            out.close()
    print(json.dumps(r), file=sys.stderr)
    return 1 if r["failed"] else 0


if __name__ == "__main__":
    sys.exit(main())
