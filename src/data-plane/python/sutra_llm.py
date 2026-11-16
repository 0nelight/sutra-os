
import json
import os
import random
import sys
import time
import urllib.error
import urllib.request
from dataclasses import dataclass
from typing import Optional

from sutra_runtime import defer_remaining, get_secret, span, trace


@dataclass
class LanguageModel:

    source: Optional[str] = None
    model: Optional[str] = None
    base_url: Optional[str] = None
    timeout: int = 120
    max_tokens: int = 1024
    kind: str = "language-model"

    def __post_init__(self):
        if not self.model:
            raise ValueError("LanguageModel needs model= — a name of the model register")


class LanguageModelError(RuntimeError):

    def __init__(self, *args, fatal: bool = False, status: Optional[int] = None):
        super().__init__(*args)
        self.fatal = fatal
        self.status = status


class LanguageModelBudgetWait(LanguageModelError):
    pass


_PLATFORM_FAILURE_MARKERS = (
    "connection is not available",
    "sutra:pg-control",
    "request timed out after",
)


_BUDGET_EXHAUSTED_MARKERS = (
    "daily llm budget",
)

_BUDGET_WAIT_MARKER = "llm budget wait"

_BUDGET_WAITING = None


def _budget_wait(reason):
    global _BUDGET_WAITING
    _BUDGET_WAITING = reason
    defer_remaining()
    return LanguageModelBudgetWait(reason, status=429)


_RATE_ATTEMPTS = 8
_RATE_WAIT_MAX = 60.0

DEFAULT_WORKERS = 32


def _rate_wait(e, attempt):
    try:
        s = float((e.headers or {}).get("Retry-After"))
    except (TypeError, ValueError):
        s = float(2 ** attempt)
    return min(_RATE_WAIT_MAX, max(0.5, s)) + random.uniform(0, 1.0)


def _budget_exhausted(code: Optional[int], detail: str) -> bool:
    d = (detail or "").lower()
    return code in (None, 429) and any(s in d for s in _BUDGET_EXHAUSTED_MARKERS)


def _fatal_http(code: Optional[int], detail: str) -> bool:
    if code in (401, 403):
        return True
    if _budget_exhausted(code, detail):
        return True
    d = (detail or "").lower()
    if any(s in d for s in _PLATFORM_FAILURE_MARKERS):
        return True
    return any(s in d for s in ("http 401", "http 403", "forbidden", "unauthorized",
                                "invalid api key", "invalid_api_key", "permission"))


class ChatModel:

    def __init__(self, source: Optional[str], model: str, base_url: Optional[str] = None,
                 timeout: int = 120, max_tokens: int = 1024):
        self.source = source
        self.model = model
        self.base_url = base_url or "https://api.x.ai/v1/chat/completions"
        self.timeout = timeout
        self.max_tokens = max_tokens
        self._api_key = None

    def _key(self) -> str:
        if self._api_key is None:
            self._api_key = get_secret(self.source, "api-key")
        return self._api_key

    def chat(self, messages, *, tools=None, tool_choice=None,
             max_tokens=None, **params) -> dict:
        if _BUDGET_WAITING is not None:
            raise LanguageModelBudgetWait(_BUDGET_WAITING, status=429)
        trace('chat', model=self.model)
        payload = {"model": self.model,
                   "max_tokens": max_tokens or self.max_tokens,
                   "messages": messages}
        if tools is not None:
            payload["tools"] = tools
        if tool_choice is not None:
            payload["tool_choice"] = tool_choice
        payload.update(params)
        lm_service = os.environ.get("SUTRA_LM_SERVICE")
        if lm_service:
            cfg = json.loads(lm_service)
            url = cfg["url"]
            data = json.dumps({"source": self.source, "model": self.model,
                               "caller": os.environ.get("SUTRA_TRANSFORM_NAME"),
                               "run_id": os.environ.get("SUTRA_RUN_ID"),
                               "request": payload}).encode("utf-8")
            headers = {"content-type": "application/json"}
            if cfg.get("token"):
                headers["authorization"] = f"Bearer {cfg['token']}"
        else:
            url = self.base_url
            data = json.dumps(payload).encode("utf-8")
            headers = {"content-type": "application/json",
                       "authorization": f"Bearer {self._key()}"}
        req = urllib.request.Request(url, data=data, headers=headers, method="POST")
        attempt = 0
        while True:
            try:
                return self._post(req)
            except urllib.error.HTTPError as e:
                try:
                    detail = e.read().decode("utf-8", "replace")
                except Exception:
                    detail = ""
                if (e.code == 429 and attempt + 1 < _RATE_ATTEMPTS
                        and _BUDGET_WAIT_MARKER not in detail.lower()
                        and not _budget_exhausted(e.code, detail)):
                    wait = _rate_wait(e, attempt)
                    trace('llm-rate-wait', model=self.model, attempt=attempt + 1,
                          seconds=round(wait, 1))
                    time.sleep(wait)
                    attempt += 1
                    continue
                raise self._failure(e, detail) from e
            except (urllib.error.URLError, TimeoutError, OSError) as e:
                raise LanguageModelError(
                    f"{self.source} chat call failed: {e}") from e

    def _post(self, req):
        with span('llm', model=self.model) as s, \
                urllib.request.urlopen(req, timeout=self.timeout) as resp:
            answer = json.loads(resp.read().decode("utf-8"))
            usage = answer.get('usage') if isinstance(answer, dict) else None
            if isinstance(usage, dict):
                s['tokens_in'] = usage.get('prompt_tokens', usage.get('input_tokens'))
                s['tokens_out'] = usage.get('completion_tokens',
                                            usage.get('output_tokens'))
                cached = (usage.get('prompt_tokens_details') or {}).get('cached_tokens')
                if cached:
                    s['tokens_cached'] = cached
            return answer

    def _failure(self, e, detail):
        if e.code == 429 and _BUDGET_WAIT_MARKER in (detail or "").lower():
            return _budget_wait(f"{self.source}: {detail}")
        if _budget_exhausted(e.code, detail):
            return LanguageModelError(
                f"daily LLM budget exhausted — run aborted so that no item is "
                f"dropped or swept as a skip; re-run once the budget resets "
                f"({self.source} HTTP {e.code}: {detail})",
                fatal=True, status=e.code)
        return LanguageModelError(
            f"{self.source} chat call failed: {e}"
            + (f" — {detail}" if detail else ""),
            fatal=_fatal_http(e.code, detail), status=e.code)

    def tool_call(self, tool, messages, *, max_tokens=None, **params):
        name = tool["function"]["name"]
        resp = self.chat(messages, tools=[tool],
                         tool_choice={"type": "function",
                                      "function": {"name": name}},
                         max_tokens=max_tokens, **params)
        return tool_args(resp, name)


def tool_args(api_response, name):
    for choice in api_response.get("choices", []):
        for tc in (choice.get("message") or {}).get("tool_calls") or []:
            fn = tc.get("function") or {}
            if fn.get("name") == name:
                try:
                    return json.loads(fn.get("arguments") or "{}")
                except json.JSONDecodeError:
                    return None
    return None


def parallel_map(fn, items, max_workers: int = DEFAULT_WORKERS, on_error: str = 'fail'):
    import concurrent.futures as _cf
    if on_error not in ('fail', 'skip'):
        raise ValueError("parallel_map: on_error must be 'fail' or 'skip', got %r" % (on_error,))
    items = list(items)
    if not items:
        return []
    out = [None] * len(items)
    errors = 0
    last_exc = None
    waited = None
    with _cf.ThreadPoolExecutor(max_workers=max(1, min(max_workers, len(items)))) as ex:
        futs = {ex.submit(fn, it): i for i, it in enumerate(items)}
        for f in _cf.as_completed(futs):
            if f.cancelled():
                continue
            try:
                out[futs[f]] = f.result()
            except LanguageModelBudgetWait as e:
                if waited is None:
                    waited = e
                    for g in futs:
                        g.cancel()
            except Exception as e:
                if getattr(e, "fatal", False):
                    raise
                if on_error == 'fail':
                    raise
                errors += 1
                last_exc = e
                out[futs[f]] = None
    if waited is not None:
        done = sum(1 for o in out if o is not None)
        defer_remaining()
        print("parallel_map: WAITING for the LLM budget — %d of %d item(s) answered, "
              "the rest is left for a later run (%s)" % (done, len(items), waited),
              file=sys.stderr)
        trace("llm-budget-wait", answered=done, total=len(items), reason=str(waited))
        return out
    if errors == len(items):
        raise LanguageModelError(
            f"all {len(items)} model call(s) failed — systemic outage, not "
            f"per-item skips (last: {last_exc})", fatal=True) from last_exc
    if errors:
        print("parallel_map: SKIPPED %d of %d item(s) after a transient error "
              "(on_error='skip') — they produced NO row this run (last: %s: %s)"
              % (errors, len(items), type(last_exc).__name__, last_exc),
              file=sys.stderr)
        trace("parallel-map-skips", skipped=errors, total=len(items),
              last_error="%s: %s" % (type(last_exc).__name__, last_exc))
    return out


def error_column_for(result_key):
    return "_completion_error-%s" % (result_key,)


def completion_column(fn, rows, result_key, max_workers: int = DEFAULT_WORKERS):
    import concurrent.futures as _cf
    rows = list(rows)
    if not rows:
        return []
    err_key = error_column_for(result_key)
    out = [None] * len(rows)
    errors = 0
    last_exc = None
    with _cf.ThreadPoolExecutor(max_workers=max(1, min(max_workers, len(rows)))) as ex:
        futs = {ex.submit(fn, r): i for i, r in enumerate(rows)}
        for f in _cf.as_completed(futs):
            i = futs[f]
            base = dict(rows[i]) if isinstance(rows[i], dict) else {}
            try:
                out[i] = dict(base, **{result_key: f.result(), err_key: None})
            except Exception as e:
                if getattr(e, "fatal", False):
                    raise
                errors += 1
                last_exc = e
                out[i] = dict(base, **{result_key: None,
                                       err_key: "%s: %s" % (type(e).__name__, e)})
    if errors == len(rows):
        raise LanguageModelError(
            f"all {len(rows)} model call(s) failed — systemic outage, not "
            f"per-item errors (last: {last_exc})", fatal=True) from last_exc
    if errors:
        print("completion_column: %d of %d item(s) carry %s — they are ROWS, not "
              "drops (last: %s: %s)"
              % (errors, len(rows), err_key, type(last_exc).__name__, last_exc),
              file=sys.stderr)
        trace("completion-column-errors", errors=errors, total=len(rows),
              column=err_key, last_error="%s: %s" % (type(last_exc).__name__, last_exc))
    return out
