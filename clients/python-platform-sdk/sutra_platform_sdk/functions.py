from __future__ import annotations

from ._naming import snake_to_kebab
from .paths import url


class _BoundFunction:
    def __init__(self, functions: "Functions", name: str):
        self._functions = functions
        self._name = name

    def __call__(self, **inputs):
        return self._functions._run(self._name, inputs)


class Functions:
    def __init__(self, client):
        self._client = client

    def __getattr__(self, name: str) -> _BoundFunction:
        return _BoundFunction(self, snake_to_kebab(name))

    def run(self, name: str, inputs: dict | None = None):
        return self._run(snake_to_kebab(name), inputs or {})

    def _run(self, name: str, inputs: dict):
        wire_inputs = {snake_to_kebab(k): v for k, v in inputs.items()}
        return self._client._http.post(
            url("function-run"), {"function-name": name, "inputs": wire_inputs})
