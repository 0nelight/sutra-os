from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from ._naming import snake_to_kebab
from .schema import is_required
from .objects import OntologyObject
from .paths import url

EDIT_TYPES = {"add-object": "addObject",
              "modify-object": "modifyObject",
              "delete-object": "deleteObject"}


class ActionMode(str, Enum):
    VALIDATE_ONLY = "VALIDATE_ONLY"
    VALIDATE_AND_EXECUTE = "VALIDATE_AND_EXECUTE"


class ReturnEditsMode(str, Enum):
    ALL = "ALL"
    NONE = "NONE"


@dataclass(frozen=True)
class ActionConfig:
    mode: ActionMode = ActionMode.VALIDATE_AND_EXECUTE
    return_edits: ReturnEditsMode = ReturnEditsMode.NONE


@dataclass(frozen=True)
class ObjectEdit:
    type: str
    object_type: str
    object_id: Any
    primary_key: Any = None


@dataclass(frozen=True)
class ObjectEdits:
    edits: tuple = ()
    added_objects_count: int = 0
    modified_objects_count: int = 0
    deleted_objects_count: int = 0
    added_links_count: int = 0
    deleted_links_count: int = 0
    type: str = "edits"


@dataclass(frozen=True)
class ParameterEvaluationResult:
    result: str
    required: bool


@dataclass(frozen=True)
class ValidateActionResponse:
    result: str
    parameters: dict = field(default_factory=dict)
    submission_criteria: tuple = ()


@dataclass(frozen=True)
class SyncApplyActionResponse:
    action: str
    validation: ValidateActionResponse | None = None
    edits: ObjectEdits | None = None
    staged: bool = False
    seq: Any = None
    id: Any = None
    result: Any = None
    revision: int | None = None
    raw: dict = field(default_factory=dict, repr=False)


@dataclass(frozen=True)
class BatchApplyActionResponse:
    action: str
    edits: ObjectEdits | None = None
    staged: bool = False
    seqs: tuple = ()
    ids: tuple = ()
    result: Any = None
    raw: dict = field(default_factory=dict, repr=False)


def _edits(raw: dict | None) -> ObjectEdits | None:
    if not raw:
        return None
    return ObjectEdits(
        edits=tuple(ObjectEdit(type=EDIT_TYPES.get(e.get("type"), e.get("type")),
                               object_type=e.get("object-type"),
                               object_id=e.get("object-id"),
                               primary_key=e.get("primary-key"))
                    for e in raw.get("edits") or []),
        added_objects_count=raw.get("added-objects-count", 0),
        modified_objects_count=raw.get("modified-objects-count", 0),
        deleted_objects_count=raw.get("deleted-objects-count", 0),
        added_links_count=raw.get("added-links-count", 0),
        deleted_links_count=raw.get("deleted-links-count", 0))


def _validation(raw: dict) -> ValidateActionResponse:
    missing = set(raw.get("missing") or [])
    invalid = set(raw.get("invalid") or [])
    params = raw.get("params") or {}
    return ValidateActionResponse(
        result="INVALID" if raw.get("blocked?") else "VALID",
        parameters={p: ParameterEvaluationResult(
            result="INVALID" if (p in missing or p in invalid) else "VALID",
            required=is_required(spec))
            for p, spec in params.items()},
        submission_criteria=tuple(x for x in [raw.get("reason")] if x))


def _revision(raw: dict):
    result = raw.get("result")
    commit = result.get("commit") if isinstance(result, dict) else None
    return ((commit or {}).get("basis") or {}).get("rev")


def _query(publish: bool, cfg: "ActionConfig") -> str:
    q = []
    if publish:
        q.append("publish=true")
    if cfg.return_edits is ReturnEditsMode.ALL:
        q.append("return-edits=true")
    return "?" + "&".join(q) if q else ""


def _ref(v):
    if isinstance(v, OntologyObject):
        return v.id
    if isinstance(v, (list, tuple)):
        return [_ref(x) for x in v]
    return v


def _args(args: dict) -> dict:
    return {snake_to_kebab(k): _ref(v) for k, v in args.items()}


class _BoundAction:
    def __init__(self, actions: "Actions", action_kw: str):
        self._actions = actions
        self._action = action_kw

    def __call__(self, *, publish: bool = True,
                 action_config: ActionConfig | None = None, **kwargs):
        args = {snake_to_kebab(k): v for k, v in kwargs.items()}
        return self._actions._invoke(self._action, args, publish=publish,
                                     action_config=action_config)


class Actions:
    def __init__(self, client):
        self._client = client

    def __getattr__(self, name: str) -> _BoundAction:
        return _BoundAction(self, snake_to_kebab(name))

    def invoke(self, action: str, args: dict, *, publish: bool = True,
               action_config: ActionConfig | None = None):
        return self._invoke(snake_to_kebab(action), dict(args), publish=publish,
                            action_config=action_config)

    def validate(self, action: str, args: dict) -> ValidateActionResponse:
        raw = self._client._http.post(
            url("action-params"),
            {"action": snake_to_kebab(action), "args": _args(args)})
        return _validation(raw)

    def batch_apply(self, action: str, requests: list, *, publish: bool = True,
                    action_config: ActionConfig | None = None):
        cfg = action_config or ActionConfig()
        action = snake_to_kebab(action)
        rows = [_args(r) for r in requests]
        raw = self._client._http.post(url("actions-batch") + _query(publish, cfg),
                                      {"action": action, "requests": rows})
        return BatchApplyActionResponse(
            action=raw.get("action", action), edits=_edits(raw.get("edits")),
            staged=bool(raw.get("staged")), seqs=tuple(raw.get("seqs") or []),
            ids=tuple(raw.get("ids") or []), result=raw.get("result"), raw=raw)

    def _invoke(self, action: str, args: dict, *, publish: bool = True,
                action_config: ActionConfig | None = None):
        cfg = action_config or ActionConfig()
        if cfg.mode is ActionMode.VALIDATE_ONLY:
            return SyncApplyActionResponse(action=action,
                                           validation=self.validate(action, args))
        raw = self._client._http.post(url("actions") + _query(publish, cfg),
                                      {"action": action, "args": _args(args)})
        return SyncApplyActionResponse(
            action=raw.get("action", action), edits=_edits(raw.get("edits")),
            staged=bool(raw.get("staged")), seq=raw.get("seq"), id=raw.get("id"),
            result=raw.get("result"), revision=_revision(raw), raw=raw)
