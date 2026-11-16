from __future__ import annotations

import io

import fs.base
import fs.errors
from fs.enums import ResourceType
from fs.info import Info

from ._naming import snake_to_kebab

_MISSING = "∅"


def _epoch(v):
    if v is None:
        return None
    if hasattr(v, "timestamp"):
        try:
            return v.timestamp()
        except Exception:
            return None
    if isinstance(v, (int, float)):
        return float(v)
    if isinstance(v, str):
        try:
            from datetime import datetime
            return datetime.fromisoformat(v.replace("Z", "+00:00")).timestamp()
        except Exception:
            return None
    return None


class OntologyFS(fs.base.FS):
    def __init__(self, client, axes, *, type_name="file", view=None,
                 title_attr="name", hash_attr="hash", size_attr="size",
                 mtime_attr="mtime", ctime_attr="ctime", atime_attr="atime"):
        super().__init__()
        self.client = client
        self.axes = [snake_to_kebab(a) for a in axes]
        if view is not None:
            v = client.views.get(view) or {}
            vis = v.get("visible-types")
            if isinstance(vis, list) and vis:
                type_name = vis[0]
            elif isinstance(vis, dict) and vis.get("object-types"):
                type_name = list(vis["object-types"])[0]
        self.type_name = type_name
        self.title_attr = snake_to_kebab(title_attr)
        self.hash_attr = snake_to_kebab(hash_attr)
        self.size_attr = snake_to_kebab(size_attr)
        self.mtime_attr = snake_to_kebab(mtime_attr)
        self.ctime_attr = snake_to_kebab(ctime_attr)
        self.atime_attr = snake_to_kebab(atime_attr)
        self._cache = None

    def _objects(self):
        if self._cache is None:
            self._cache = self.client.ontology.objects[self.type_name].all()
        return self._cache

    @staticmethod
    def _parts(path):
        return [p for p in path.strip("/").split("/") if p]

    @staticmethod
    def _axis_val(obj, axis):
        v = obj.values.get(axis)
        return _MISSING if v is None else str(v)

    def _matching(self, parts):
        out = []
        for o in self._objects():
            if all(self._axis_val(o, self.axes[i]) == seg for i, seg in enumerate(parts)):
                out.append(o)
        return out

    def _title(self, obj):
        return str(obj.values.get(self.title_attr) or obj.id)

    def _resolve_file(self, parts):
        axis_parts, leaf = parts[: len(self.axes)], parts[len(self.axes)]
        leaf = leaf.split("~")[0]
        for o in self._matching(axis_parts):
            if self._title(o) == leaf:
                return o
        return None

    def listdir(self, path):
        parts = self._parts(path)
        if len(parts) > len(self.axes):
            raise fs.errors.DirectoryExpected(path)
        if len(parts) < len(self.axes):
            axis = self.axes[len(parts)]
            seen, vals = set(), []
            for o in self._matching(parts):
                v = self._axis_val(o, axis)
                if v not in seen:
                    seen.add(v)
                    vals.append(v)
            return sorted(vals)
        names, counts = [], {}
        for o in self._matching(parts):
            t = self._title(o)
            if t in counts:
                counts[t] += 1
                t = f"{t}~{counts[t]}"
            else:
                counts[t] = 0
            names.append(t)
        return names

    def getinfo(self, path, namespaces=None):
        namespaces = namespaces or ()
        parts = self._parts(path)
        if len(parts) <= len(self.axes):
            name = parts[-1] if parts else ""
            return Info({"basic": {"name": name, "is_dir": True},
                         "details": {"type": int(ResourceType.directory)}})
        obj = self._resolve_file(parts)
        if obj is None:
            raise fs.errors.ResourceNotFound(path)
        details = {"type": int(ResourceType.file)}
        size = obj.values.get(self.size_attr)
        if isinstance(size, int):
            details["size"] = size
        for attr, key in ((self.mtime_attr, "modified"),
                          (self.ctime_attr, "created"),
                          (self.atime_attr, "accessed")):
            ts = _epoch(obj.values.get(attr))
            if ts is not None:
                details[key] = ts
        return Info({"basic": {"name": parts[-1], "is_dir": False}, "details": details})

    def openbin(self, path, mode="r", buffering=-1, **options):
        if any(c in mode for c in "wax+"):
            raise fs.errors.ResourceReadOnly(path)
        parts = self._parts(path)
        if len(parts) <= len(self.axes):
            raise fs.errors.FileExpected(path)
        obj = self._resolve_file(parts)
        if obj is None:
            raise fs.errors.ResourceNotFound(path)
        h = obj.values.get(self.hash_attr)
        if not h:
            raise fs.errors.ResourceNotFound(path + " (no content hash)")
        resp = self.client.blob(h)
        return io.BytesIO(resp.content)

    def makedir(self, path, permissions=None, recreate=False):
        raise fs.errors.ResourceReadOnly(path)

    def remove(self, path):
        raise fs.errors.ResourceReadOnly(path)

    def removedir(self, path):
        raise fs.errors.ResourceReadOnly(path)

    def setinfo(self, path, info):
        raise fs.errors.ResourceReadOnly(path)
