from sutra_functions import ObjectSet, OntologyEdits, _MANIFEST, _by_api


class _Objects:
    def __getattr__(self, api):
        name = _by_api(api)
        if name is None:
            raise AttributeError("no object type " + repr(api))
        return ObjectSet(name)


class _Ontology:
    def __init__(self):
        self.objects = _Objects()

    def edits(self):
        return OntologyEdits()


class PlatformClient:
    def __init__(self, *_args, **_kwargs):
        self.ontology = _Ontology()


__all__ = ["PlatformClient"]
