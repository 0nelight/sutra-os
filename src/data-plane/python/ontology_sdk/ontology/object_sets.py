import sutra_functions as _rt


def __getattr__(api):
    if api.endswith("ObjectSet") and _rt._by_api(api[:-len("ObjectSet")]):
        return _rt.ObjectSet
    raise AttributeError("no object set type " + repr(api))
