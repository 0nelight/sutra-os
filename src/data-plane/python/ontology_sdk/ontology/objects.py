import sutra_functions as _rt


def __getattr__(api):
    name = _rt._by_api(api)
    if name is None:
        raise AttributeError("no object type " + repr(api))
    return _rt.object_class(name)
