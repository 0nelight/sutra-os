import datetime
import decimal
from typing import List, Optional

from sutra_functions import OntologyEdit, UserFacingError
from sutra_runtime import intermediate, refuse_generator
from sutra_transforms import Media

Integer = int
Long = int
Short = int
Byte = int
Float = float
Double = float
Decimal = decimal.Decimal
String = str
Boolean = bool
Date = datetime.date
Timestamp = datetime.datetime
Array = List
Set = set
Map = dict

__all__ = ["function", "intermediate", "OntologyEdit", "UserFacingError", "Integer", "Long",
           "Short", "Byte", "Float", "Double", "Decimal", "String", "Boolean", "Date",
           "Timestamp", "Array", "Set", "Map", "List", "Optional", "Media"]


def function(fn=None, *, edits=None, sources=None, api_name=None, **_options):
    def mark(f):
        refuse_generator(f, "@function")
        f._sutra_function = True
        f._sutra_edits = list(edits or [])
        f._sutra_sources = list(dict.fromkeys(sources or []))
        f._sutra_api_name = api_name
        return f
    return mark(fn) if callable(fn) else mark
