# From https://github.com/poppyschmo/znc-signal
import sys
import pprint
import reprlib
from importlib.util import module_from_spec

ordered_pprint = module_from_spec(pprint.__spec__)
pprint.__loader__.exec_module(ordered_pprint)
sys.modules["ordered_pprint"] = ordered_pprint

ordered_reprlib = module_from_spec(reprlib.__spec__)
reprlib.__loader__.exec_module(ordered_reprlib)
sys.modules["ordered_reprlib"] = ordered_reprlib


class cmp_falso:
    __slots__ = ['obj']

    def __init__(self, obj):
        self.obj = obj

    def __lt__(self, other):
        return False


def _safe_tuple(t):
    """Call cmp_falso instead of _safe_key.

    Not sure how stable this is, but the original ``_safe_key`` still
    gets called by ``_pprint_set``.
    """
    return cmp_falso(t[0]), cmp_falso(t[1])


def _possibly_sorted(x):
    """Bypass the sorted() call in reprlib._possibly_sorted

    This is mostly for ensuring reproducibility in tests.
    """
    return list(x)


ordered_pprint._safe_tuple = _safe_tuple
OrderedPrettyPrinter = ordered_pprint.PrettyPrinter

ordered_reprlib._possibly_sorted = _possibly_sorted
OrderedRepr = ordered_reprlib.Repr
