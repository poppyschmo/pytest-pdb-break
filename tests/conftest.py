import sys
from pathlib import Path
from contextlib import contextmanager

# When True, don't inject this repo's root into the sys.path of testdir
# subprocesses (as is otherwise done via mini conftest)
installed = False

reporoot = Path(__file__).parent.parent
assert (reporoot / "tox.ini").exists()

# Glob pat for installed (versioned) egg-/dist-info metadata dirs
info_fnpat = "pytest_pdb_break-*.*-info"


@contextmanager
def prepended_root():
    sys.path.insert(0, str(reporoot))
    try:
        yield
    finally:
        sys.path.remove(str(reporoot))


try:
    import pytest_pdb_break
except ImportError:
    # Assume local editing is in effect and src should be directly importable
    sys.path.insert(0, str(reporoot))
else:
    if next(Path(pytest_pdb_break.__file__).parent.glob(info_fnpat), None):
        installed = True
        # Since /helpers isn't bundled as a subpackage of the main plugin, it
        # must be imported early on while still hiding everything else under /
        with prepended_root():
            import helpers  # noqa: F401
    else:
        "Repo root must already be in sys.path, likely via -m pytest"
