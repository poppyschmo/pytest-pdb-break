"""
The point of this is to gauge whether it's worth trying to predict sys.path
based on various environmental factors, assuming no other plugins are involved.

Notes
~~~~~
Not sure exactly where sys.path is being modified, but it changes between
initial config parsing and the actual test run. See expected output below.
"""

import os
import sys
import json
import pytest
import subprocess
from pathlib import Path
from textwrap import dedent
from itertools import product
from collections import namedtuple

# pytest_addhooks is nearest hook to _preparse and load_setuptools_entrypoints
plugin_src = dedent("""\
    paths = []

    def pytest_addhooks(pluginmanager):
        import sys
        paths.extend(sys.path)
""")

# "Entry" data obj
#
# variant: i|c|p - plugin discovery: installed, conftest, -p option arg
# is_marg: bool  - mode of exec invocation - pytest or python -m module
# has_ini: bool  - has ini at top level
# in_subd: bool  - invoked from subdir, beneath ini/conftest
# samedir: bool  - rootdir matches invocation dir
# preroot: [..]  - rootdir appearances in partial sys.path during parse
# preinvo: [..]  - invodir appearances in partial sys.path during parse
# runroot: [..]  - rootdir appearances in partial sys.path during run
# runinvo: [..]  - invodir appearances in partial sys.path during run
#
main_src = dedent("""\

    def abbrev(path):
        from pathlib import Path
        return str(Path(*(s[0] for s in Path(path).parts)))

    def test_foo(request):
        variant = %(variant)r
        if variant == "conftest":
            from conftest import paths
        else:
            assert variant in ("-p import", "installed")
            _pm = request.config.pluginmanager
            paths = _pm.get_plugin("pytest_syspath").paths

        invodir = request.config.invocation_dir.strpath
        rootdir = request.config.rootdir.strpath
        assert %(cwd)r == invodir
        import sys
        assert list(sorted(paths)) == list(sorted(set(paths)))
        assert all(paths)
        assert not any(p in paths[3:] for p in (rootdir, invodir))
        assert not any(p in sys.path[3:] for p in (rootdir, invodir))

        entry = dict(variant=variant,
                     is_marg=%(is_marg)s,
                     has_ini=%(has_ini)s,
                     in_subd=%(in_subd)s,
                     samedir=invodir == rootdir,
                     preroot=[p == rootdir for p in paths[:3]],
                     preinvo=[p == invodir for p in paths[:3]],
                     runroot=[p == rootdir for p in sys.path[:3]],
                     runinvo=[p == invodir for p in sys.path[:3]])

        import json
        with open(%(data)r) as flor:
            d = json.load(flor)
        d.append(entry)
        with open(%(data)r, "w") as flow:
            json.dump(d, flow)

        from pprint import PrettyPrinter as PP

        # This is useless, should try leveraging pytest analysis tools
        diff = [(i, abbrev(a), abbrev(b)) for i, (a, b) in
                enumerate(zip(paths, sys.path)) if a != b]
        out=dict(entry=entry, paths=paths, syspath=sys.path, diff=diff)
        print("", PP().pformat(out), sep="\\n")
""")


@pytest.fixture(scope="module")
def module_tmpdir(tmpdir_factory):
    root = tmpdir_factory.mktemp("external_lib", numbered=True)
    lib = root.mkdir("lib")
    data = root.mkdir("data") / "results.json"
    json.dump([], data)
    project = root.mkdir("project")
    #
    project.join("pytest_syspath.py").write(plugin_src)
    project.join("setup.py").write(dedent("""\
        import setuptools
        setuptools.setup(
            name="pytest_syspath",
            entry_points={"pytest11": ["pytest_syspath = pytest_syspath"]},
            py_modules=["pytest_syspath"]
        )
    """))
    cmdline = [sys.executable, "-mpip", "install", "--target", lib, project]
    subprocess.check_call(cmdline)
    return root, lib, data, project


_params = product(("conftest", "installed", "-p import"),  # plugin variant
                  (True, False),  # is_marg
                  (True, False),  # has_ini
                  (True, False))  # in_subd


class Pnt(namedtuple("Param", "variant, is_marg, has_ini, in_subd")):
    def __repr__(self):
        # E.g., conf-ini-nomarg-nosubd
        rest = [n.split("_")[-1] for n in self._fields[1:]]
        variant = "parg" if self.variant == "-p import" else self.variant
        return "-".join((
            variant[:4],
            *(f"no{n}" if v else n for n, v in zip(rest, self[1:]))
        ))


_params = list(Pnt(*p) for p in _params)


@pytest.fixture(params=_params, ids=str)
def plugged(testdir, module_tmpdir, request):
    import subprocess

    __, lib, data, project = module_tmpdir

    variant, is_marg, has_ini, in_subd = request.param

    # Replace orig, which prepends curdir to PYTHONPATH
    def popen_patch(inst, cmdargs, stdout, stderr, **kw):
        env = os.environ.copy()
        if "PYTHONPATH" in env:
            # Too complicated
            raise RuntimeError("Don't run this with PYTHONPATH set")
        if variant == "installed":
            env["PYTHONPATH"] = lib.strpath
        elif variant == "-p import":
            env["PYTHONPATH"] = project.strpath
        kw["env"] = env
        kw.update(stdin=subprocess.PIPE, stdout=stdout, stderr=stderr)
        popen = subprocess.Popen(cmdargs, **kw)
        popen.stdin.close()
        return popen

    testdir.popen = popen_patch.__get__(testdir)

    fmt_kwargs = dict(data=data.strpath,
                      variant=variant,
                      is_marg=is_marg,
                      has_ini=has_ini,
                      in_subd=in_subd)

    runargs = ("--capture=no",)

    if variant == "conftest":
        testdir.makeconftest(plugin_src)
    elif variant == "-p import":
        runargs = "-p", "pytest_syspath", *runargs

    if not is_marg:  # <- NOT (else sys.executable -mpytest ...)
        testdir._getpytestargs = lambda: ("pytest",)

    if has_ini:
        testdir.makeini("[pytest]\n")

    if in_subd:
        subdir = testdir.tmpdir.join("subdir")
        fmt_kwargs["cwd"] = subdir.strpath
        testdir.makepyfile(**{"subdir/test_file.py": main_src % fmt_kwargs})
        subdir.chdir()
    else:
        fmt_kwargs["cwd"] = testdir.tmpdir.strpath
        testdir.makepyfile(main_src % fmt_kwargs)

    try:
        yield testdir, runargs
    finally:
        del testdir.__dict__["popen"]
        if not is_marg:
            del testdir.__dict__["_getpytestargs"]


def test_main(plugged):
    testdir, runargs = plugged
    result = testdir.runpytest_subprocess(*runargs)
    result.assert_outcomes(passed=1)
    testdir.tmpdir.join("stdout").write("\n".join(result.outlines) + "\n")
    testdir.tmpdir.join("stderr").write("\n".join(result.errlines) + "\n")


def render(items):
    outlines = []
    for member in items:
        p = Pnt(**{k: v for k, v in member.items() if k in Pnt._fields})
        prez = zip(member["preroot"], member["preinvo"])
        runz = zip(member["runroot"], member["runinvo"])
        if member["samedir"]:
            pre = ["x" if r | i else "." for r, i in prez]
            run = ["x" if r | i else "." for r, i in runz]
        else:
            pre = [r and "r" or (i and "i") or "." for r, i in prez]
            run = [r and "r" or (i and "i") or "." for r, i in runz]
        outlines.append(f"{p.variant:11s}"
                        f"{('pt', '-m')[p.is_marg]:3s}"
                        f"{('', 'ini')[p.has_ini]:4s}"
                        f"{('/', '../')[p.in_subd]:4s}"
                        f"[{''.join(pre)}] "
                        f"({''.join(run)})")
    return "\n".join(outlines) + "\n"


def do_render(path=None):
    if path is None:
        path = Path(f"/tmp/pytest-of-{os.getlogin()}/pytest-current",
                    "external_libcurrent/data/results.json")
    assert path.exists() and path.stat().st_size > 0
    items = json.loads(path.read_text())
    out = path.parent / "results.out"
    out.write_text(render(items))
    return out


def test_render(module_tmpdir):
    out = do_render(Path(module_tmpdir[2]))
    assert out.read_text() == dedent("""\
    conftest   -m ini ../ [ri.] (iri)
    conftest   -m ini /   [x..] (x..)
    conftest   -m     ../ [.x.] (x.x)
    conftest   -m     /   [x..] (x..)
    conftest   pt ini ../ [r..] (ir.)
    conftest   pt ini /   [x..] (x..)
    conftest   pt     ../ [...] (x..)
    conftest   pt     /   [x..] (x..)
    installed  -m ini ../ [i..] (i..)
    installed  -m ini /   [x..] (x..)
    installed  -m     ../ [x..] (x..)
    installed  -m     /   [x..] (x..)
    installed  pt ini ../ [...] (i..)
    installed  pt ini /   [...] (x..)
    installed  pt     ../ [...] (x..)
    installed  pt     /   [...] (x..)
    -p import  -m ini ../ [i..] (i..)
    -p import  -m ini /   [x..] (x..)
    -p import  -m     ../ [x..] (x..)
    -p import  -m     /   [x..] (x..)
    -p import  pt ini ../ [...] (i..)
    -p import  pt ini /   [...] (x..)
    -p import  pt     ../ [...] (x..)
    -p import  pt     /   [...] (x..)
    """)
