================
pytest-pdb-break
================

This is an aid to help **text editors** fire up the debugger and fast-forward
to the point of interest. If you already have a solid workflow with
``breakpoint()`` snippets and/or the ``--trace`` and ``--pdb`` options, there's
nothing to see here.

    .. _actually:

    This basically_ does ...

    .. code:: console

        $ pytest --trace spam.py::test_foo
        ...
        (pdb) until 42

    or ...

    .. code:: python

        def test_foo():
            ...
            pytest.set_trace(header="line 42")

    with a few minor conveniences sprinkled in


Simpatico check
    #. You prefer a noninvasive, one-click, carry-me approach to entering the
       debugger
    #. You like the certainty of stopping offered by the second example above
    #. You like tab completion of variables and attributes
    #. You want inside ``unittest.TestCase`` s and your own fixtures
       (experimental)

Don't install
    Unlike proper pytest plugins, this isn't meant to be installed as a Python
    package. The editor will instead inject an isolated installation via
    ``PYTHONPATH``, but only while in use (no internet connection required).

WIPs
    - `Emacs <https://github.com/poppyschmo/pytest-pdb-break/blob/master/emacs/>`_
    - `Vim <https://github.com/poppyschmo/pytest-pdb-break/blob/master/vim/>`_

Notes/caveats
    #. Unfortunately, this thing has only ever been tried/tested on Linux

    #. It mainly exists for its author to learn about pytest, a decent
       understanding of which continues to evade

    #. It does not support the ``-m pytest`` style of invocation, meaning
       working directories are not implicitly prepended to ``sys.path``

    #.
       .. _basically:

       Actually_, it's more like ...

       .. code:: console

           $ echo until 42 > .pdbrc
           $ pytest --trace spam.py::test_foo; rm -f .pdbrc
           ========================== test session starts ==========================
           ...
            > /repo/spam.py(42)test_foo()
           -> assert True  # line 42
           (pdb) _

       or ...

       .. code:: console

           $ cp spam.py spam.py~
           $ sed -i -E \
           >   '42{h;s/^(\s+)(.*)$/\1import pytest; pytest.set_trace()/p;g}' \
           >   spam.py
           $ pytest spam.py::test_foo; mv -f spam.py~ spam.py
           ========================== test session starts ==========================
           ...
            > /repo/spam.py(43)test_foo()
           -> assert True  # line 42
           (pdb) _

       (assuming the target line opens with a new statement)

    #. When hacking on the main pytest plugin, disregard the imperative above
       and *do install*:

       .. code:: console

           (.venv)repo@master$ pip install -e .

       If sticking with the included reference integrations, some extra
       finagling may be required. For example:

       - Disabling "isolated-lib" creation via one of the documented options
       - Manually managing the editor plugin (or making it play nice with your
         manager)

       If doing your own thing, the usual rules apply, like ensuring the right
       exec path and env vars have precedence when summoning pytest:

       .. code:: javascript

           const pytest = pty.spawn(
               'pytest', ['--break=spam.py:42', nodeid],
               { cwd: rootdir, env: Object.assign({}, process.env, modified) }
           );  // or whatever


TODOs
    #. External helper returning ``--options`` list
    #. ``asyncio``
