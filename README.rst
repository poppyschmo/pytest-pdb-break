================
pytest-pdb-break
================

This is an aid to help traditional **text editors** fire up the debugger and
fast-forward to the point of interest. If you already have a solid workflow
with ``breakpoint()`` snippets and/or the ``--trace`` and ``--pdb`` options,
there's nothing to see here.

    This basically does ...

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
    #. You want inside ``unittest.TestCase`` s, your own fixtures, and nested
       async funcs (all experimental)

Before you try
    Check out the newer ptvsd/DAP/pydevd-based extensions for Vim and Emacs.
    They definitely have the potential to work seamlessly with pytest, VS-Code
    style. This plugin only launches PDB, the built-in GDB-style debugger.

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
    #. Improve ``asyncio`` support
    #. Support class names with non-``Test`` prefixes.
    #. External helper returning ``--options`` list
