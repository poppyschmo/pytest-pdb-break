================
pytest-pdb-break
================

This is an aid to help **traditional text editors** (Vim and Emacs) fire up the
debugger and fast-forward to the point of interest. If you already have a solid
workflow with ``breakpoint()`` snippets and/or the ``--trace`` and ``--pdb``
options, there's nothing to see here.

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

    #. You like the certainty of stopping afforded by the second example above

    #. You like tab completion of variables and attributes

    #. You want inside fixtures, |unittest|_'s, and pytest-asyncio_ items

.. |unittest| replace:: ``unittest.TestCase``
.. _unittest: https://docs.python.org/3.8/library/unittest.html#unittest.TestCase
.. _pytest-asyncio: https://pypi.org/project/pytest-asyncio


Before you try
    Check out the newer ptvsd_/`DAP`_/`pydevd`_-based extensions for Vim and
    Emacs. They definitely have the potential to work seamlessly with pytest,
    as VS-Code seems to. This plugin merely launches PDB, the built-in
    GDB-style debugger.

.. _ptvsd: https://github.com/microsoft/ptvsd
.. _pydevd: https://github.com/fabioz/PyDev.Debugger
.. _DAP: https://microsoft.github.io/debug-adapter-protocol/implementors/adapters


Don't install
    Unlike proper pytest plugins, this isn't meant to be installed as a Python
    package. The editor will instead inject an isolated installation via
    ``PYTHONPATH``, but only while in use (no internet connection required).


WIPs
    - `Emacs <https://github.com/poppyschmo/pytest-pdb-break/blob/master/emacs/>`_
    - `Vim <https://github.com/poppyschmo/pytest-pdb-break/blob/master/vim/>`_


TODOs
    #. Ditch helpers in favor of client/server model

    #. Support arbitrary test names via the |pyfunc|_ and ``python_classes``
       options

    #. Support FreeBSD


Notes/caveats
    #. Unfortunately, this thing has only ever been tried/tested on Linux

    #. It mainly exists for its author to learn about pytest, a decent
       understanding of which continues to evade

    #. It does not support the ``-m pytest`` style of invocation, meaning
       working directories are not implicitly prepended to ``sys.path``

    #. If ever hacking on the main pytest plugin, disregard the above and
       *do install*:

       .. code:: console

           (.venv)repo@master$ pip install -e .

       Some extra pains may be required to control how changes get reloaded.
       For example:

       - Disabling "isolated-lib" creation via one of the documented options

       - Manually managing the editor plugin (or making it play nice with your
         manager)

       Otherwise, the usual rules apply, like ensuring the right exec path and
       env vars have precedence when summoning pytest:

       .. code:: javascript

           const pytest = pty.spawn(
               'pytest', ['--break=spam.py:42', nodeid],
               { cwd: rootdir, env: Object.assign({}, process.env, modified) }
           );  // or whatever


.. |pyfunc| replace:: ``"python_functions"``
.. _pyfunc: https://docs.pytest.org/en/latest/reference.html#confval-python_functions
