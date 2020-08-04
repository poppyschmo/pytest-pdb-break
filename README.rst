================
pytest-pdb-break
================

**OBSOLETE** this project has been obsoleted by modern tooling with superior
editor integration based on DAP_. It will disappear from PyPI on or soon after
January 1 2021. Maintenance will cease with the release of pytest 7.

Some possibilities worth trying:

    - `ptvsd`_\/`pydevd`_\-based extensions for |vimspector|_ and |dapmode|_

    - (your specific suggestion here)

.. _ptvsd: https://github.com/microsoft/ptvsd

.. _pydevd: https://github.com/fabioz/PyDev.Debugger

.. _DAP: https://microsoft.github.io/debug-adapter-protocol/implementors/adapters

.. No `foo <bar>` like in sphinx?
.. |dapmode| replace:: Emacs
.. _dapmode: https://emacs-lsp.github.io/dap-mode

.. |vimspector| replace:: Vim
.. _vimspector: https://github.com/puremourning/vimspector


This plugin merely launches PDB, the built-in GDB-style debugger.  It does so
by adding a command-line option to specify an initial breakpoint. [#f1]_
Included are add-ons for two traditional text editors, Vim_ and Emacs_. They
help fire up the debugger and fast-forward to the point of interest. If you
already have a solid workflow with ``breakpoint()`` snippets and/or the
``--trace`` and ``--pdb`` options, there's nothing to see here.

    This basically does ...

    .. code:: console

       $ pytest --trace test_spam.py::test_foo
       ...
       (pdb) until 42

    with a few minor conveniences sprinkled in


.. _Vim: https://github.com/poppyschmo/pytest-pdb-break/blob/master/vim/

.. _Emacs: https://github.com/poppyschmo/pytest-pdb-break/blob/master/emacs/


Don't install

    Unlike a proper pytest plugin, this isn't meant to be installed as a Python
    package. [#f2]_ The editor will instead inject an isolated installation via
    ``PYTHONPATH``, but only while in use (no internet connection required).


TODOs

    #. Support remote operation

    #. Support arbitrary test names via the |pyfunc|_ and ``python_classes``
       options

    #. Ensure proper breaking in overridden fixtures

    #. Support FreeBSD (postponed/abandoned)

    #. Ditch helpers in favor of client/server model (postponed/abandoned)


Notes/caveats

    #. This is only meant to run under Linux but has been reported to work
       on MacOS

    #. It mainly exists for its author to learn about pytest, a decent
       understanding of which continues to evade


.. [#f1] E.g., ``pytest --break=test_file.py:42``

.. [#f2] ... unless used as a standalone command-line option

.. |pyfunc| replace:: ``"python_functions"``
.. _pyfunc: https://docs.pytest.org/en/latest/reference.html#confval-python_functions
