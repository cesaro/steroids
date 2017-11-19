
Example Code
============

The files in this folder exemplifies how to use the public API of Steroids to
implement a basic analysis.

To run the example, just type ``make run``.

The ``Makefile`` shows how to compile and link against Steroids and LLVM. It
assumes that the compiled version of steroids is in ``../../dist/``. And that
will be the case only after you have executed ``make dist`` in the root folder
of Steroids.

The example code is in ``example.c``. The ``program.c`` is the code that will be
executed by Steroids.

