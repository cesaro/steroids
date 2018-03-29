
========
Steroids
========

.. image:: https://travis-ci.org/cesaro/steroids.svg?branch=master
    :target: https://travis-ci.org/cesaro/steroids

Steroids is a library for dynamic analysis of multithreaded POSIX C programs. It
can instrument, JIT-compile, and execute a C program in a controlled manner,
allowing the user to control the thread scheduler and get information about the
execution.  For each execution, Steroids returns a stream of program actions
describing the execution.  This contains relevant information, including
thread operations (creations, mutex lock/unlock), memory accesses (if
requested), assertion violations or calls to ``abort``.

Steroids offers both a C++ and a C interface, and compiles as a shared and
static library.  Bindings for Haskell used to be available, but as of November
2017 they are outdated.

How it works
============

Steroids operates on C programs represented as LLVM IR. Supporting C++ programs
should be easy, but has not been tried.

We first instrument the bitcode with a Steroids-provided runtime that gets
control on every call to a ``pthread_*`` function. The runtime also controls the
calls to many other functions of the libc (e.g. ``exit``, ``abort``, ``malloc``,
file access system calls and others).  To hijack a call to function ``foo`` we
define a function named ``_rt_foo`` with the same type signature.  Steroids
automatically scans the input program for calls to ``foo`` and substitutes them
for calls to ``_rt_foo``.  Thus adding new functions is very simple.

The user then specifies the command line of the program (array ``argv``) and the
environment variables (array ``environ``). It then asks Steroids to run the
program. The runtime will collect a user-defined amount of information,
including:

- Calls to ``pthread_*`` functions (mandatory).
- Calls to ``abort`` and ``exit`` (mandatory).
- Memory read and write operations (optional).
- Calls to ``malloc``, ``calloc``, ``realloc``, and ``free`` (currently disabled)
- Allocation of stack space for local variables, function calls, and function
  returns (currently disabled)

During the execution, the runtime logs these operations into a designated memory
area, shared with Steroids.  After the execution this array is returned to the
user for analysis (class ``stid::action_streamt``).  Execution takes place in
the same process (address space) than the user code. Steroids uses LLVM's JIT
support to compile and link the program.

The user may decide now to re-execute the program with a different thread
schedule. Before every execution, the user specifies a `replay sequence` (class
``stid::Replay``). This is a sequence of pairs (``tid``, ``count``), each one of
which specifies that thread ``tid`` is allowed to perform ``count`` so-called
*global operations* before context-switching to the next thread in the sequence.
For the time being, the only global operations considered are (see
``stid/action.hh``):

- ``THCREAT``: a call to ``pthread_create``
- ``THJOIN``: a call to ``pthread_join``
- ``THEXIT``: a call to ``pthread_exit``
- ``MTXLOCK``: a call to ``pthread_mutex_lock``
- ``MTXUNLK``: a call to ``pthread_mutex_unlock``
- ``THSTART``: The beginning of a thread execution.

Example: the program in `<doc/example/program.c>`__ has only two interesting
replay sequences, corresponding to the two orders in which the critical sections
can interleave:

| Replay 1: (0, 4), (1, 4), (0, 2)
| Replay 2: (0, 2), (1, 4), (0, 4)

Example code
============

See folder `<doc/example/>`__ for an example of user code using the Steroids API
to instrument and run (2 times) a program.

Assumptions about the input program
===================================

Steroids assumes that the input program is data-deterministic. That is, the only
source of non-determinism in the execution is the order in which
concurrent thread statements can be interleaved. As a result, all sources of
non-deterministic execution (e.g., command-line arguments, input files) need to
be fixed before running the tool.

Compilation
===========

Instructions for compiling from the sources are available in the
`<COMPILING.rst>`__ file.

Related tools
=============

- `DPU <https://github.com/cesaro/dpu>`__
- `Nidhugg <https://github.com/nidhugg/nidhugg>`__

Contact
=======

DPU is maintained by
`César Rodríguez <http://lipn.univ-paris13.fr/~rodriguez/>`__.
Please feel free to contact me in case of questions or to send feedback.
