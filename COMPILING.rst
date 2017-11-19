
Compiling Steroids
==================

Compiling Steroids requires ``clang++`` v3.7, among other packages. Please also
notice that:

- Development for Steroids happens in the ``master`` branch. If you want a stable
  version of the library you should download and compile the sources of the
  `latest available release <https://github.com/cesaro/steroids/releases>`__.
- Steroids has only been compiled and tested under Debian/Ubuntu, although it should
  probably work on other Linux distributions.
- Steroids will only work on x86-64 machines.

Dependencies
------------

- coreutils
- git
- GNU make
- Python 2
- Clang 3.7
- LLVM 3.7

Optional:

- Generating bitcode files for C programs consisting of multiple compilation
  units will require
  `Whole Program LLVM <https://github.com/travitch/whole-program-llvm>`__.

Compilation
-----------

The following steps assume that you have a Debian/Ubuntu distribution:

1. Install a suitable development environment::

    sudo apt-get install coreutils git make python2.7

2. Install clang v3.7 and LLVM v3.7. DPU currently does not compile under g++,
   and you will need clang 3.7 to run the tool, anyway::

    sudo apt-get install llvm-3.7-dev clang-3.7

   After the installation, the command ``llvm-config-3.7`` should be in your
   ``PATH``, and typing ``llvm-config-3.7 --prefix`` should print the
   installation path of LLVM 3.7.

3. Download and compile the sources of the `latest release`_ available.

6. Compile::

    make dist

7. Optional: run regression tests (note: currently we dont' have any)::

    make regression

Steroids is now installed in the ``dist/`` folder. See `<doc/example>`__ for an
example project that compiles and link against Steroids.

.. _latest release : https://github.com/cesaro/dpu/releases/latest
