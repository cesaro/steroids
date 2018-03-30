
Compiling Steroids
==================

Compiling Steroids requires ``clang++`` v6.0, among other packages. Please also
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
- Clang 6.0
- LLVM 6.0
- libncurses
- zlib

Optional:

- Generating bitcode files for C programs consisting of multiple compilation
  units will require
  `Whole Program LLVM <https://github.com/travitch/whole-program-llvm>`__.

Compilation
-----------

The following steps assume that you have a Debian/Ubuntu distribution:

1. Install a suitable development environment::

    sudo apt-get install coreutils git make python2.7

2. Install clang v6.0 and LLVM v6.0. DPU currently does not compile under g++,
   and you will need clang 6.0 to run the tool, anyway::

    sudo apt-get install llvm-6.0-dev clang-6.0

   If the above command fails, you might want to update your APT configuration
   (``/etc/apt/sources.list``) with the repositories listed in
   http://apt.llvm.org/ for your distribution.

   After a successful installation, the command ``llvm-config-6.0`` should be in
   your ``PATH``, and typing ``llvm-config-6.0 --prefix`` should print the
   installation path of LLVM 6.0.

3. Install additional libraries::
   
    sudo apt-get install libncurses5-dev zlib1g-dev

4. Download and compile the sources of the `latest release`_ available.

5. Compile::

    make dist

6. Optional: run regression tests (note: currently we do not have any)::

    make regression

Steroids is now installed in the ``dist/`` folder. See `<doc/example>`__ for an
example project that compiles and link against Steroids.

.. _latest release : https://github.com/cesaro/steroids/releases/latest

