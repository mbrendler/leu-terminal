===
Leu
===

Leu is a command line tool to query dict.leo.org.

It started as one of my first Haskell projects and was later rewritten in C, so
that it builds without installing a language toolchain.  The Haskell sources
are still in the repository.


Getting started
===============

Build::

   $ ./build.sh

Usage::

   $ build/leu löwe
   $ build/leu -h

Tests::

   $ ./build.sh test


Requirements
============

macOS
   Nothing but the Xcode Command Line Tools (``xcode-select --install``).
   libcurl and libxml2 are part of the SDK, no package manager needed.

Linux
   A C compiler and the libcurl and libxml2 development packages, on Debian::

      $ sudo apt install gcc libcurl4-openssl-dev libxml2-dev pkg-config


Build without a local compiler
==============================

The Dockerfile builds ``leu`` in a container::

   $ podman build -t leu-build .
   $ podman create --name leu-tmp leu-build
   $ podman cp leu-tmp:/out/leu ./leu
   $ podman rm leu-tmp

Usage::

   $ ./leu löwe

This only produces a Linux binary.  For macOS run ``./build.sh`` on the Mac.


The old Haskell version
=======================

Build::

   $ cabal new-build

Usage::

   $ dist-newstyle/build/*/*/*/x/leu/build/leu/leu löwe
