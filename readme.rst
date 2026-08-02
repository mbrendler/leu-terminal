===
Leu
===

Leu is a command line tool to query dict.leo.org.

This is one of my first Haskell projects, so there is much to improve and to
learn.  But it works!


Getting started
===============

Build::

   $ cabal new-build

Usage::

   $ dist-newstyle/build/*/*/*/x/leu/build/leu/leu löwe
   $ dist-newstyle/build/*/*/*/x/leu/build/leu/leu -h


Build without a Haskell environment
===================================

The Dockerfile builds ``leu`` in a container, so no local GHC is needed::

   $ podman build -t leu-build .
   $ podman create --name leu-tmp leu-build
   $ podman cp leu-tmp:/out/leu ./leu
   $ podman rm leu-tmp

Usage::

   $ ./leu löwe
