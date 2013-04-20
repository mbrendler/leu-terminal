===
Leu
===

Leu is a little command line tool to query dict.leo.org.

This is one of my first Haskell projects, so there is much to improve and to
learn.


Getting started
===============

Build::

   $ cabal install --only-dependencies
   $ cabal configure
   $ cabal build

Usage::

   $ dist/build/leu/leu löwe
   $ dist/build/leu/leu -h


TODO
====

 * Leu and pipes: ``leu hello | cat`` (*getTermSize* is the problem)
 * ignore or process *minprio*-tag
