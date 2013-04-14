#! /bin/bash


HLINT=$(which hlint)

if test "$HLINT" == "" ; then
    HLINT="$HOME/.cabal/bin/hlint"
fi


if test -x "$HLINT" ; then
    $HLINT Leu/*.hs leu.hs CmdArgs.hs Tests.hs
else
    echo "Can't find hlint."
fi
