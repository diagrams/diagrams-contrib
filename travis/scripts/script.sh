#! /bin/bash

if [[ $GHCVER == "head" || -n "$SKIP_HADDOCK" ]]
  then
    DOCBUILD="echo 'Skipping docs...'"
  else
    DOCBUILD="$CABAL haddock"
fi

$CABAL configure --enable-tests --enable-benchmarks -v2 $CABAL_FLAGS\
  && $CABAL build --ghc-options='-Wall -Werror -ddump-minimal-imports'\
  && $CABAL test\
  && $DOCBUILD\
  && $CABAL check\
  && $CABAL sdist

  # having trouble getting this to work, Cabal version mismatches etc.
  #  && packdeps `ls --color=never *.cabal`\
  #  && packunused\
