#!/bin/bash

# Has to be run from the root of the repo

cabal clean
rm -rf docs
cabal haddock
cp -r dist-newstyle/build/*/ghc-9.6.3/AdventOfCode-*/doc/html/AdventOfCode docs