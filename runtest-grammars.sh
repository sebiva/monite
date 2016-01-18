#!/bin/sh

DIR=$(pwd)
cd src/
make
cd $DIR
cabal test test-grammars
