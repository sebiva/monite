#!/bin/sh

cabal test test-scripts
rm -r test/tmp
