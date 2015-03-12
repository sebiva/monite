#!/bin/sh
# This is used mainly because we need to clean up after the tests are run, and
# the command rm -r is not a nice command to 'try' evaluation of in a test
# suite, as it could lead to disaster.
cabal test test-scripts
rm -r test/tmp
