#! /bin/bash
#
# Most tests are now in t-tget.cl.
#
# The test here just tests upgrading and what episodes would be
# downloaded with the new version of the program.

args="--root $PWD --db $PWD/test.db --config $PWD/tget-config/config.cl"
tget="./tget/tget $args"

set -eux

rm -fr test.db*

cp -rp ~/.tget.d/db test.db

### This is a one-time test thing:
#$tget --catch-up

$tget --learn
