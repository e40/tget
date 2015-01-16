#! /bin/bash
#
# Most tests are now in t-tget.cl.

set -eu

function tget {
    ./tget/tget --debug --root \
	$PWD --db $PWD/main.db \
	--config $PWD/tget-test-data/config.cl \
	"$@"
}

set -x

######### test #1
# copy production db, compact it, which also tests upgrade code

rm -fr main.db*
cp -rp ~/.tget.d/db main.db

tget --cron --dump-all | egrep '^#' > test.compact.before
tget --compact-database
tget --cron --dump-all | egrep '^#' > test.compact.after

if ! diff <(sort < test.compact.before) <(sort < test.compact.after); then
    echo ERROR: compare of compact before/after failed 
    exit 1
else
    rm -f test.compact.before test.compact.after
fi

du -sh main.db*
