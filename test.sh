#! /bin/bash
#
# Most tests are now in t-tget.cl.

set -eu

function tget {
    ./tget/tget --debug --root \
	$PWD --db $PWD/test.db \
	--config $PWD/tget-config/config.cl \
	"$@"
}

set -x

rm -fr test.db*

######### test #1
# copy production db, compact it

cp -rp ~/.tget.d/db test.db
du -sh test.db*
tget --compact-database
du -sh test.db*

######### test #2
# start from a fresh database and learn

tget --run --reset --learn

tget --cron --dump-complete-to > test.complete-to
if ! diff tget-test-data/reference.complete-to test.complete-to; then
    echo ERROR: complete-to test failed 
    exit 1
else
    rm -f test.complete-to
fi

tget --cron --dump-stats > test.stats
if ! diff tget-test-data/reference.stats test.stats; then
    echo ERROR: stats test failed 
    exit 1
else
    rm -f test.stats
fi

tget --cron --dump-all > test.all
if ! diff <(sort < tget-test-data/reference.all) <(sort < test.all); then
    echo ERROR: all test failed 
    exit 1
else
    rm -f test.all
fi
