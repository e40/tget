#! /bin/bash

set -eux

tget()
{
    ./tget/tget --debug --learn --root "$PWD" --db "$PWD/test.db" \
	--config "$PWD/tget-config/config.cl" $*
}

clean_database()
{
    rm -fr test.db*
}

test_start_fresh()
{
    clean_database
    tget
}

test_upgrade()
{
    clean_database
    cp -rp ~/.tget.d/db test.db
    tget
}

test_start_fresh
test_upgrade
