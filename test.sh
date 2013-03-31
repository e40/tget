#! /bin/bash

set -eux

tget()
{
    ./tget/tget --learn --root "$PWD" --db "$PWD/test.db" \
	--config "$PWD/tget-config/config.cl" \
	$*
}

clean_database()
{
    rm -fr test.db*
}

test_start_fresh()
{
    clean_database
    tget --feed-interval 180
}

test_upgrade()
{
    clean_database

    # test upgrade
    cp -rp ~/.tget.d/db test.db
    tget
}

test_start_fresh
test_upgrade
