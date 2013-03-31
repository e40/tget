#! /bin/bash

#make clean && make

set -eux

tget()
{
    ./tget/tget --root "$PWD" --db "$PWD/test.db" \
	--config "$PWD/tget-config/config.cl" \
	$*
}

clean()
{
    rm -fr test.db*
}

test0()
{
    tget --feed-interval 180
}

test1()
{
    # read the static feed:
    tget --debug-feed tget-test-data/tvt.xml
    rm -f ep.log
    # read the live data:
    tget
}

test2()
{
    # test upgrade
    cp -rp ~/.tget.d/db test.db
    tget
}

clean && test0
#clean && test1
#clean && test2
