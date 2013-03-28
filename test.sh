#! /bin/bash

#make clean && make

set -eux

tget="./tget/tget --root $PWD --db $PWD/test.db --config $PWD/config.cl"

clean()
{
    rm -fr test.db*
}

test1()
{
    # read the static feed:
    $tget --debug
    rm -f ep.log
    # read the live data:
    $tget
}

test2()
{
    # test upgrade
    cp -rp ~/.tget.d/db test.db
    $tget
}

clean && test1
clean && test2
