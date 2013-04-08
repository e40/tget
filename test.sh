#! /bin/bash
#
# Most tests are now in t-tget.cl.
#
# The main feature of this script is that it's a way to operate
# on the production database in a safe way.
#
# The test here just tests upgrading and what episodes would be
# downloaded with the new version of the program.

set -eu

args="--root $PWD --db $PWD/test.db --config $PWD/tget-config/config.cl"
tget="./tget/tget $args"

key=${1---learn}
[ $# -ge 1 ] && shift

case $key in
    copy)            ;;
    compact)         ;;
    --dump-all)      ;;
    --catch-up)      ;;
    --learn)         ;;
    *)
	echo $0: unknown key: $key
	exit 1
	;;
esac

set -x

rm -fr test.db*

cp -rp ~/.tget.d/db test.db

case $key in
    --*)  $tget $key ${*-}
	;;
    compact)
	$tget --compact-database --dump-stats
	$tget --learn
	;;
esac
