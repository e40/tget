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
    archive-restore) ;;
    copy)            ;;
    save-restore)    ;;
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
    archive-restore)
	rm -f archive.before archive.after
	# get a before/after snapshot of the amount of data:
	$tget --auto-backup never --dump-stats --archive archive.before
	$tget --restore archive.before --dump-stats
	$tget --archive archive.after --dump-stats
	diff archive.before archive.after || true
	;;
    save-restore)
	rm -f archive.before archive.after
	# get a before/after snapshot of the amount of data:
	$tget --auto-backup never --dump-stats --archive archive.before
	$tget --backup-method save-restore --auto-backup force \
	    --dump-stats --archive archive.after
	diff archive.before archive.after || true
	;;
esac
