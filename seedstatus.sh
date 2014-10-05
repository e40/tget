#! /bin/bash

###############################################################################
## user tweakable variables

# Minimum seed time in seconds
#  BTN requires 24 hours, so add 3 hours slop, just in case
seedmin=$(( 3600 * 27 )) 

###############################################################################

set -eu

###############################################################################

function usage()
{
    if test -n "${*-}"; then
	echo "Error: $*" 1>&2
    fi
    cat 1>&2 <<EOF
Usage: $0 [-r] [-q]

-r :: remove torrents that are "done"
-q :: only print essential information

Torrents are "done" that are finished seeding or have seeded
for the minimum amount of time ($(print_time $seedmin)).
EOF
    exit 1
}

function errordie()
{
    if [ -n "${*-}" ]; then
	echo "Error: $*" 1>&2
    fi
    exit 1
}

function print_time()
{
    local i=$1
    ((i/=60, i/=60, i/=24, days=i%24))
    echo ${days}d $(date -u -d @$1 +"%Hh %Mm %Ss")
}

function tm()
{
    transmission-remote ${TRANSMISSION_HOST}:${TRANSMISSION_PORT} \
	--auth=${TRANSMISSION_USER}:${TRANSMISSION_PASS} \
	"$@"
}

function float_ge()
{
    if [ $(echo $1 '>=' $2 | bc -ql) -eq 1 ]; then
	return 0
    else
	return 1
    fi
}
float_ge 1.04 1.04 || errordie float_ge failed test 1
float_ge 1.041 1.04 || errordie float_ge failed test 2
float_ge 1.05 1.04 || errordie float_ge failed test 3
float_ge 1.04 1.041 && errordie float_ge failed test 4
float_ge 1.04 1.0401 && errordie float_ge failed test 5
float_ge 1.03999 1.04 && errordie float_ge failed test 6

cron=
quiet=
remove=

while [ $# -gt 0 ]; do
    case $1 in
	--help) usage ;;
	-r) remove=$1 ;;
	-q) quiet=$1 ;;
	-*) usage ;;
	*)  usage ;;
    esac
    shift
done

function remove_torrent()
{
    if [ ! "$remove" ]; then
	return 0
    fi

    if tm -t $1 -r > $temp2 2>&1; then
	:
    else
	echo ${name}: removing exited with a non-zero status:
	cat $temp2
	return 0
    fi

    if ! grep -q success $temp2; then
	echo ${name}: torrent removal might not have worked, please check
    fi
}

###############################################################################

temp=/tmp/temp$$
temp2=/tmp/temp2$$
rm -f $temp $temp2
trap "/bin/rm -f $temp $temp2" 0

now=$(date +"%s")
# Our timezone
zone=$(date +"%Z")

if tm -t all -si > $temp; then
    dsr=$(grep 'Default seed ratio limit:' $temp | awk '{print $5}')
else
    errordie transmission failed 1
fi

if ! tm -t all --info > $temp; then
    errordie transmission failed 2
fi

state=START

while read line; do
    [ -z "$line" ] && continue
    a=( $line )
    word0=${a[0]}
    n=${#a[@]}
    if [ $n -gt 1 ]; then
	word1=${a[1]}
    else
	word1=
    fi

    case $state in
	START) # we must be at the start of an entry
	    [ "$word0" != "NAME" ] && errordie expected NAME
	    state=ID
	    continue
	    ;;
	SKIP) # skip to the next entry, which starts with "NAME"
	    [ "$word0" != "NAME" ] && continue
	    state=ID
	    continue
	    ;;
	ID)
	    [ "$word0" != "Id:" ] && continue
	    id=$word1
	    state=NAME
	    continue
	    ;;
	NAME)
	    [ "$word0" != "Name:" ] && continue
	    name=$word1
	    state=PDONE
	    continue
	    ;;
	PDONE)
	    [ "$word0" != "Percent" ] && continue
	    [ "$word1" != "Done:" ] && errordie expected Done:
	    if [ "${a[2]}" = "100%" ]; then
		state=RATIO
	    else
		# Not finished, so skip it
		if [ ! "$quiet" ]; then
		    echo -e "Torrent is not finished (${a[2]})\\n  $name\\n"
		fi
		state=SKIP
	    fi
	    continue
	    ;;
	RATIO)
	    [ "$word0" != "Ratio:" ] && continue
	    ratio=$word1
	    state=RATIOLIM
	    continue
	    ;;
	RATIOLIM)
	    [ "$word0" != "Ratio" ] && continue
	    [ "$word1" != "Limit:" ] && errordie expected Limit:
	    if [ "${a[2]}" = "Default" ]; then
		ratio_limit=$dsr
	    else
		ratio_limit=${a[2]}
	    fi
	    state=DATE_FINISHED
	    continue
	    ;;
	DATE_FINISHED)
	    if [ "$word0" = "Date" ] && [ "$word1" = "finished:" ]; then
		# Add time zone
		finished=$(date +"%s" --date="${a[2]} ${a[3]} ${a[4]} ${a[5]} $zone ${a[6]}")
		state=CALC
		continue
	    else
		continue
	    fi
	    ;;
	CALC)
	    # Determine if this torrent is "done" and can be removed.
	    # It is if either 1) seeding is complete, or 2) we have seeded
	    # the torrent for $seedmin seconds
	    #
	    # How do we know the torrent can be removed?
	    # transmission-remote doesn't give us a "seeding complete"
	    # indication, so we use "Ratio" >= "Ratio Limit".
	    if float_ge $ratio $ratio_limit; then
		echo -e "Seeding is complete for (ratio is $ratio):\\n  $name\\n"
		remove_torrent $id $name
		state=SKIP
		continue
	    fi

	    # seeding not complete, so see if we've seeded for the
	    # minimum amount of time.

	    seedsecs=$(($now - $finished))
	    if [ $seedsecs -gt $seedmin ]; then
		echo -e "Torrent has seeded enough ($(print_time $seedsecs)):\\n  $name\\n"
		remove_torrent $id $name
	    elif [ ! "$quiet" ]; then
		left=$(($seedmin - $seedsecs))
		echo -e "Incomplete:\\n  $name"
		echo -e "    seeding time left: $(print_time $left)"
		echo -e "    ratio $ratio, target $ratio_limit"
	    fi
	    state=SKIP
	    continue
	    ;;
    esac
done <<< "$(cat $temp)"

case $state in
    START|SKIP) ;;
    *)  errordie expected state START or SKIP got $state
	;;
esac
