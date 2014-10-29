#! /opt/local/bin/bash
# Mac OS X 10.9 note: need newer bash than comes with 10.9
# (for associative arrays "declare -A"), so I installed the Macports
# one.

###############################################################################
# user tweaks:

# Ignore anything watched in the last 4 days
min_hours=$(( 4 * 24 ))

# Use an associative array so we can add a label for the section.
# The downside to using them is the order is no longer sorted.
declare -A directories=(
    ["/Volumes/me/layer/videos/adrian"]="TV:Adrian"
    ["/Volumes/me/layer/videos/adrian+kevin"]="TV:Adrian+Kevin"
    # SKIP: /Volumes/me/layer/videos/anh
    ["/Volumes/me/layer/videos/anh+kevin"]="TV:Anh+Kevin"
    # SKIP /Volumes/me/layer/videos/download.tmp
    ["/Volumes/me/layer/videos/kevin"]="TV:Kevin"
    ["/Volumes/me/layer/videos/movies/adrian"]="Movies:Adrian"
    ["/Volumes/me/layer/videos/movies/adrian+kevin"]="Movies:Adrian+Kevin"
    # SKIP /Volumes/me/layer/videos/movies/anh
    ["/Volumes/me/layer/videos/movies/anh+kevin"]="Movies:Anh+Kevin"
    ["/Volumes/me/layer/videos/movies/kevin"]="Movies:Kevin"
    ["/Volumes/me/layer/videos/tmp"]="temp"
    )

function usage()
{
    if test -n "${*-}"; then
	echo "Error: $*" 1>&2
    fi
    cat 1>&2 <<EOF
Usage: $0 [-v] [-r]

Remove files already watched in Plex that:
 1. are not seeding, and
 2. were watched more than $min_hours hours ago.

This script (currently) must run on the same machine as the PMS.

Command line arguments:

--help           :: this usage
-v               :: print diagnostic information to aid in debugging this script
-r               :: remove files -- without this argument, the files
                    that would be removed are just printed
EOF
    exit 1
}

###############################################################################

# The standard place for Plex db on a Mac:
db="$HOME/Library/Application Support/Plex Media Server/Plug-in Support/Databases/com.plexapp.plugins.library.db"

[ -f "$db" ] || usage PMS database does not exist

###############################################################################

set -eu

verbose=
remove=

while [ $# -gt 0 ]; do
    case $1 in
	--help) usage ;;
	-v) verbose=$1 ;;
	-r) remove=$1 ;;
	*)  usage unknown arg: $1 ;;
    esac
    shift
done

temp="/tmp/sqlite3$$"
rm -f $temp
trap "/bin/rm -f $temp" 0

seeding="$(transmission-remote ${TRANSMISSION_HOST}:${TRANSMISSION_PORT} --auth=${TRANSMISSION_USER}:${TRANSMISSION_PASS} -t all --info | grep Name: | sed 's/Name: //g')"

function seeding()
{
#TODO: use an associative array for this, for speed
    for seeding in $seeding; do
	[ "$1" = "$seeding" ] && return 0
    done
    return 1
}

function watched()
{
    # ' is escaped in sqlite3 with another '
    local file=$(printf '%q' "$1" | sed "s/\'/\'\'/g")
    local date now hours days

    # Plex Media Server uses SQLite3.
    cat <<EOF > $temp
select last_viewed_at from metadata_item_settings
where view_count > 0 AND guid in
  (select guid from metadata_items where id in
    (select metadata_item_id from media_items where id in
      (select media_item_id from media_parts
        where file='$file'
      )
    )
  );
EOF

    if date="$(sqlite3 -line -init $temp "$db" < /dev/null)"; then
	:
    else
	return 1
    fi

    [ "$date" ] || return 1

    # Convert the date from the Plex db to seconds
    date=$(date -j -f "%Y-%m-%d %H:%M:%S" "$date" +"%s")
    # Now, in seconds
    now=$(date +"%s")

    # The number of hours since this item was viewed 
    hours=$(( ( $now - $date ) / 3600 ))

    if [ $hours -lt $min_hours ]; then
	echo "watched $hours hours ago (< $min_hours)"
	return 1
    fi

    # Meets our time-based criteria for removal...

    if seeding $(basename "$file"); then
        # But ignore it if we're still seeding
	echo "still seeding"
	return 1
    fi

    if [ $hours -gt 24 ]; then
	echo watched $(( $hours / 24 )) days ago
    else
	echo watched $hours hours ago
    fi
    return 0
}

function process_directory()
{
    local header="$2:"
    local files="$(find $1 '(' -name '*.avi' -o -name '*.mp4' -o -name '*.mkv' ')' -print)"
    local hours
    local header
    while read file; do
	if hours=$(watched "$file"); then
	    if [ "$remove" ]; then
		rm "$file"
	    else
		if [ "$header" ]; then
		    echo "$header"
		    header=
		fi
		echo "would remove $(basename "$file")"
		echo "  reason: $hours"
	    fi
	elif [ ! "$hours" ]; then
	    # Not watched
	    :
	elif [ "$verbose" ]; then
	    # Watched, but not ready to remove for some reason
	    echo "would NOT remove $(basename "$file")"
	    echo "  reason: $hours"
	fi
    done <<< "$files"
}

for d in "${!directories[@]}"; do
    process_directory "$d" "${directories[$d]}"
done
