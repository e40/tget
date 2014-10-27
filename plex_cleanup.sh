#! /bin/bash

video_root="/Volumes/me/layer/videos"

db="$HOME/Library/Application Support/Plex Media Server/Plug-in Support/Databases/com.plexapp.plugins.library.db"

set -eu

temp="/tmp/sqlite3$$"
rm -f $temp
trap "/bin/rm -f $temp" 0

function watched()
{
    # ' is escaped in sqlite3 with another '
    file=$(echo "$1" | sed "s/\'/\'\'/g")
    cat <<EOF > $temp
select viewed_at from metadata_item_views where id in
(select id from metadata_item_views where guid in
  (select guid from metadata_items where id in
     (select metadata_item_id from media_items where id in
        (select media_item_id from media_parts
    	   where file='$file'
	)
     )
  )
);
EOF

    # For some reason, there are sometimes multiple dates in viewed_at,
    # so just grab the last one with tail.
    if date="$(sqlite3 -line -init $temp "$db" < /dev/null | tail -1)"; then
	:
    else
	return
    fi

    [ "$date" ] || return 0

    date=$(date -j -f "%Y-%m-%d %H:%M:%S" "$date" +"%s")
    now=$(date +"%s")

    hours=$(( ( $now - $date ) / 3600 ))

    echo Watched $hours hours ago
}

function process_directory()
{
    local header="In $(basename $1):"
    local files="$(find $1 '(' -name '*.avi' -o -name '*.mp4' -o -name '*.mkv' ')' -print)"
    while read file; do
	efile=$(printf '%q' "$file")
	hours=$(watched "$efile")
	if [ "$hours" ]; then
	    if [ "$header" ]; then
		echo "$header"
		header=
	    fi
	    echo "$(basename "$efile"): $hours"
	fi
    done <<< "$files"
}

for d in $video_root/*; do
    case $d in
	*/download.tmp) ;;
	*/anh) ;;
	*)  process_directory $d ;;
    esac
done
