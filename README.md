# tget - t0rrent get

tget grew out of my dissatisfaction with flexget's behavior and
configuration.  There were some things I couldn't get it to do, and
the configuration is clumsy, at best.  Coming from the Lisp world YAML
just sucks.

The main thing I wanted to do, though, was to set the download quality
based on time: wait 6 hours, then download the x264 SD version, but if
after 12 hours that version isn't available, then download the x264
720p version, and if either aren't available for 2 days then download
the XviD SD version.  That's not possible in flexget, and I tried.
Configuration in Lisp, on the other hand, is easy and natural.

tget isn't nearly as functional as flexget.  And, it only works
(currently) with two sites (TVT, BTN).

## Installation

## Configuration

## Putting it into service

## Maintenance tasks

## Usage

The tget options are below.  When there is an argument naming series,
these are canonicalized by removing single quotes and converting to lower
case.

* --help

  Print this help text and exit.

The following are arguments controlling primary behavior:

* --catch-up

  Go through the database and make the newest episode of each series the
  oldest episode that will ever be downloaded for that series; this
  prevents old episodes, which are released from time to time, from being
  downloaded.

* --catch-up-series series-ep

  Catch series up to the episode given in the companion argument.
  See examples below.

* --delete-episodes name

  Delete episodes with series name matching `name`.  This is permanant!
  Using this option with --auto-backup force is recommended.

* --delete-series name

  Delete series with series name matching `name`.  This is permanant!
  Using this option with --auto-backup force is recommended.

* --dump-all

  Dump all `episode` objects to stdout.

* --dump-complete-to

  Dump a table of `complete-to` information for all series in the
  database to stdout.  See --catch-up-series.

* --dump-episodes name

  Dump all `episode` objects matching series name `name` to stdout.

* --dump-series name

  Dump all `series` objects matching series name `name` to stdout.

* --dump-stats

  Dump information about the database to stdout.

The following options augment the options above or have the stated side
effects:

* --archive file

  Save the database to `file`, in XML format for easier archiving.

* --auto-backup {reset|program-update|schema-update|restore|force|never}

  Perform a backup of the database under specific conditions given by the
  companion argument:

  * force - always
  * never - never
  * program-update - when the program changed since the last db update
  * reset - when the database is being reset
  * restore - when a restore is being requested
  * schema-update - when the schema changes

  The default is to make backups for all conditions above.

* --backup-method { copy | save-restore }

  Select the backup method, either by copying the files (fast) or
  saving/restoring (slow); save/restore has the benefit of compacting a
  big database; the default is `copy`.

* --config file

  Load `file` as the configuration file instead of one of the built-in
  defaults.  The default list (searched in this order) is:

  * a file named `config.cl` in the same directory as the program

  * `$HOME/.tget.cl`

  * a file named `config.cl` in the tget data directory (see the --root
    option)

* --cron

  Quiet mode, for cron jobs.

* --db database

  The name of the database.  The default is `db` in the tget data
  directory (see the --root).  This name will itself become a directory.

* --debug

  Run in *debug* mode.  This is recommended for developers only.

* --feed-interval ndays

  Set the feed interval to `ndays`.  Only useful when a user-defined
  function of one argument is given to a `defgroup`'s :rss-url option.
  See the example config file.

* --learn

  Don't download anything--useful in conjunction with reset to wipe the
  database and start over, or when starting to use tget for the first
  time.  See the examples below.

* --reset

  Reset database before beginning operation--this removes *all* data
  from the database.  The default --auto-backup settings will cause a
  backup to be performed before the reset.

* --restore file

  Restore the database from `file`, made with the --archive option.

* --root data-directory

  Change the data directory, the defaults is $HOME/.tget.d/

Examples:

Toss current database and catch up on shows released in the last 180 days
marking them all as `downloaded'

    $ tget --reset --learn --feed-interval 180

Same, but to a `temp' database in the current directory:

    $ tget --reset --learn --feed-interval 180 --root $PWD --db test.db

Usage from Cron:

    $ tget --cron

Let's see what the series object for "Regular Show" looks like.
The series name is not case sensitive:

    $ tget --dump-series "regular show"

These all refer to the same series:

    $ tget --dump-series "james mays man lab"
    $ tget --dump-series "James Mays Man Lab"
    $ tget --dump-series "James May's Man Lab"

To see the episodes of the above, you would:

    $ tget --dump-episodes "James May's Man Lab"

Compact the database and compare the result

    $ tget --auto-backup never --dump-stats --archive archive.before
    $ tget --backup-method save-restore --auto-backup force \
	   --dump-stats --archive archive.after
    $ diff archive.before archive.after

Catch up series to a specific episode:

    #   note that episodes before s04e21 have been downloaded:
    $ tget --catch-up-series "regular show s04e20"
    #   note that all of season 4 has been downloaded:
    $ tget --catch-up-series "breaking bad s04"
