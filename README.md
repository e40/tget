# tget 2.1 - torrent get

_tget_ grew out of my dissatisfaction with [FlexGet][2]'s behavior and
configuration.  Don't get me wrong, [FlexGet][2] is an amazing program in
its own right, but there were some things I couldn't get it to do.
And configuration is clumsy, at best.  Coming from the Lisp world,
YAML just sucks.  Go ahead and compare _tget_'s example configuration
file to that of [FlexGet][2].  The power and flexibility of Lisp make the
comparison a shutout.  Of course, that assumes _tget_ does what you want
it to do.

The main limitation I wanted to overcome by writing _tget_, though, was
to set the download quality based on time: wait 6 hours (from
publication date), then download the x264 SD version, but if after 12
hours after that a version isn't available, then download the x264
720p version, and if either of those aren't available for 2 days then
download the XviD SD version.  The latter is really my last choice,
because the quality can be pretty crappy.  What I just described is
not possible in [FlexGet][2].  _tget_ makes this pretty easy.

_tget_ isn't nearly as functional as [FlexGet][2], though, and the feed
parsing only works (currently) with two sites (TVT, BTN).  I'm looking
at adding more.

### Table of Contents
**[How it works](#how-it-works)**  
**[Installation](#installation)**  
**[Getting started](#getting-started)**  
**[Configuration](#configuration)**  
**[Maintenance tasks](#maintenance-tasks)**  
**[Usage](#usage)**  
**[Example configuration file](#example-configuration-file)**  

## How it works

_tget_ has these basic phases:

* parsing the configuration file,

* fetching the RSS feeds defined by the configuration file, and

* downloading torrent files for episodes never seen before, which meet
  the criteria defined by the configuration file.

To accomplish the above, _tget_ maintains a database of series and
episodes, with various data about them.

Here are some details about the above phases:

* The configuration file defines which series to monitor.  When the
  configuration file is read, entries are created in the database for
  each series.  You can examine this stored information with command
  line arguments to _tget_.

* When an RSS feed is fetched, any episodes related to series defined
  by the configuration file, and thus in the database, are stored in
  the database, but marked as _not downloaded_.

* Each series in the database has a `complete-to` slot.  This tells
  _tget_ what episode number the series is completed to, and _tget_ will
  never download anything at or before this episode.

  Say you've downloaded all the episodes of _Breaking Bad_ up to
  S05E08.  Any episodes which appear in the RSS feed at or before this
  episode will be ignored and never downloaded.

  There are command line arguments, which start with `--catch-up`,
  which can be used to manipulate the `complete-to` slot of a series.

* Episodes are sometimes downloaded out of order.  You will not miss
  an episode of a series if it appears in the RSS feed out of order.

  Say the `complete-to` for a series is S05E08, and episode S05E10
  appears on one invocation of _tget_ and S05E09 appears in a later
  invocation.  Both episodes will be downloaded and the `complete-to`
  will not be updated to S05E10 until after episode S05E09 is
  downloaded.

* When the torrent file is downloaded, the resulting file can be given
  to _transmission-remote_ for processing (_transmission-remote_ is a
  command line tool used to control [Transmission][1], the
  cross-platform BitTorrent client) or put in a specific directory
  watched by your BitTorrent client.  See the example configuration
  file for information on how to use either of these methods.

## Installation

_tget_ has been tested on Linux and Mac OS X and depends on two other
software packages:

_transmission-remote_ -- [OPTIONAL] a program that can communicate to a local or
remote instance of Transmission.  _tget_ uses it to download the actual
episodes .  I run [Transmission][1] on Mac OS X, on the same local
area network.  _transmission-remote_ makes this easy.  On CentOS and
Fedora, install the `transmission-common` rpm.  On a Mac, I suggest
looking to [MacPorts][3].

_Allegro Common Lisp_ -- For now, at least, no binary packages of _tget_
are available and you need Allegro Common Lisp to build it.

After cloning the repo, build _tget_:

    $ make

That should produce a directory `tget/`, which can be installed with:

    $ make install

You can make a `Makefile.local` to override features of the GNU Make
file.

## Getting started

After installing the software, the next thing you need is a
configuration file.  See the next section for information on
configuration, and the section at the end for an example configuration
file, which you can edit for your needs.

The database used by _tget_ is stored in `$HOME/.tget.d`.  You can
change this location with the `--root` command line argument.

Next, you probably want to create a new database which has everything
you care about marked as _already downloaded_.  The following command 
will start from scratch and, assuming the RSS feed you are using
supports it, mark 6 months of episodes as already downloaded:

    $ tget --reset --learn --feed-interval 180

Next, you'll want to set up _cron_ to periodically run _tget_, perhaps
every day at 3AM:

    0 3 * * *  source $HOME/.bashrc; MAILTO=username@domain /usr/local/bin/tget --cron

Running more often is fine, but you need to be careful not to run
afoul of the site rules for how often you can download their RSS feed.
You can use the `MAILTO` environment variable to change the user that
receives the output of _tget_.  `$HOME/.bashrc` is sourced to pick up
some environment variables used by the example configuration file.
You can omit this part if you do not need any environment variables.

## Configuration

At the highest level, the configuration file defines these entities:

* _transmission-remote settings_ - in the config example below I pull
  items from the environment, mainly because I don't want to share
  that private information in this documentation.  You can store yours
  in the configuration file directly.

* _RSS feeds_ - this is where _tget_ gets the information on what is
  available to download.  Each feed is site specific and possibly user
  specific.

* _quality settings_ - these define the quality of the files you want
  to download.  It's a very important aspect of the configuration, as
  it will directly affect your viewing pleasure.

* _download groups_ - if you have multiple people that are watching
  episodes, or multiple sites from which you are downloading, then you
  likely will want to use different groups with differing options for
  each group.  Each group might, for example, have different download
  locations.

* _TV shows you care about_ - the `defseries` macro defines the shows
  you want to download.  A series is associated with a group, at a
  minimum, and can override the quality of what you want to download.
  For example, you may want to download _720p_ versions of _Top Gear_,
  but would accept a lower resolution of other shows.

That's the big picture.  There is an example configuration file below.
It is fully annotated and is a good place to start.

### `set-torrent-handler handler`

Define how _.torrent_ files will be handled.  There are two choices:
have _transmission-remote_ handle them or define a directory in which
the _.torrent_ files are placed.  For the latter, the configuration
file option would be:

    (set-torrent-handler (pathname "~/Downloads/"))

This would cause _tget_ to store all downloaded _.torrent_ files in
the directory `~/Downloads/`.

The second choice is to use `make-tranmission-remote-handler` to
define how to communicate with _transmission-remote_.

### `make-transmission-remote-handler ...options...`

The values for each option have direct correspondence to
_transmission-remote_ command line arguments, given by the table
below.  Each _keyword_ given in the table must be paired with a value,
like this:

    :host "download.example.com"

The `:host` part must come before the value.

| keyword(s)                | _transmission-remote_ argument |
| :-----------------------: | :----------------------------: |
| `:host` & `:port`         | _host:port_                    |
| `:username` & `:password` | `--auth` _username:password_   |
| `:add-paused t`           | `--start-paused`               |
| `:add-paused nil`         | `--no-start-paused`            |
| `:trash-torrent-file t`   | `--trash-torrent`              |
| `:ratio`                  | `-sr` _ratio_                  |
| `:download-path`          | `--download-dir` _path_        |

For example:

    (set-torrent-handler
     (make-transmission-remote-handler
      :host (sys:getenv "TRANSMISSION_HOST")
      :port (sys:getenv "TRANSMISSION_PORT")
      :username (sys:getenv "TRANSMISSION_USER")
      :password (sys:getenv "TRANSMISSION_PASS")
      :add-paused nil
      :trash-torrent-file t
      :ratio "1.04"))

In this example, the values for `:host` and other options are being
pulled from the environment with `sys:getenv`.

### `defquality &key priority container source codec resolution`

The valid values for each keyword option (the names after _&key_) are:

`:priority` -- any positive integer from 1 to 100.  The higher the
number the higher the priority the that has this quality will be given
if there are multiple matching episodes.

`:container` -- the acceptable containers for the quality.  A container
is, essentially, the file type of the downloaded file (e.g. _mp4_).
Valid values:

    :avi, :mkv, :vob, :mpeg, :mp4, :iso, :wmv, :ts, :m4v, :m2ts

`:source` -- the acceptable source for the quality.  The source is where
the stream originated.  _:hdtv_ is a common source.  Valid values:

    :pdtv, :hdtv, :dsr, :dvdrip, :tvrip, :vhsrip, :bluray, :bdrip, :brrip, :dvd5, :dvd9, :hddvd, :web-dl

`:codec` -- the codec used to encode the original source (e.g. _XviD_ or
_x264_, aka _h.264_).  Valid values:

    :x264, :h.264, :xvid, :mpeg2, :divx, :dvdr, :vc-1, :wmv, :bd

`:resolution` -- the resolution of the encoded image (e.g. _720p_).
Valid values:

    :sd, :720p, :1080p, :1080i

### `defgroup name &key rss-url debug-feed delay ratio quality download-path`

`name` -- the name of the group, a keyword (e.g. :bruce).

`:rss-url` -- the URL of the RSS feed.

`:debug-feed` -- a file name containing the static XML to be used in
debug mode instead of the fetching it from the URL (given by
:rss-url).

`:delay` -- nil or a positive integer, which represents the delay, in
hours, that episodes should be delayed from download.  **NOTE:** this
is distinct from any site-specific delay that might be available in
the RSS feed.

`ratio` -- the share ratio given to all downloads for this group.
It's a string not a floating point number, so we don't have to worry
about the printed representation of floats.  It no ratio is given,
then the default in Transmission is used.  A ratio of "-1" means seed
forever.

`:quality` -- a user-defined quality symbol.  See `defquality`.

`:download-path` -- The path used by _transmission-remote_ to store
the downloaded file for this group.  Because the path can be remote,
no checking on the validity of the path is done.

### `defseries name group &key delay quality catch-up`

Required arguments:

`name` -- the name of the series.  Case is not significant, and single
quotes are removed in parsing.

`group` -- the group to which this series belongs.

Optional arguments:

`:delay` -- this allows the group delay to be overriden.

`:quality` -- this allows the group quality to be overriden.

`:catch-up` -- do not download any episodes of this series at or
before this season and episode.  An example value is `S05E08`.  Case
differences are ignored (e.g., you can use `s` or `S`).

## Maintenance tasks

## Maintenance task: making the database smaller

After some time, _tget_ can slow down due to the database growing.  It
has to do with how _tget_ operates, temporarily storing episodes while
it is deciding what to download.  These temporarily-stored episodes
are deleted almost immediately, but they leave _holes_ in the
database.  To clean them out, you can

    $ tget --compact-database

This will backup the current database and then compact it to make it
more efficient.  It will usually dramatically decrease the size of the
database, if it's been in use for a month or more.

## Maintenance task: adding a new series

The easiest way to add a new series is to add it to the configuration
file, like this:

    (defseries "New Series" :group1)

However, sometimes you might want to add it in a caught up state.
That would be:

    (defseries "New Series" :group1 :catch-up "S05E08")

Which would cause episodes from `S05E09` and later to be downloaded,
and nothing before it.

## Maintenance task: removing a series

There are two ways you can remove a series.

The first method entails removing it from the configuration file
and then running _tget_ to remove it from the database.  The removing
from the database part would be:

    $ tget --delete-series "True Blood"

The second method entails changing the configuration file to note this
series should be removed:

    (defseries "Series Name" :group1 :remove t)

You can remove this entry in the configuration file after _tget_ has
been run once.

## Usage

Primary behavior determining arguments (one of these must be given):

    --run
    --catch-up   
    --catch-up-series series-episode-name
    --check-database
    --clean-database
    --compact-database
    --delete-episodes series-name
    --delete-series series-name
    --dump-all
    --dump-complete-to
    --dump-episodes series-name
    --dump-series series-name
    --dump-stats
    --skip

Behavior modifying arguments:

    --auto-backup condition
    --config file
    --cron
    --db database-name
    --debug
    --feed-interval ndays
    --learn
    --reset
    --root data-directory

### Usage details

The tget options are below.  When there is an argument naming series,
these are canonicalized by removing single quotes and converting to lower
case.  However, the series names presented to you will be stored in their
original form.

* `--help`

  Print full help text and exit.

The following are arguments controlling primary behavior:

* `--run`

  The primary mode of operation, whereby RSS feeds are retrieved, searched
  for new episodes and those episode torrents downloaded.

* `--catch-up`

  Go through the database and make the newest episode of each series the
  oldest episode that will ever be downloaded for that series; this
  prevents old episodes, which are released from time to time, from being
  downloaded.

* `--catch-up-series series-ep`

  Catch series up to the episode given in the companion argument.
  See examples below.

* `--check-database`

  Report on items in the database which can be cleaned up.

* `--clean-database`

  Remove items reported by `--check-database`.

* `--compact-database`

  This saves and restores the database, compacting it at the same time.

* `--delete-episodes series-name`

  Delete episodes with series name matching `series-name`.  This is permanant!
  Using this option with --auto-backup force is recommended.

* `--delete-series series-name`

  Delete series with series name matching `series-name`.  This is permanant!
  Using this option with --auto-backup force is recommended.

* `--dump-all`

  Dump all `episode` objects to stdout.

* `--dump-complete-to`

  Dump a table of series and last downloaded information for all series in
  the database to stdout.  See --catch-up-series.

* `--dump-episodes series-name`

  Dump all episode objects matching series name `series-name` to stdout.

* `--dump-series series-name`

  Dump all series objects matching series name `series-name` to stdout.

* `--dump-stats`

  Dump information about the database to stdout.

* `--skip series-name`

  Skip the next episode of `series-name`.  It does so by using the last
  downloaded episode and incrementing it by 1.

The following options augment the options above or have the stated side
effects:

* `--auto-backup {reset|program-update|schema-update|restore|force|never}`

  Perform a backup of the database under specific conditions given by the
  companion argument:

  * `compact` - when the database is being compacted
  * `force` - always
  * `never` - never
  * `program-update` - when the program changed since the last db update
  * `reset` - when the database is being reset
  * `schema-update` - when the schema changes

  The default is to make backups for all conditions above.

* `--config file`

  Load `file` as the configuration file instead of one of the built-in
  defaults.  The default list (searched in this order) is to load the first
  file found in the list:

  * `config.cl` in the same directory as the program
  * `$HOME/.tget.cl`
  * `config.cl` in the tget data directory (see the --root option)

* `--cron`

  Quiet mode, for cron jobs.

* `--db database`

  The name of the database.  The default is `db` in the tget data
  directory (see the --root).  This name will itself become a directory.

* `--debug`

  Run in debug mode.  In this mode, torrents are not downloaded and the
  debug feed defined by the configuration file is used.  Also, the program
  is more verbose.

* `--feed-interval ndays`

  Set the feed interval to `ndays`.  Only useful when a user-defined
  function of one argument is given to a `defgroup`'s :rss-url option.
  See the example config file.

* `--learn`

  Mark episodes which match the configuration as _downloaded_.  This is
  useful when using tget for the first time.  See the examples below.

* `--reset`

  Reset database before beginning operation--this removes all data
  from the database.  The default --auto-backup settings will cause a
  backup to be performed before the reset.

* `--root data-directory`

  Change the data directory, the defaults is $HOME/.tget.d/

Examples:

Toss current database and catch up on shows released in the last 180 days
marking them all as `downloaded'

    $ tget --reset --learn --feed-interval 180

Same, but to a `temp' database in the current directory:

    $ tget --reset --learn --feed-interval 180 --root $PWD --db test.db

Let's see what the series object for "Regular Show" looks like.
The series name is not case sensitive:

    $ tget --dump-series "regular show"

These all refer to the same series:

    $ tget --dump-series "james mays man lab"
    $ tget --dump-series "James Mays Man Lab"
    $ tget --dump-series "James May's Man Lab"

To see the episodes of the above, you could use any of the variations on
the names given, for example:

    $ tget --dump-episodes "James May's Man Lab"

Compact the database:

    $ tget --compact-database

Catch up series to a specific episode:

    #   note that episodes before s04e21 have been downloaded:
    $ tget --catch-up-series "regular show s04e20"
    #   note that all of season 4 has been downloaded:
    $ tget --catch-up-series "breaking bad s04"

## Example configuration file

    ;; Config file for tget
    
    (in-package :user)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; General options
    
    ;; This is now reset once per run, so it's not too much data to save,
    ;; and it's good for debugging feed issues.
    (setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))
    
    ;; A good resource to see why something is or isn't downloading
    (setq *log-file* (merge-pathnames "ep.log" *tget-data-directory*))
    
    ;; Use transmission-remote to tell your torrent client to download the
    ;; episode:
    (set-torrent-handler
     (make-transmission-remote-handler
      :host (sys:getenv "TRANSMISSION_HOST")
      :port (sys:getenv "TRANSMISSION_PORT")
      :username (sys:getenv "TRANSMISSION_USER")
      :password (sys:getenv "TRANSMISSION_PASS")
      :add-paused nil
      :trash-torrent-file t
      :ratio "1.04"))
    
    ;; An alternate method for downloading .torrent files: put them into a
    ;; specific directory, where your torrent client will see them.
    #+ignore
    (set-torrent-handler (pathname "~/Downloads/"))
    
    
    (setq *download-root* "/me/layer/videos/")
    
    (defvar *codec-x264*
        ;; It goes by two different names:
        '(:x264 :h.264))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; TVT
    
    ;; Not all sites support the idea of a delay, but TVT does.
    ;; It's a nice feature, because it allows you to delay "seeing" RSS items,
    ;; to give the various downloads time to settle.  It reduces the chances of
    ;; having to download repacks or propers (another name for repacks).
    ;;
    ;; I like a 6 hour delay.
    (defvar *tvt-delay* 6)
    
    ;; Not all sites support the idea of a feed interval, but TVT does.
    ;; It's a nice feature, because if you decide to download a new series,
    ;; you'll get any episodes released in this period of time.  And, for the
    ;; initial installation, you can specify a really high interval (on the
    ;; command line, not here), to populate your database with your shows.
    (setq *feed-interval* 14)
    
    ;; This function is given as the value of the defgroup :rss-url option.
    ;; The function is called, when tget needs to fetch the feed, with the
    ;; value *feed-interval* or the command line override for that variable
    ;; (the --interval argument).
    (defun tvt-rss-feed (interval)
      (format nil "~a&interval=~d+days"
    	  ;; This is the "Recent torrents" feed instead of the "Favorite
    	  ;; shows" feed I was using before.
    	  ;;
    	  ;; Using "Favorite shows" feed means you have to maintain your
    	  ;; list of shows in *two* places, which I find very annoying.
    	  ;;
    	  "http://www.tvtorrents.com/..."
    	  interval))
    
    (defvar *tvt-rss* 'tvt-rss-feed)
    
    ;; When --debug is given on the command line, the debug version is used,
    ;; and that's what this is.  No need to bombard the RSS server with
    ;; requests while debugging.
    (defvar *tvt-debug-feed* "tget-test-data/tvt.xml")
    
    ;; This is how you define names for qualities you care about.
    ;;
    (defquality :normal
        ;; The priority of a quality allows selection of episodes when more
        ;; than one quality is available at the same time, as is often the
        ;; case.  Higher numerical priority is given precedence.
        ;;
        ;; This is my preferred quality.
        
    ;;;; The documentation for these options is in the README.md file.
        :priority 50
        :source :hdtv
        :codec *codec-x264* 
        :resolution :sd)
    
    (defquality :high
        :priority 40
        :source :hdtv
        :codec *codec-x264* 
        :resolution :720p)
    
    (defquality :low
        :priority 30
        :source :hdtv
        :codec :xvid
        :resolution :sd)
    
    (defquality :indi :resolution :sd)
    
    ;; This is a user-defined quality function.
    
    (defun my-quality (episode)
      ;; Download :normal quality immediately.
      ;; If that's not available, then download :high quality after 12 hours
      ;; and :low quality after 2 days.  There are still some shows that never
      ;; get anything but :low, for some episodes.
      (if* (and (null
    	     ;; See if there is an episode with :normal quality.  The
    	     ;; :transient keyword is important, since it restricts the
    	     ;; search to episodes we have *not* downloaded yet.
    	     (query-episode :episode episode :quality :normal :transient t))
    	    (eq :high (episode-quality episode))
    	    (>= (hours-available episode) 12))
         then ;; :normal quality is not available and the :high quality episode
    	  ;; has been available for 12 hours, then return...
    	  :high
       elseif (and (null
    		;; See if there is an episode with :normal or :high
    		;; quality.
    		(or
    		 (query-episode :episode episode :quality :normal :transient t)
    		 (query-episode :episode episode :quality :high :transient t)))
    	       (eq :low (episode-quality episode))
    	       (>= (hours-available episode) 48))
         then :low
         else :normal))
    
    (defgroup :adrian
        :rss-url *tvt-rss*
        :debug-feed *tvt-debug-feed*
        :delay *tvt-delay*
        :quality 'my-quality
        :download-path (merge-pathnames "adrian/" *download-root*))
    
    (defgroup :anh
        :rss-url *tvt-rss*
        :debug-feed *tvt-debug-feed*
        :delay *tvt-delay*
        :quality 'my-quality
        :download-path (merge-pathnames "anh/" *download-root*))
    
    (defgroup :kevin
        :rss-url *tvt-rss*
        :debug-feed *tvt-debug-feed*
        :delay *tvt-delay*
        :quality 'my-quality
        :download-path (merge-pathnames "kevin/" *download-root*))
    
    (defgroup :adrian+kevin
        :rss-url *tvt-rss*
        :debug-feed *tvt-debug-feed*
        :delay *tvt-delay*
        :quality 'my-quality
        :download-path (merge-pathnames "adrian+kevin/" *download-root*))
    
    (defgroup :anh+kevin
        :rss-url *tvt-rss*
        :debug-feed *tvt-debug-feed*
        :delay *tvt-delay*
        :quality 'my-quality
        :download-path (merge-pathnames "anh+kevin/" *download-root*))
    
    ;; Use ... :catch-up "S01E02" ... to start a series after the 1st ep
    ;; Use ... :remove t ... to delete a series
    
    (defseries "8 Out of 10 Cats" :kevin)
    (defseries "Almost Human" :kevin)
    (defseries "An Idiot Abroad" :adrian+kevin)
    (defseries "Archer" :kevin)
    (defseries "Bates Motel" :anh+kevin)
    (defseries "Bear Grylls: Escape From Hell" :kevin :catch-up "S01E05")
    (defseries "Black Mirror" :kevin)
    (defseries "Boardwalk Empire" :kevin)
    (defseries "Brooklyn Nine-Nine" :kevin)
    (defseries "Childrens Hospital (US)" :kevin)
    (defseries "Come Fly with Me (2010)" :kevin)
    (defseries "Community" :adrian+kevin)
    (defseries "Curb your Enthusiasm" :anh+kevin)
    (defseries "Doc Martin" :anh+kevin)
    (defseries "Downton Abbey" :anh)
    (defseries "Dracula (2013)" :kevin)
    (defseries "Dragons Den (UK)" :kevin)
    (defseries "Eagleheart" :adrian+kevin)
    (defseries "Elementary" :kevin)
    (defseries "Family Tree" :kevin)
    (defseries "Frontline (US)" :kevin)
    (defseries "Game of Thrones" :kevin :delay 0) ;; immediate download
    (defseries "Hannibal" :anh+kevin)
    (defseries "Hell on Wheels" :kevin)
    (defseries "Homeland" :kevin :delay 0) ;; immediate download
    (defseries "James May's Man Lab" :adrian+kevin)
    (defseries "Justified" :kevin)
    (defseries "Kung Fu Panda: Legends of Awesomeness" :adrian
      ;; All there is on TVT
      :quality :indi)
    (defseries "Longmire" :kevin)
    (defseries "Louis Theroux Documentaries" :kevin)
    (defseries "Louie" :kevin)
    (defseries "Luther" :kevin)
    (defseries "Mad Men" :kevin)
    (defseries "Maron" :kevin)
    (defseries "Midsomer Murders" :anh)
    (defseries "Misfits" :kevin)
    (defseries "Modern Family" :adrian+kevin)
    (defseries "Motive" :kevin)
    (defseries "Mythbusters" :adrian+kevin)
    (defseries "NCIS" :adrian+kevin)
    (defseries "Nathan for You" :adrian+kevin)
    (defseries "Nova" :adrian+kevin)
    (defseries "Oliver Stone's Untold History of the United States" :adrian+kevin)
    (defseries "Parks and Recreation" :adrian+kevin)
    (defseries "Person of Interest" :kevin)
    (defseries "Phineas and Furb" :adrian)
    (defseries "Ray Donovan" :kevin)
    (defseries "Ridiculousness" :adrian+kevin)
    (defseries "Shark Tank" :adrian+kevin)
    (defseries "Sherlock" :kevin)
    (defseries "The Americans (2013)" :kevin)
    (defseries "The Blacklist" :adrian+kevin)
    (defseries "The Burn" :kevin)
    (defseries "The Colbert Report" :kevin)
    (defseries "The Daily Show with Jon Stewart" :kevin)
    (defseries "The Good Wife" :anh+kevin)
    (defseries "The Graham Norton Show" :kevin)
    (defseries "The IT Crowd" :kevin)
    (defseries "The Jeselnik Offensive" :kevin)
    (defseries "The Mentalist" :adrian+kevin)
    (defseries "The Neighbors (2012)" :adrian+kevin)
    (defseries "The Newsroom (2012)" :kevin)
    (defseries "The Simpsons" :adrian+kevin)
    (defseries "The Ultimate Fighter" :kevin)
    (defseries "The Walking Dead" :kevin :delay 0) ;; immediate download
    (defseries "The White Queen" :anh+kevin)
    (defseries "Top Gear (US)" :adrian+kevin :quality :high)
    (defseries "Top Gear" :adrian+kevin :quality :high)
    (defseries "Top of the Lake" :anh+kevin)
    (defseries "Tosh.0" :kevin)
    (defseries "Vikings" :kevin)
    (defseries "Wallander" :anh+kevin)
    (defseries "White Collar" :anh+kevin)
    (defseries "Would I Lie To You" :adrian+kevin)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; BTN
    
    ;; BTN quality is kinda funky and inconsistent.  Define some different
    ;; qualities for shows from them.
    
    (defquality :high-any-source
        :priority 40
        :codec *codec-x264*
        :resolution :720p)
    
    (defquality :x264-?dtv-mp4
        :priority 10
        :container :mp4
        :source '(:pdtv :hdtv)
        :codec *codec-x264*)
    
    (defvar *btn-my-series-feed*
        "https://broadcasthe.net/...")
    
    (defvar *btn-debug-feed* "tget-test-data/btn.xml")
    
    (defgroup :btn-adrian+kevin
        :rss-url *btn-my-series-feed*
        :debug-feed *btn-debug-feed*
        :ratio "-1" 
        :quality :normal
        :download-path (merge-pathnames "adrian+kevin/" *download-root*))
    
    (defgroup :btn-kevin
        :rss-url *btn-my-series-feed*
        :debug-feed *btn-debug-feed*
        :ratio "-1" 
        :quality :normal
        :download-path (merge-pathnames "kevin/" *download-root*))
    
    (defseries "Regular Show" :btn-adrian+kevin :quality :high-any-source)
    (defseries "Spongebob Squarepants" :btn-adrian+kevin :quality :high-any-source)
    (defseries "World's Craziest Fools" :btn-adrian+kevin :quality :x264-?dtv-mp4)
    (defseries "Witness (2012)" :btn-kevin :quality :x264-?dtv-mp4)

[1]: http://www.transmissionbt.com/   "Transmission"
[2]: http://flexget.com/              "FlexGet"
[3]: http://www.macports.org/         "MacPorts"
