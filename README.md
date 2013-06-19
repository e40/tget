# tget - t0rrent get

tget grew out of my dissatisfaction with flexget's behavior and
configuration.  Don't get me wrong, flexget is an amazing program in
its own right, but there were some things I couldn't get it to do.
And configuration is clumsy, at best.  Coming from the Lisp world,
YAML just sucks.  Go ahead and compare tget's example configuration
file to that of flexget.  The power and flexibility of Lisp make the
comparison a shutout.  Of course, that assumes tget does what you want
it to do.

The main limitation I wanted to overcome by writing tget, though, was
to set the download quality based on time: wait 6 hours (from
publication date), then download the x264 SD version, but if after 12
hours after that a version isn't available, then download the x264
720p version, and if either of those aren't available for 2 days then
download the XviD SD version.  The latter is really my last choice,
because the quality can be pretty crappy.  What I just described is
not possible in flexget.  tget makes this pretty easy.

tget isn't nearly as functional as flexget, though, and the feed
parsing only works (currently) with two sites (TVT, BTN).  I'm looking
at adding more.

### Table of Contents
**[Installation](#installation)**  
**[Configuration](#configuration)**  
**[Putting it into service](#putting-it-into-service)**  
**[Maintenance tasks](#maintenance-tasks)**  
**[Usage](#usage)**  
**[Example configuration file](#example-configuration-file)**  

## Installation

I run tget on Linux.  It should run fine on Mac OS X, as well.

tget depends on two other software packages:

*transmission-remote* -- a program that can communicate to a local
or remote instance of Transmission.  tget uses it to do the work of
download the files.  I run Transmission on Mac OS X, on the same
local area network.  *transmission-remote* makes this easy.

*Allegro Common Lisp* -- For now, at least, no binary packages of tget
are available and you need Allegro Common Lisp to build it.

After cloning the repo, do this to build tget:

    $ make

That should produce a directory `tget/`, which can be installed with:

    $ make install

You can make a `Makefile.local` to override features of the GNU Make
file.

## Configuration

At the highest level, the configuration file defines these entities:

* *transmission-remote settings* - in the config example below I pull
  items from the environment, mainly because I don't want to share
  that private information in this documentation.  You can store yours
  in the configuration file directly.

* *RSS feeds* - this is where tget gets the information on what is
  available to download.  Each feed is site specific and possibly user
  specific.

* *quality settings* - these define the quality of the files you want
  to download.  It's a very important aspect of the configuration, as
  it will directly affect your viewing pleasure.

* *download groups* - if you have multiple people that are watching
  episodes, or multiple sites from which you are downloading, then you
  likely will want to use different groups with differing options for
  each group.  Each group might, for example, have different download
  locations.

* *TV shows you care about* - the `defseries` macro defines the shows
  you want to download.  A series is associated with a group, at a
  minimum, and can override the quality of what you want to download.
  For example, you may want to download *720p* versions of *Top Gear*,
  but would accept a lower resolution of other shows.

That's the big picture.  There is an example configuration file below.
It is fully annotated and is a good place to start.

### `deftransmission &key ...keywords...`

The values for each keyword option (the names after *&key*, given in
the following table) have direct correspondence to
*transmission-remote* command line arguments:

| tget keyword | *transmission-remote* argument |
| :----------: | :----------------------------: |
| `:host` & `:port` | *host:port* |
| `:username` & `:password` | `--auth` *username:password* |
| `:add-paused t` | `--start-paused` |
| `:add-paused nil` | `--no-start-paused` |
| `:trash-torrent-file t` | `--trash-torrent` |
| `:ratio` | `-sr` *ratio* |
| `:download-path` | `--download-dir` *path* |

You can use `(sys:getenv "ENV_VAR")` to grab values from the
environment.

### `defquality &key priority container source codec resolution`

The valid values for each keyword option (the names after *&key*) are:

`:priority` -- any positive integer from 1 to 100.  The higher the
number the higher the priority the that has this quality will be given
if there are multiple matching episodes.

`:container` -- the acceptable containers for the quality.  A container
is, essentially, the file type of the downloaded file (e.g. *mp4*).
Valid values:

    :avi, :mkv, :vob, :mpeg, :mp4, :iso, :wmv, :ts, :m4v, :m2ts

`:source` -- the acceptable source for the quality.  The source is where
the stream originated.  *:hdtv* is a common source.  Valid values:

    :pdtv, :hdtv, :dsr, :dvdrip, :tvrip, :vhsrip, :bluray, :bdrip, :brrip, :dvd5, :dvd9, :hddvd, :web-dl

`:codec` -- the codec used to encode the original source (e.g. *XviD* or
*x264*, aka *h.264*).  Valid values:

    :x264, :h.264, :xvid, :mpeg2, :divx, :dvdr, :vc-1, :wmv, :bd

`:resolution` -- the resolution of the encoded image (e.g. *720p*).
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

`:download-path` -- The path used by *transmission-remote* to store
the downloaded file for this group.  Because the path can be remote,
no checking on the validity of the path is done.

### `defseries name group &key delay quality`

Required arguments:

`name` -- the name of the series.  Case is not significant, and single
quotes are removed in parsing.

`group` -- the group to which this series belongs.

Optional arguments:

`:delay` -- this allows the group delay to be overriden.

`:quality` -- this allows the group quality to be overriden.

## Putting it into service

Running from *cron* is the preferred method of operation.  This
crontab entry will do the job:

    0 3 * * *  source $HOME/.profile; /usr/local/bin/tget --cron

It runs every day at 3AM.  Running more often is fine, but you need to
be careful not to run afoul of the site rules.

If you want the email to go to `username@domain`, then you can specify
the *crontab* entry like this:

    0 3 * * *  source $HOME/.profile; MAILTO=username@domain /usr/local/bin/tget --cron

I source my `$HOME/.profile` (which is sourced by my `$HOME/.bashrc`)
to pick up some environment variables used for the Transmission
interface.

## Maintenance tasks

coming soon

## Usage

The tget options are below.  When there is an argument naming series,
these are canonicalized by removing single quotes and converting to lower
case.

* `--help`

  Print this help text and exit.

The following are arguments controlling primary behavior:

* `--catch-up`

  Go through the database and make the newest episode of each series the
  oldest episode that will ever be downloaded for that series; this
  prevents old episodes, which are released from time to time, from being
  downloaded.

* `--catch-up-series series-ep`

  Catch series up to the episode given in the companion argument.
  See examples below.

* `--delete-episodes name`

  Delete episodes with series name matching `name`.  This is permanant!
  Using this option with --auto-backup force is recommended.

* `--delete-series name`

  Delete series with series name matching `name`.  This is permanant!
  Using this option with --auto-backup force is recommended.

* `--dump-all`

  Dump all `episode` objects to stdout.

* `--dump-complete-to`

  Dump a table of series and last downloaded information for all series in
  the database to stdout.  See --catch-up-series.

* `--dump-episodes name`

  Dump all episode objects matching series name `name` to stdout.

* `--dump-series name`

  Dump all series objects matching series name `name` to stdout.

* `--dump-stats`

  Dump information about the database to stdout.

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

* `--compact-database`

  This saves and restores the database, compacting it at the same time.

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

  Run in debug mode.  This is recommended for developers only.

* `--feed-interval ndays`

  Set the feed interval to `ndays`.  Only useful when a user-defined
  function of one argument is given to a `defgroup`'s :rss-url option.
  See the example config file.

* `--learn`

  Don't download anything--useful in conjunction with reset to wipe the
  database and start over, or when starting to use tget for the first
  time.  See the examples below.

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

Compact the database:

    $ tget --compact-database --dump-stats

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
    
    ;;#+ignore ;; Not really using this and it's a lot of data
    (setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))
    
    ;; A good resource to see why something is or isn't downloading
    (setq *log-file* (merge-pathnames "ep.log" *tget-data-directory*))
    
    (set-torrent-handler
    ;;;;Choose one of the following
     
     ;; Use transmission-remote to tell your torrent client to download the
     ;; episode:
     (make-transmission-remote
      :host (sys:getenv "TRANSMISSION_HOST")
      :port (sys:getenv "TRANSMISSION_PORT")
      :username (sys:getenv "TRANSMISSION_USER")
      :password (sys:getenv "TRANSMISSION_PASS")
      :add-paused nil
      :trash-torrent-file t
      :ratio "1.04")
    
     ;; Specify a directory into which the .torrent files are downloaded, so
     ;; your torrent client can pick them up from there:
     #+ignore
     (pathname "~/Downloads/"))
    
    ;; The old style, deprecated.  However, it still works.
    #+ignore
    (deftransmission ()
        :host (sys:getenv "TRANSMISSION_HOST")
        :port (sys:getenv "TRANSMISSION_PORT")
        :username (sys:getenv "TRANSMISSION_USER")
        :password (sys:getenv "TRANSMISSION_PASS")
        :add-paused nil
        :trash-torrent-file t
        :ratio "1.04")
    
    
    (setq *download-root* "/me/layer/videos/")
    
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
    (defvar *tvt-debug-feed* "tget-test-data/tvt-recent.xml")
    
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
        :codec :x264 
        :resolution :sd)
    
    (defquality :high
        :priority 40
        :source :hdtv
        :codec :x264 
        :resolution :720p)
    
    (defquality :low
        :priority 30
        :source :hdtv
        :codec :xvid
        :resolution :sd)
    
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
    
    (defseries "8 Out of 10 Cats" :kevin)
    (defseries "An Idiot Abroad" :adrian+kevin)
    (defseries "Archer" :kevin)
    (defseries "Bates Motel" :anh+kevin)
    (defseries "Boardwalk Empire" :kevin)
    (defseries "Breaking Bad" :kevin :delay 0) ;; immediate download
    (defseries "Childrens Hospital (US)" :kevin)
    (defseries "Come Fly with Me (2010)" :kevin)
    (defseries "Community" :adrian+kevin)
    (defseries "Curb your Enthusiasm" :anh+kevin)
    (defseries "Dexter" :kevin)
    (defseries "Doc Martin" :anh+kevin)
    (defseries "Downton Abbey" :anh)
    (defseries "Dragons Den (UK)" :kevin)
    (defseries "Eagleheart" :adrian+kevin)
    (defseries "Elementary" :kevin)
    (defseries "Family Tree" :kevin)
    (defseries "Falling Skies" :kevin)
    (defseries "Frontline (US)" :kevin)
    (defseries "Futurama" :adrian+kevin)
    (defseries "Game of Thrones" :kevin :delay 0) ;; immediate download
    (defseries "Hannibal" :anh+kevin)
    (defseries "Homeland" :kevin)
    (defseries "Inside Amy Schumer" :kevin)
    (defseries "James May's Man Lab" :adrian+kevin)
    (defseries "Justified" :kevin)
    (defseries "Kung Fu Panda: Legends of Awesomeness" :adrian)
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
    (defseries "Ridiculousness" :adrian+kevin)
    (defseries "Shark Tank" :adrian+kevin)
    (defseries "Sherlock" :kevin)
    (defseries "Southland" :kevin)
    (defseries "Strike Back" :kevin)
    (defseries "The Americans (2013)" :kevin)
    (defseries "The Colbert Report" :kevin)
    (defseries "The Daily Show with Jon Stewart" :kevin)
    (defseries "The Following" :anh+kevin)
    (defseries "The Good Wife" :anh+kevin)
    (defseries "The Graham Norton Show" :kevin)
    (defseries "The IT Crowd" :kevin)
    (defseries "The Jeselnik Offensive" :kevin)
    (defseries "The Killing" :anh+kevin)
    (defseries "The Mentalist" :adrian+kevin)
    (defseries "The Neighbors (2012)" :adrian+kevin)
    (defseries "The Newsroom (2012)" :kevin)
    (defseries "The Simpsons" :adrian+kevin)
    (defseries "The Ultimate Fighter" :kevin)
    (defseries "The Walking Dead" :kevin :delay 0) ;; immediate download
    (defseries "Top Gear (US)" :adrian+kevin :quality :high)
    (defseries "Top Gear" :adrian+kevin :quality :high)
    (defseries "Top of the Lake" :anh+kevin)
    (defseries "Tosh.0" :kevin)
    (defseries "Vikings" :kevin)
    (defseries "Wallander" :anh+kevin)
    (defseries "White Collar" :anh+kevin)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; BTN
    
    ;; BTN quality is kinda funky and inconsistent.  Define some different
    ;; qualities for shows from them.
    
    (defquality :high-any-source
        :priority 40
        :codec :x264 
        :resolution :720p)
    
    (defquality :x264-?dtv-mp4
        :priority 10
        :container :mp4
        :source '(:pdtv :hdtv)
        :codec :x264)
    
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
