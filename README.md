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

### Table of Contents
**[Installation](#installation)**  
**[Configuration](#configuration)**  
**[Putting it into service](#putting-it-into-service)**  
**[Maintenance tasks](#maintenance-tasks)**  
**[Usage](#usage)**  
**[Example Configuration](#example-configuration)**  

## Installation

coming soon

## Configuration

coming soon

## Putting it into service

coming soon

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

* `--archive file`

  Save the database to `file`, in XML format for easier archiving.

* `--auto-backup {reset|program-update|schema-update|restore|force|never}`

  Perform a backup of the database under specific conditions given by the
  companion argument:

  * `force` - always
  * `never` - never
  * `program-update` - when the program changed since the last db update
  * `reset` - when the database is being reset
  * `restore` - when a restore is being requested
  * `schema-update` - when the schema changes

  The default is to make backups for all conditions above.

* `--backup-method { copy | save-restore }`

  Select the backup method, either by copying the files (fast) or
  saving/restoring (slow); save/restore has the benefit of compacting a
  big database; the default is `copy`.

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

* `--restore file`

  Restore the database from `file`, made with the --archive option.

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

## Example configuration file

    
    (in-package :user)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; General options
    
    ;;;;Not really using this, and it's a lot of data:
    ;;(setq *log-rss* (merge-pathnames "rss.log" *tget-data-directory*))
    (setq *log-file* (merge-pathnames "ep.log" *tget-data-directory*))
    
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
    
    ;; Wait 6 hours before downloading (most) episodes, to wait for repacks and
    ;; propers.
    (defvar *tvt-delay* 6)
    
    (setq *feed-interval* 14)
    
    (defun tvt-rss-feed (interval)
      (format nil "~a&interval=~d+days"
    	  ;; This is the "Recent torrents" feed instead of the "Favorite
    	  ;; shows" feed I was using before.
    	  "http://www.tvtorrents.com/..."
    	  interval))
    
    (defvar *tvt-rss* 'tvt-rss-feed)
    (defvar *tvt-debug-feed* "tget-test-data/tvt-recent.xml")
    
    (defquality :normal
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
    (defseries "Falling Skies" :kevin)
    (defseries "Frontline" :kevin)
    (defseries "Futurama" :adrian+kevin)
    (defseries "Game of Thrones" :kevin :delay 0) ;; immediate download
    (defseries "Homeland" :kevin)
    (defseries "James May's Man Lab" :adrian+kevin)
    (defseries "Justified" :kevin :delay 0) ;; immediate download
    (defseries "Kung Fu Panda: Legends of Awesomeness" :adrian)
    (defseries "Longmire" :kevin)
    (defseries "Louis Theroux Documentaries" :kevin)
    (defseries "Louie" :kevin)
    (defseries "Luther" :kevin)
    (defseries "Mad Men" :kevin)
    (defseries "Midsomer Murders" :anh)
    (defseries "Misfits" :kevin)
    (defseries "Modern Family" :adrian+kevin)
    (defseries "Motive" :kevin)
    (defseries "Mythbusters" :adrian+kevin)
    (defseries "NCIS" :adrian+kevin)
    (defseries "Nathan for You" :adrian+kevin)
    (defseries "Nova" :adrian+kevin)
    (defseries "Oliver Stone's Untold History of the United States" :adrian+kevin)
    (defseries "Orphan Black" :kevin)
    (defseries "Parks and Recreation" :adrian+kevin)
    (defseries "Person of Interest" :kevin)
    (defseries "Phineas and Furb" :adrian)
    (defseries "Ridiculousness" :adrian+kevin)
    (defseries "Shameless (US)" :kevin)
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
