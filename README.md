# tget 4.5.0 - torrent get

_tget_ is a suite of programs: _tget_, _tcleanup_ and _plexfix_.

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
parsing only works (currently) with two sites (TvT and BTN).
I'm always looking to add more.

_tcleanup_ has a dual use: remove torrents from _transmission_ after
they have seeded enough and to remove programs watched by Plex.

_plexfix_ works around deficiencies in Plex, which doesn't recognize
certain filename formats.  For example, `Foo-612.mp4` would not be
recognized by Plex, perhaps because the naming convention is
ambiguous--is it season 6 and espisode 12 or the June 12th episode of
_Foo_?  Plex punts of filenames like this.  _plexfix_ creates symbolic
links so that Plex can see the correct season and episode.

### Table of Contents
**[How it works](#how-it-works)**  
**[Installation](#installation)**  
**[Getting started](#getting-started)**  
**[Configuration](#configuration)**  
**[Maintenance tasks](#maintenance-tasks)**  
**[tcleanup](#tcleanup)**  
**[plexfix](#plexfix)**  
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
| `:docker-container-name`  | See below                      |
| `:docker-user`            | See below                      |
| `:ssh-user`               | See below                      |
| `:ssh-identity`           | See below                      |

When a subdirectory is specified by defseries, _tget_ must ensure it
exists before telling _transmission_ to download into it.  There are
two methods for _tget_ communicating with _transmission_: via _docker_
or SSH.

You may specify one of the group of arguments, `:docker-*` or
`:ssh-*`, but not both.

`:docker-container-name` and `:docker-user` are the container's name
and user inside the container (for `su`) to create directories.

`:ssh-user` and `:ssh-identity` are the user and identity file to
create directories.

SSH example:

    (set-torrent-handler
     (make-transmission-remote-handler
      :host (sys:getenv "TRANSMISSION_HOST")
      :port (sys:getenv "TRANSMISSION_PORT")
      :username (sys:getenv "TRANSMISSION_USER")
      :password (sys:getenv "TRANSMISSION_PASS")
      :ssh-user (sys:getenv "TRANSMISSION_SSH_USER")
      :ssh-identity (sys:getenv "TRANSMISSION_SSH_IDENTITY")
      :add-paused nil
      :trash-torrent-file t
      :ratio "1.04"))

In this example, the values for `:host` and other options are being
pulled from the environment with `sys:getenv`.

Docker example:

    (set-torrent-handler
     (make-transmission-remote-handler
      ...
      :docker-container-name "transmission"
      :docker-user (sys:getenv "USER")
      ...))

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

### `deftracker name &key url debug-feed public download-delay disabled ratio upload-limit`

`name` -- the name of the tracker, a keyword (e.g. :eztv).

`url` -- the URL that points to an RSS feed for this tracker.  If you
require a password to access the tracker's RSS feed, it would need to
be contained in the URL.

`debug-feed` -- a filename which is to be substituted for the actual
URL, for debugging purposes.

`public` -- a boolean which indicates if this tracker is public or
not.

`download-delay` -- the delay imposed on downloads for this tracker.
This option allows you to give priority to different trackers, based
of time.

`disabled` -- a boolean indicating if the tracker has been disabled.
It is useful when trackers go offline for extended periods of time.

`ratio` -- the ratio to be applied to this specific tracker.  It takes
precedence over the global ratio specified for Transmission, if that
is used.

`upload-limit` -- set the upload limit for this tracker.  This is
useful for when the tracker unreliably gives you upload credit for
what you upload (I'm looking at you TvT!).  Sometimes, if you upload
too fast the tracker will not count a significant amount of your upload
data.

### `defgroup name &key rss-url trackers delay ratio quality download-path`

`name` -- the name of the group, a keyword (e.g. :bruce).

`:trackers` -- a list of trackers which apply to this group.

`:rss-url` -- the URL of the RSS feed.  **This option has been
deprecated in favor of the `:trackers` option.**

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

### `defseries name group &key private delay quality catch-up subdir date-based aliases`

Required arguments:

`name` -- the name of the series.  Case is not significant, and single
quotes are removed in parsing.

`private` -- a boolean indicating this series should only be
downloaded from a tracker listed as private.

`group` -- the group to which this series belongs.

Optional arguments:

`:delay` -- this allows the group delay to be overriden.

`:quality` -- this allows the group quality to be overriden.

`:catch-up` -- do not download any episodes of this series at or
before this season and episode.  An example value is `S05E08`.  Case
differences are ignored (e.g., you can use `s` or `S`).

`:subdir` -- put episodes of this series into a subdirectory of the
group download path.  This is a workaround for limitations in Plex
Media Server, which will not recognize episodes of shows with dates in
their names instead of episode numbers.  _The Daily Show_ and
_The Colbert Report_ are two examples of these types of shows.

`:date-based` -- indicate this series is date based and turn off the
accounting for complete seasons.  This is useful for shows like
_The Daily Show_ and _The Colbert Report_, which don't have seasons
and the episodes for these are ordered by date.

`:aliases` -- a list of aliases for `name`. This feature exists
because many sites use different names for series, `The Daily Show
with Jon Stewart` vs. `The Daily Show`.
Here's how you specify it:

    :aliases ("The Daily Show")

    #<Printer Error, obj=#x10000000af9: `86400' is not of the expected type `list'>
This value is the number of seconds after an episode is downloaded
that we will download a *repack* of that episode.  A repack is a
re-release of an episode to fix problems in the original.

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

## Maintenance task: removing an episode or episodes

You may remove a single episode or episodes.

Let's say someone uploaded pre-release versions of some really
popular show, in SD with a really crappy frame rate.  Say it was the
first 4 episodes of the new Season 5 of the show.  _tget_ downloaded
them and you want to delete them:

    $ tget --delete-episode "Game of Bones S05E01"
    $ tget --delete-episode "Game of Bones S05E02"
    $ tget --delete-episode "Game of Bones S05E03"
    $ tget --delete-episode "Game of Bones S05E04"
    $ tget --catch-up-series "Game of Bones S04"

The last command was to make sure you get them in the future, but also
make sure to upgrade your config file so that only high quality
episodes will be downloaded in the future:

    (defseries "Game of Bones :me :private t :delay 0 :quality :720p)

## tcleanup

_tcleanup_ is intended to be run manually or from cron.  I recommend
that you run it manually for a while, since it will take a while to
tune the configuration so that you do not receive hit and runs from
any tracker.

With no arguments, _tcleanup_ does not remove anything.  It just gives
you the status of your torrents and files:

    $ tcleanup 
    These torrents are complete:

    T name                                     %done ratio seeded      left        
    F at midnight 2015.06.10                   100%  1.22  1d 15:45:19 
    F the daily show 2015.06.09                100%  0.64  3d 01:45:17 
    F at midnight 2015.06.08                   100%  1.02  3d 18:37:16 

    These torrents are incomplete:

    T name                                     %done ratio seeded      left        
    F the daily show 2015.06.10                100%  0.60  2d 00:45:26     23:14:34
    B the daily show 2015.06.11                100%  0.22  23:45:30     2d 00:14:30
    B the nightly show with larry wilmore 2015 100%  0.22  19:43:26     2d 04:16:34
    F the graham norton show S17E10            100%  1.10  14:31:43     2d 09:28:17
    B the nightly show with larry wilmore 2015 100%  0.24  1d 19:37:36  1d 04:22:24
    F hannibal S03E02                          100%  1.10  1d 10:41:49  1d 13:18:11
    F at midnight 2015.06.09                   100%  1.01  2d 18:44:23     05:15:37
    F at midnight 2015.06.11                   100%  0.88  15:42:07     2d 08:17:53
    F the ultimate fighter S21E08              100%  0.49  2d 09:36:32     14:23:28
    B the nightly show with larry wilmore 2015 100%  0.23  1d 19:37:09  1d 04:22:51
    B the nightly show with larry wilmore 2015 100%  0.29  1d 19:37:22  1d 04:22:38
    B ridiculousness S06E29                    100%  0.24  10:45:16     2d 13:14:44
    F childrens hospital us S06E13             100%  0.62  09:36:38     2d 14:23:22

    TV:Adrian+Kevin:
    NO seeding:ridiculousness.S06E29.mp4
    TV:Kevin:
    NO 68h<72h:Last.Week.Tonight.With.John.Oliver.S02E16.HDTV.x264-BATV.mp4
    NO seeding:the.daily.show.2015.06.09.nick.offerman.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.10.colin.quinn.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.11.mark.ruffalo.hdtv.x264-crooks.mp4
    NO 68h<72h:the.daily.show.2015.06.08.nicola.sturgeon.hdtv.x264-crooks.mp4
    NO seeding:at.midnight.2015.06.08.mp4
    NO seeding:at.midnight.2015.06.11.mp4
    NO seeding:The.Graham.Norton.Show.S17E10.720p.HDTV.x264-FTP.mkv
    NO seeding:The.Ultimate.Fighter.S21E08.HDTV.x264-KOENiG.mp4

To remove the completed torrents, use `--remove`:

    $ tcleanup --remove
    These torrents were removed:

    T name                                     %done ratio seeded      left        
    F at midnight 2015.06.10                   100%  1.22  1d 15:45:47 
    F the daily show 2015.06.09                100%  0.64  3d 01:45:45 
    F at midnight 2015.06.08                   100%  1.02  3d 18:37:44 

    These torrents are incomplete:

    T name                                     %done ratio seeded      left        
    F the daily show 2015.06.10                100%  0.60  2d 00:45:54     23:14:06
    B the daily show 2015.06.11                100%  0.22  23:45:58     2d 00:14:02
    B the nightly show with larry wilmore 2015 100%  0.22  19:43:54     2d 04:16:06
    F the graham norton show S17E10            100%  1.10  14:32:11     2d 09:27:49
    B the nightly show with larry wilmore 2015 100%  0.24  1d 19:38:04  1d 04:21:56
    F hannibal S03E02                          100%  1.10  1d 10:42:17  1d 13:17:43
    F at midnight 2015.06.09                   100%  1.01  2d 18:44:51     05:15:09
    F at midnight 2015.06.11                   100%  0.88  15:42:35     2d 08:17:25
    F the ultimate fighter S21E08              100%  0.49  2d 09:37:00     14:23:00
    B the nightly show with larry wilmore 2015 100%  0.23  1d 19:37:37  1d 04:22:23
    B the nightly show with larry wilmore 2015 100%  0.29  1d 19:37:50  1d 04:22:10
    B ridiculousness S06E29                    100%  0.24  10:45:44     2d 13:14:16
    F childrens hospital us S06E13             100%  0.62  09:37:06     2d 14:22:54

    TV:Adrian+Kevin:
    NO seeding:ridiculousness.S06E29.mp4
    TV:Kevin:
    NO 68h<72h:Last.Week.Tonight.With.John.Oliver.S02E16.HDTV.x264-BATV.mp4
    NO 32h<72h:the.daily.show.2015.06.09.nick.offerman.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.10.colin.quinn.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.11.mark.ruffalo.hdtv.x264-crooks.mp4
    NO 68h<72h:the.daily.show.2015.06.08.nicola.sturgeon.hdtv.x264-crooks.mp4
    NO 10h<72h:at.midnight.2015.06.08.mp4
    NO seeding:at.midnight.2015.06.11.mp4
    NO seeding:The.Graham.Norton.Show.S17E10.720p.HDTV.x264-FTP.mkv
    NO seeding:The.Ultimate.Fighter.S21E08.HDTV.x264-KOENiG.mp4

Now we have:

    $ tcleanup

    These torrents are incomplete:

    T name                                     %done ratio seeded      left        
    F the daily show 2015.06.10                100%  0.60  2d 00:46:03     23:13:57
    B the daily show 2015.06.11                100%  0.22  23:46:07     2d 00:13:53
    B the nightly show with larry wilmore 2015 100%  0.22  19:44:03     2d 04:15:57
    F the graham norton show S17E10            100%  1.10  14:32:20     2d 09:27:40
    B the nightly show with larry wilmore 2015 100%  0.24  1d 19:38:13  1d 04:21:47
    F hannibal S03E02                          100%  1.10  1d 10:42:26  1d 13:17:34
    F at midnight 2015.06.09                   100%  1.01  2d 18:45:00     05:15:00
    F at midnight 2015.06.11                   100%  0.88  15:42:44     2d 08:17:16
    F the ultimate fighter S21E08              100%  0.49  2d 09:37:09     14:22:51
    B the nightly show with larry wilmore 2015 100%  0.23  1d 19:37:46  1d 04:22:14
    B the nightly show with larry wilmore 2015 100%  0.29  1d 19:37:59  1d 04:22:01
    B ridiculousness S06E29                    100%  0.24  10:45:53     2d 13:14:07
    F childrens hospital us S06E13             100%  0.62  09:37:15     2d 14:22:45

    TV:Adrian+Kevin:
    NO seeding:ridiculousness.S06E29.mp4
    TV:Kevin:
    NO 68h<72h:Last.Week.Tonight.With.John.Oliver.S02E16.HDTV.x264-BATV.mp4
    NO 32h<72h:the.daily.show.2015.06.09.nick.offerman.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.10.colin.quinn.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.11.mark.ruffalo.hdtv.x264-crooks.mp4
    NO 68h<72h:the.daily.show.2015.06.08.nicola.sturgeon.hdtv.x264-crooks.mp4
    NO 10h<72h:at.midnight.2015.06.08.mp4
    NO seeding:at.midnight.2015.06.11.mp4
    NO seeding:The.Graham.Norton.Show.S17E10.720p.HDTV.x264-FTP.mkv
    NO seeding:The.Ultimate.Fighter.S21E08.HDTV.x264-KOENiG.mp4

To remove the watched torrents before the configured waiting period of
72h is up (set the waiting period to 0, for example):

    $ tcleanup --remove -h0

    These torrents are incomplete:

    T name                                     %done ratio seeded      left        
    F the daily show 2015.06.10                100%  0.60  2d 00:46:16     23:13:44
    B the daily show 2015.06.11                100%  0.22  23:46:20     2d 00:13:40
    B the nightly show with larry wilmore 2015 100%  0.22  19:44:16     2d 04:15:44
    F the graham norton show S17E10            100%  1.10  14:32:33     2d 09:27:27
    B the nightly show with larry wilmore 2015 100%  0.24  1d 19:38:26  1d 04:21:34
    F hannibal S03E02                          100%  1.10  1d 10:42:39  1d 13:17:21
    F at midnight 2015.06.09                   100%  1.01  2d 18:45:13     05:14:47
    F at midnight 2015.06.11                   100%  0.88  15:42:57     2d 08:17:03
    F the ultimate fighter S21E08              100%  0.49  2d 09:37:22     14:22:38
    B the nightly show with larry wilmore 2015 100%  0.23  1d 19:37:59  1d 04:22:01
    B the nightly show with larry wilmore 2015 100%  0.29  1d 19:38:12  1d 04:21:48
    B ridiculousness S06E29                    100%  0.24  10:46:06     2d 13:13:54
    F childrens hospital us S06E13             100%  0.62  09:37:28     2d 14:22:32

    TV:Adrian+Kevin:
    NO seeding:ridiculousness.S06E29.mp4
    TV:Kevin:
    rm /me/tplex/content/videos/kevin/Last.Week.Tonight.With.John.Oliver/Last.Week.Tonight.With.John.Oliver.S02E16.HDTV.x264-BATV.mp4
    rm /me/tplex/content/videos/kevin/The.Daily.Show/the.daily.show.2015.06.09.nick.offerman.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.10.colin.quinn.hdtv.x264-crooks.mp4
    NO seeding:the.daily.show.2015.06.11.mark.ruffalo.hdtv.x264-crooks.mp4
    rm /me/tplex/content/videos/kevin/The.Daily.Show/the.daily.show.2015.06.08.nicola.sturgeon.hdtv.x264-crooks.mp4
    rm /me/tplex/content/videos/kevin/At.Midnight/at.midnight.150608-yestv.mp4
    rm /me/tplex/content/videos/kevin/At.Midnight/at.midnight.2015.06.08.mp4
    NO seeding:at.midnight.2015.06.11.mp4
    NO seeding:The.Graham.Norton.Show.S17E10.720p.HDTV.x264-FTP.mkv
    NO seeding:The.Ultimate.Fighter.S21E08.HDTV.x264-KOENiG.mp4
    $ 

## plexfix

_plexfix_ is a simple program run from _transmission_ after a torrent
completes.  _plexfix_ is given no arguments.  The directory and name
are passed in the environment variables `TR_TORRENT_DIR` and
`TR_TORRENT_NAME`.  _plexfix_ uses these to determine if Plex needs a
symlink to see the newly download torrent.  To enable it, use the
`settings.json` file for _transmission_:

    "script-torrent-done-enabled": true, 
    "script-torrent-done-filename": "/usr/local/lib/plexfix/plexfix", 

`/usr/local/lib/plexfix` is the default location for the installation
of _plexfix_.

For debugging purposes, you can also give a complete pathname to
_plexfix_ on the command line, rather than setting the above
environment variables.

_plexfix_ also accepts these arguments:

`-d` :: debug mode
`-m` :: do not send mail
`-n` :: do not execute, just say what would be done
`-q` :: be quiet, only print error messages
`-v` :: be verbose

An example email:

    From: layer@somewhere
    To: layer@somewhere
    Subject: plexfix

    OLD name: /me/tplex/content/videos/adrian+kevin/ridiculousness.0629-yestv.mp4
    NEW name: /me/tplex/content/videos/adrian+kevin/ridiculousness.S06E29.mp4

## Usage

Primary behavior determining arguments (one of these must be given):

    --run
    --add file-or-directory
    --catch-up   
    --catch-up-series series-episode-name
    --check-database
    --clean-database
    --compact-database
    --delete-episodes series-name
    --delete-episode episode-description
    --delete-orphans
    --delete-series series-name
    --dump-all
    --dump-complete-to
    --dump-episodes series-name
    --dump-orphans
    --dump-series series-name
    --dump-stats
    --skip

Behavior modifying arguments:

    --auto-backup condition
    --config file
    --cron
    --db database-name
    --debug
    --force
    --learn
    --reset
    --root data-directory
    --verbose or -v

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

* `--add thing`

  If `thing` is a file, it should be a `.torrent` file, which is 
  manually added.  If `thing` is a directory, then all the `.torrent` files
  in the directory are added.  This circumvents any matching and assumes
  the episodes should be downloaded.  The series name,episode and
  season numbers are taken directly from the file name.  If the information
  cannot be extracted, you can rename the files to suit `tget`.

  If the episode has already been downloaded, then `--force` is required
  to make `tget` download it again.

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

  Delete episodes with series name matching `series-name`.  This is permanent!
  Using this option with --auto-backup force is recommended.

* `--delete-episode episode-description`

  Delete the episode matching `episode-description`.  This is permanent!
  Using this option with --auto-backup force is recommended.  Example:

    --delete-episode "I Love Lucy S05E01"

* `--delete-orphans`

  Delete orphaned series and episodes from the database.  See
  `--dump-orphans` for more information.

* `--delete-series series-name`

  Delete series with series name matching `series-name`.  This is permanent!
  Using this option with --auto-backup force is recommended.

* `--dump-all`

  Dump all `episode` objects to stdout.

* `--dump-complete-to`

  Dump a table of series and last downloaded information for all series in
  the database to stdout.  See --catch-up-series.

* `--dump-episodes series-name`

  Dump all episode objects matching series name `series-name` to stdout.

* `--dump-orphans`

  Dump orphaned series and episodes.  Orphaned series are those which do
  not appear in the config file but exist in the database.  Orphaned
  episodes are those are in the database but have no corresponding series
  object.

* `--dump-series series-name`

  Dump all series objects matching series name `series-name` to stdout.

* `--dump-stats`

  Dump information about the database to stdout.

* `--skip series-name`

  Skip the next episode of `series-name`.  It does so by using the last
  downloaded episode and incrementing it by 1.

  Note: this has no effect on date-based series.  See the `:date-based`
  option to `defseries`.

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
  is more verbose.  This is for testing and is not recommended.  It implies
  `--learn`.

* `--force`

  The meaning of this argument depends on the other arguments and context
  in which it is given.

* `--learn`

  Mark episodes which match the configuration as _downloaded_.  This is
  useful when using tget for the first time.  See the examples below.

* `--reset`

  Reset database before beginning operation--this removes all data
  from the database.  The default --auto-backup settings will cause a
  backup to be performed before the reset.

* `--root data-directory`

  Change the data directory, the defaults is $HOME/.tget.d/

* `--verbose` or `-v`

  Verbose mode.  For now, it makes `--dump-episodes` print detailed
  information on episodes.

Examples:

Toss current database and catch up on shows released in the last 180 days
marking them all as `downloaded'

    $ tget --reset --learn

The usefulness of this is highly dependent on how far the feed for the site
you are using goes back.  Many sites do not have deep feeds, but some sites
have parameters that allow you to go back in time.  Sadly, this feature is
rare these days, as it is expensive to support.

Same, but to a `temp' database in the current directory:

    $ tget --reset --learn --root $PWD --db test.db

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
    
    ;;;; Now using transmission in a container, with no ssh
      :docker-container-name "transmission"
      :docker-user (sys:getenv "USER")
    ;;;; 
      ;;:ssh-user (sys:getenv "TRANSMISSION_SSH_USER")
      ;;:ssh-identity (sys:getenv "TRANSMISSION_SSH_IDENTITY")
    
      :add-paused nil
      :trash-torrent-file t
      ;; The default when there is tracker defined ratio
      :ratio 1.1))
    
    ;; An alternate method for downloading .torrent files: put them into a
    ;; specific directory, where your torrent client will see them.
    #+ignore
    (set-torrent-handler (pathname "~/Downloads/"))
    
    ;; Same in and outside the transmission container
    (setq *download-root* "/me/tplex/content/videos/")
    
    (defvar *codec-x264*
        ;; It goes by two different names:
        '(:x264 :h.264))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Trackers
    
    #+testing
    (deftracker :eztv
        ;; The usual place for EZTV is down (https://ezrss.it/feed/),
        ;; try this, which I got from this page:
        ;;   http://www.bt-chat.com/rsstag.php?
        :url "http://rss.bt-chat.com/..."
        :debug-feed :eztv
        :public t
        :download-delay 0
        :disabled nil
        :ratio 1.0)
    
    (deftracker :freshon
        ;; The feed URL is
        ;;  https://freshon.tv/rss.php?feed=dl&passkey=<passkey>"
        ;; Get the <passkey> from your profile page.
        :url "https://freshon.tv/..."
        :debug-feed :freshon
        :download-delay 0
        ;; See if limiting the upload rate allows freshon to correctly count my
        ;; upload credits.  Sick of uploading lots and not getting credit for it.
        :upload-limit 100 ;; KB/s
        :ratio 1.3)
    
    (deftracker :btn
        ;; RSS feed configured by creating a notification filter with these
        ;; options: 
        ;; - categories: episode
        ;; - containers: avi, mkv, mp4
        ;; - codecs: x264, h.264
        ;; - resolutions: sd, 720p
        ;; All other checkboxs were unchecked.
        ;;
        :url "https://broadcasthe.net/..."
        :debug-feed :btn
        :disabled #-debug nil #+debug t
        ;; Can't make the delay too long or shows will fall off the RSS feed
        :download-delay #-debug 1 #+debug 0
        :ratio 1.5)
    
    (deftracker :shazbat
        :url "https://www.shazbat.tv/..."
        :debug-feed :shazbat
        :disabled t
        :download-delay 0
        :ratio 1.5)
    
    (defvar *trackers*
        ;; move :freshon to last.  Their tracker is being very annoying lately
        ;; and not giving me upload credit.  Grrr.
        (list :btn :shazbat #+testing :eztv :freshon))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Quality settings
    
    (defquality :high-1080p
        :priority -1			; never download
        :codec *codec-x264* 
        :resolution :1080p)
    
    (defquality :high-1080i
        :priority -1			; never download
        :codec *codec-x264* 
        :resolution :1080i)
    
    (defquality :normal
        ;; The priority of a quality allows selection of episodes when more
        ;; than one quality is available at the same time, as is often the
        ;; case.  Higher numerical priority is given precedence.
        :priority 50
        :source :hdtv
        :codec *codec-x264* 
        :resolution :sd)
    
    (defquality :high
        :priority 40
        :codec *codec-x264* 
        :resolution :720p)
    
    (defquality :low
        :priority 30
        :source :hdtv
        :codec :xvid
        :resolution :sd)
    
    (defquality :btn
        :priority 60
        :container '(:mkv :mp4)
        ;;:source :hdtv
        :codec *codec-x264*)
    
    ;; The additional delay waiting to download a high quality ep while
    ;; waiting for a normal quality one to become available
    (defvar *download-hq-delay* #-debug 1 #+debug 5)
    
    ;; The additional delay waiting to download a low quality ep while
    ;; waiting for a normal or high quality one to become available
    (defvar *download-lq-delay* #-debug 24 #+debug 24)
    
    ;; While I'm tuning the new delays, let's be verbose:
    (pushnew :tget-config-debug *features*)
    
    (defun my-quality (episode &aux (tracker (episode-tracker episode))
    				temp)
      (flet
          (#+tget-config-debug
           (debug-msg (temp)
    	 (when temp
    	   ;; we would have downloaded this if enough time had passed, so
    	   ;; let's say that
    	   (format t "~
    DEBUG: (tracker delay + quality delay) - hours avail = ~d hours for:
           ~a~%"
    		   (- temp (hours-available episode)) episode))))
        ;; My defined quality, as a function.  This allows me to download
        ;; different qualities based on different criteria.
        ;;
        ;; We also output progress reports for episodes not downloaded,
        ;; so we can monitor progress, for debugging purposes.
        (when (and (null
    		;; See if there is an episode with :normal quality.  The
    		;; :transient keyword is important, since it restricts the
    		;; search to episodes we have *not* downloaded yet.
    		(query-episode :episode episode :quality :normal :transient t))
    	       (eq :high (episode-quality episode))
    	       (and tracker
    		    (numberp (tracker-download-delay tracker))
    		    (if* (>= (hours-available episode)
    			     (setq temp
    			       (+ (tracker-download-delay tracker)
    				  *download-hq-delay*)))
    		       thenret
    		       else (@log ">>>waiting for ~a more hours for this HQ ep"
    				  (- temp (hours-available episode)))
    			    nil)))
          ;; :normal quality is not available and the :high quality episode
          ;; has been available for a set amount of hours, then take this
          ;; one
          (return-from my-quality :high))
      
        #+tget-config-debug (debug-msg temp)
      
        (when (=~ "broadcasthe.net" (episode-torrent-url episode))
          (return-from my-quality :btn))
    
        (setq temp nil)
        (when (and (null
    		;; See if there is an episode with :normal or :high
    		;; quality.
    		(or
    		 (query-episode :episode episode :quality :normal :transient t)
    		 (query-episode :episode episode :quality :high :transient t)))
    	       (eq :low (episode-quality episode))
    	       (and tracker
    		    (>= (hours-available episode)
    			(setq temp
    			  (+ (tracker-download-delay tracker)
    			     *download-lq-delay*)))))
          (return-from my-quality :low))
        
        #+tget-config-debug (debug-msg temp)
      
        :normal))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Groups
    
    (defgroup :adrian
        :trackers '#.*trackers*
        :quality 'my-quality
        :download-path (merge-pathnames "adrian/" *download-root*))
    
    (defgroup :anh
        :trackers '#.*trackers*
        :quality 'my-quality
        :download-path (merge-pathnames "anh/" *download-root*))
    
    (defgroup :kevin
        :trackers '#.*trackers*
        :quality 'my-quality
        :download-path (merge-pathnames "kevin/" *download-root*))
    
    (defgroup :adrian+kevin
        :trackers '#.*trackers*
        :quality 'my-quality
        :download-path (merge-pathnames "adrian+kevin/" *download-root*))
    
    (defgroup :anh+kevin
        :trackers '#.*trackers*
        :quality 'my-quality
        :download-path (merge-pathnames "anh+kevin/" *download-root*))
    
    ;;;;TODO: seems like this shouldn't be here... figure out a way to get
    ;;;;      around it.  See --add command line argument.
    (defgroup :manual
        :rss-url nil
        :delay 0
        :quality 'my-quality
        ;; a dummy value, overridden by whatever group matches
        :download-path (merge-pathnames "kevin/" *download-root*))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; TV shows
    
    ;; Use ... :catch-up "S01E02" ... to start a series after the 1st ep
    ;; Use ... :remove t ... to delete a series
    ;; Use ... :subdir "dirname" ... to put the episodes into a subdirectory of
    ;;          the group download path -- this is a hack to make Plex Media
    ;;          Server see episodes of The Daily Show and The Colbert Report.
    
    (defseries "8 Out of 10 Cats" :kevin)
    (defseries "8 Out of 10 Cats Does Countdown" :kevin)
    (defseries "An Idiot Abroad" :adrian+kevin)
    (defseries "Archer (2009)" :kevin)
    (defseries "At Midnight" :kevin :date-based t :subdir "At.Midnight"
    	   :aliases ("@midnight"))
    (defseries "Bates Motel" :anh+kevin)
    (defseries "Better Call Saul" :adrian+kevin)
    (defseries "Black Mirror" :adrian+kevin)
    (defseries "Brooklyn Nine-Nine" :adrian+kevin)
    (defseries "Childrens Hospital (US)" :adrian+kevin)
    (defseries "Community" :adrian+kevin)
    (defseries "Curb your Enthusiasm" :adrian+kevin)
    (defseries "Doc Martin" :anh+kevin)
    (defseries "Downton Abbey" :anh)
    (defseries "Dragons Den (UK)" :kevin)
    (defseries "Eagleheart" :adrian+kevin)
    (defseries "Elementary" :kevin)
    (defseries "Fargo" :kevin)
    (defseries "Frontline (US)" :kevin)
    (defseries "Game of Thrones" :kevin :private t :delay 0 :quality :high)
    (defseries "Hannibal" :kevin :delay 0 #-testing :quality #-testing :high)
    (defseries "Hell on Wheels" :kevin)
    (defseries "Homeland" :kevin :private t :delay 0)
    (defseries "Intruders" :kevin :catch-up "S01E02")
    (defseries "James May's Man Lab" :adrian+kevin)
    (defseries "Last Week Tonight with John Oliver" :kevin :private t
      :subdir "Last.Week.Tonight.With.John.Oliver")
    (defseries "Longmire" :kevin)
    (defseries "Louis Theroux Documentaries" :kevin)
    (defseries "Louie" :kevin)
    (defseries "Luther" :kevin)		;gone forever??
    (defseries "Mad Men" :kevin)
    (defseries "Midsomer Murders" :anh)
    (defseries "Modern Family" :adrian+kevin)
    (defseries "Naked and Afraid" :kevin :catch-up "S01")
    (defseries "Nathan for You" :adrian+kevin)
    (defseries "Nova" :kevin)
    (defseries "Ray Donovan" :kevin :private t)
    (defseries "Regular Show" :adrian+kevin)
    (defseries "Rick and Morty" :adrian+kevin)
    (defseries "Ridiculousness" :adrian+kevin)
    (defseries "Shark Tank" :kevin)
    (defseries "Sherlock" :kevin)
    (defseries "Sirens (2014)" :kevin)
    (defseries "The Americans (2013)" :kevin)
    (defseries "The Daily Show with Jon Stewart" :kevin :subdir "The.Daily.Show"
    	   :date-based t
    	   :aliases ("The Daily Show"))
    (defseries "The Good Wife" :anh+kevin)
    (defseries "The Graham Norton Show" :kevin)
    (defseries "The Last Man on Earth" :adrian+kevin :catch-up "S01E02")
    (defseries "The Meltdown with Jonah and Kumail" :kevin :catch-up "S01E04")
    (defseries "The Mentalist" :adrian+kevin)
    (defseries "The Neighbors (2012)" :adrian+kevin)
    (defseries "The Nightly Show with Larry Wilmore" :kevin
      :aliases ("The Nightly Show")
      :subdir "The.Nightly.Show" :date-based t)
    (defseries "The Ultimate Fighter" :kevin)
    (defseries "The Walking Dead" :kevin :delay 0)
    (defseries "Top Gear" :adrian+kevin :quality :high)
    (defseries "Tosh.0" :kevin)
    (defseries "True Detective" :kevin :private t)
    (defseries "Vikings" :kevin)
    (defseries "Wallander" :anh+kevin)
    (defseries "Witness (2012)" :kevin :private t)
    (defseries "World's Craziest Fools" :adrian+kevin)
    (defseries "Would I Lie To You?" :kevin :catch-up "S08E01"
    	   :aliases ("Would I Lie To You"))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; These items are for the test suite only, and are not used in production
    ;; mode:
    
    #+testing (defseries "The Newsroom (2012)" :kevin)
    #+testing (defseries "Top of the Lake" :kevin)
    #+testing (defseries "Parks and Recreation" :adrian+kevin)
    #+testing (defseries "The Simpsons" :adrian+kevin)
    #+testing (defseries "Drunk History" :kevin)
    #+testing (defseries "White Collar" :anh+kevin)
    #+testing (defseries "Justified" :kevin)

[1]: http://www.transmissionbt.com/   "Transmission"
[2]: http://flexget.com/              "FlexGet"
[3]: http://www.macports.org/         "MacPorts"
