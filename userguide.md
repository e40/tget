# tget ${*tget-version*} - torrent get

_tget_ is a suite of programs: _tget_ and _plexfix_.

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
**[Cleanup](#Cleanup)**  
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

%%VALUE: *valid-containers*

`:source` -- the acceptable source for the quality.  The source is where
the stream originated.  _:hdtv_ is a common source.  Valid values:

%%VALUE: *valid-sources*

`:codec` -- the codec used to encode the original source (e.g. _XviD_ or
_x264_, aka _h.264_).  Valid values:

%%VALUE: *valid-codecs*

`:resolution` -- the resolution of the encoded image (e.g. _720p_).
Valid values:

%%VALUE: *valid-resolutions*

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

### Configuration options

`*repack-window-seconds*` -- the number of seconds within the download
of an original file that a repack will be downloaded.  The default
value is:

%%VALUE: *repack-window-seconds*

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

## Cleanup

This is a mode of _tget_, when the `--cleanup` or `-c` arguments are
given.  It is intended to be run manually or from cron.  I recommend
that you run it manually for a while, since it will take a while to
tune the configuration so that you do not receive hit and runs from
any tracker.

With no optional arguments, nothing is removed.  _tget_ prints the
status of torrents and videos:

    $ tget -c 
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

    $ tget -c --remove
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

    $ tget -c

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

    $ tget -c --remove -h0

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
