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

### `defquality &key priority container source codec resolution`

The valid values for each keyword option are:

`:priority` -- any positive integer from 1 to 100.  The higher the
number the higher the priority the that has this quality will be given
if there are multiple matching episodes.

`:container` -- the acceptable containers for the quality.  A container
is, essentially, the file type of the downloaded file (e.g. *mp4*).
Valid values:

%%VALUE: *valid-containers*

`:source` -- the acceptable source for the quality.  The source is where
the stream originated.  *:hdtv* is a common source.  Valid values:

%%VALUE: *valid-sources*

`:codec` -- the codec used to encode the original source (e.g. *XviD* or
*x264*, aka *h.264*).  Valid values:

%%VALUE: *valid-codecs*

`:resolution` -- the resolution of the encoded image (e.g. *720p*).
Valid values:

%%VALUE: *valid-resolutions*

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

    0 3 * * *  source $HOME/.profile; /usr/local/bin/tget --cron | $HOME/bin/notify.sh tget

It runs every day at 3AM.  Running more often is fine, but you need to
be careful not to run afoul of the site rules.

The `notify.sh` script is what I use to email the output of the
program, but only if there is output (no empty emails).  That script
is:

    #! /bin/bash
    # copy stdin to an email message, but only send one if there's something
    # on stdin.  Kinda like how cron works.  No output, no email.

    if read line; then
        (echo $line;
            while read line; do
                echo $line
            done) 2>&1 |
        Mail -s "${1-$0} $(date '+%Y-%M-%d %T')" username@domain
    fi

You should change `username@domain` to your email address.  Of course,
if the email address to which the cron output would go is your desired
destination, then you need only specify the *crontab* entry like this:

    0 3 * * *  source $HOME/.profile; /usr/local/bin/tget --cron

I source my `$HOME/.profile` (which is sourced by my `$HOME/.bashrc`)
to pick up some environment variables used for the Transmission
interface.

## Maintenance tasks

coming soon
