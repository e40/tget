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
**[Example configuration file](#example-configuration-file)**  

## Installation

For now, no binary packages are available and you need Allegro Common
Lisp to build it.  After cloning the repo, do this to build:

    $ make

That should produce a directory `tget/`, which can be installed with:

    $ make install

You can make a `Makefile.local` to override features of the GNU Make
file.

## Configuration

There is an example configuration file below.  It is fully anotated
and that is currently the only documentation.

## Putting it into service

Running from *cron* is the preferred method of operation.  This
crontab entry will do the job:

    0 3 * * *  source $HOME/.profile; /usr/local/bin/tget --cron | $HOME/bin/notify.sh tget

It runs every day at 3AM.  The `notify.sh` script is what I use to
email the output of the program, but only if there is output (no empty
emails).  That script is:

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

You should change `username@domain` to your email address.

## Maintenance tasks

coming soon
