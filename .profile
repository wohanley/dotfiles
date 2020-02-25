#!/bin/sh

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

export JAVA_HOME="/lib/jvm/default"
export JDK_HOME="/lib/jvm/default"

export GOPATH="$HOME/vagrant/code/go"
export PYTHONPATH="$PYTHONPATH:$HOME/lib/python"

export PATH="$PATH:$HOME/bin:$HOME/scripts:$HOME/Applications/bin:$HOME/node/node_modules/.bin:$GOPATH/bin:$HOME/opt/activator-1.3.2:$HOME/.gem/ruby/2.6.0/bin:$HOME/.rbenv/bin:/usr/local/heroku/bin:$HOME/games/glulx"

export QT_QPA_PLATFORMTHEME='qt5ct'

if [[ `uname -s` == 'Darwin' ]]
    then launchctl setenv PATH $PATH
fi

alias ghc-usual="ghc -fwarn-missing-signatures"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
