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

export GOPATH="$HOME/code/go"
export PYTHONPATH="$PYTHONPATH:$HOME/lib/python"

export PATH="$PATH:$HOME/bin:$HOME/scripts:$HOME/Applications/bin:$HOME/node/node_modules/.bin:$GOPATH/bin:$HOME/opt/activator-1.3.2:$HOME/.gem/ruby/2.6.0/bin:$HOME/.gem/ruby/2.7.0/bin:$HOME/.rbenv/bin:/usr/local/heroku/bin:$HOME/games/glulx"

# nix stuff
# export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH # nixpkgs unstable
export NIX_PATH="nixpkgs=$HOME/code/nixpkgs" # local nixpkgs
export NIX_REMOTE=daemon
. "$HOME/.nix-profile/etc/profile.d/nix.sh"
. "$HOME/.nix-profile/etc/profile.d/nix-daemon.sh"
# include home-manager variables
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

# Make updated PATH (particularly Nix paths) available to systemd user units
systemctl --user import-environment PATH

# gpg-agent needs this. not sure why https://www.gnupg.org/documentation/manuals/gnupg/Invoking-GPG_002dAGENT.html
GPG_TTY=$(tty)
export GPG_TTY

export QT_QPA_PLATFORMTHEME='qt5ct'

if [ "$(uname -s)" = 'Darwin' ]; then
    launchctl setenv PATH "$PATH"
fi

export EDITOR=start-emacs

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
