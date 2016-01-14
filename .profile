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

PATH="$PATH:$HOME/bin:$HOME/.gem/ruby/2.1.0/bin:$HOME/software/elm/.cabal-sandbox/bin:/opt/gradle-1.9/bin"
export PATH

JAVA_HOME="/lib/jvm/default"
JDK_HOME="/lib/jvm/default"
export JAVA_HOME
export JDK_HOME

alias ghc-usual="ghc -fwarn-missing-signatures"

# add personal bin to PATH if it exists
# is it a problem if it doesn't?
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
