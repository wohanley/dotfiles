#!/bin/sh

export GOPATH="$HOME/code/go"
export PYTHONPATH="$PYTHONPATH:$HOME/lib/python"

export PATH="$PATH:$HOME/bin:$HOME/Applications/bin:$HOME/node/node_modules/.bin:$GOPATH/bin:$HOME/opt/activator-1.3.2:$HOME/.gem/ruby/2.2.0/bin:$HOME/.rbenv/bin:$HOME/software/elm/.cabal-sandbox/bin:/usr/local/heroku/bin:$GOPATH/bin"


if [[ `uname -s` == 'Darwin' ]]
    then launchctl setenv PATH $PATH
fi

alias ghc-usual="ghc -fwarn-missing-signatures"
