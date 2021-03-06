#!/bin/bash

set -e

# Explicitly sourcing from /home/wohanley because we might be running from a
# system unit (in order to depend on resume). Can't wait for systemd to fix its
# user sessions so I can depend on wake in a user unit :/
export NIX_PATH="nixpkgs=/home/wohanley/code/nixpkgs" # local nixpkgs
export NIX_REMOTE=daemon
. "/home/wohanley/.nix-profile/etc/profile.d/nix.sh"
. "/home/wohanley/.nix-profile/etc/profile.d/nix-daemon.sh"
# include home-manager variables
. "/home/wohanley/.nix-profile/etc/profile.d/hm-session-vars.sh"

/home/wohanley/scripts/mail/on-new-mail.sh
