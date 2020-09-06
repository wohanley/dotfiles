#!/bin/bash

# Run this from the project root to link things where they need to go.

stow -t ~/.emacs.d/private -d .emacs.d private
stow -t ~/.config/i3blocks -d .config i3blocks
