# A good chunk of this is stolen from http://stackoverflow.com/questions/171563/whats-in-your-zshrc

################################################################################
# Modules
################################################################################

autoload -U compinit promptinit zcalc zsh-mime-setup
compinit
promptinit
zsh-mime-setup

################################################################################
# Options
################################################################################

# why would you type 'cd dir' if you could just type 'dir'?
setopt AUTO_CD

# Now we can pipe to multiple outputs!
setopt MULTIOS

# Spell check commands!  (Sometimes annoying)
setopt CORRECT

# This makes cd=pushd
setopt AUTO_PUSHD

# This will use named dirs when possible
setopt AUTO_NAME_DIRS

# If we have a glob this will expand it
setopt GLOB_COMPLETE
setopt PUSHD_MINUS

# No more annoying pushd messages...
# setopt PUSHD_SILENT

# blank pushd goes to home
setopt PUSHD_TO_HOME

# use magic (this is default, but it can't hurt!)
setopt ZLE

# turn off ZLE if running in Emacs
[[ $EMACS = t ]] && unsetopt zle

setopt NO_HUP

setopt IGNORE_EOF

# If I could disable Ctrl-s completely I would!
setopt NO_FLOW_CONTROL

# beeps are annoying
setopt NO_BEEP

# Keep echo "station" > station from clobbering station
setopt NO_CLOBBER

# Case insensitive globbing
setopt NO_CASE_GLOB

# Be Reasonable!
setopt NUMERIC_GLOB_SORT

# I don't know why I never set this before.
setopt EXTENDED_GLOB

# The cleanest possible English quote escaping
setopt rcquotes

################################################################################
# Variables
################################################################################

export MATHPATH="$MANPATH:/usr/local/texlive/2007/texmf/doc/man"
export INFOPATH="$INFOPATH:/usr/local/texlive/2007/texmf/doc/info"
export PATH="$PATH:/usr/local/texlive/2007/bin/i386-linux"
export RI="--format ansi"

declare -U path

export EDITOR=emacs
export PAGER=less

################################################################################
# Completions
################################################################################

bindkey -M viins '\C-i' complete-word

# Faster! (?)
zstyle ':completion::complete:*' use-cache 1

# case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''
#zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete
zstyle ':completion:*' completer _expand _force_rehash _complete _approximate _ignored

# generate descriptions with magic.
zstyle ':completion:*' auto-description 'specify: %d'

# Don't prompt for a huge list, page it!
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Don't prompt for a huge list, menu it!
zstyle ':completion:*:default' menu 'select=0'

# Have the newer files last so I see them first
zstyle ':completion:*' file-sort modification reverse

# color code completion!!!!  Wohoo!
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"

unsetopt LIST_AMBIGUOUS
setopt  COMPLETE_IN_WORD

# Separate man page sections.  Neat.
zstyle ':completion:*:manuals' separate-sections true

# Egomaniac!
zstyle ':completion:*' list-separator 'fREW'

# complete with a menu for xwindow ids
zstyle ':completion:*:windows' menu on=0
zstyle ':completion:*:expand:*' tag-order all-expansions

# more errors allowed for large words and fewer for small words
zstyle ':completion:*:approximate:*' max-errors 'reply=(  $((  ($#PREFIX+$#SUFFIX)/3  ))  )'

# Errors format
zstyle ':completion:*:corrections' format '%B%d (errors %e)%b'

# Don't complete stuff already on the line
zstyle ':completion::*:(rm|vi):*' ignore-line true

# Don't complete directory we are already in (../here)
zstyle ':completion:*' ignore-parents parent pwd

zstyle ':completion::approximate*:*' prefix-needed false

################################################################################
# Key bindings
################################################################################

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

bindkey '^B' push-line

bindkey '\e[C' forward-char
bindkey '\e[D' backward-char

# Rebind insert and delete to more reasonable behaviour
bindkey '\e[2~' overwrite-mode
bindkey '\e[3~' delete-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if [[ -n ${terminfo[smkx]} ]] && [[ -n ${terminfo[rmkx]} ]]; then

    function zle-line-init () { echoti smkx }
    function zle-line-finish () { echoti rmkx }

    zle -N zle-line-init
    zle -N zle-line-finish
fi

################################################################################
# History
################################################################################

# Where it gets saved
HISTFILE=~/.history

# Remember about a years worth of history (AWESOME)
SAVEHIST=10000
HISTSIZE=10000

# Don't overwrite, append!
setopt APPEND_HISTORY

# Write after each command
# setopt INC_APPEND_HISTORY

# Killer: share history between multiple shells
setopt SHARE_HISTORY

# If I type cd and then cd again, only save the last one
setopt HIST_IGNORE_DUPS

# Even if there are commands inbetween commands that are the same, still only save the last one
setopt HIST_IGNORE_ALL_DUPS

# Just really no duplicates please
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS

setopt HIST_REDUCE_BLANKS

# If a line starts with a space, don't save it.
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE

# When using a hist thing, make a newline show the change before executing it.
setopt HIST_VERIFY

# Save the time and how long a command ran
setopt EXTENDED_HISTORY

################################################################################
# Prompt
################################################################################

#Color table from: http://www.understudy.net/custom.html
fg_black=$'\e[0;30m'
fg_red=$'\e[0;31m'
fg_green=$'\e[0;32m'
fg_brown=$'\e[0;33m'
fg_blue=$'\e[0;34m'
fg_purple=$'\e[0;35m'
fg_cyan=$'\e[0;36m'
fg_lgray=$'\e[0;37m'
fg_dgray=$'\e[1;30m'
fg_lred=$'\e[1;31m'
fg_lgreen=$'\e[1;32m'
fg_yellow=$'\e[1;33m'
fg_lblue=$'\e[1;34m'
fg_pink=$'\e[1;35m'
fg_lcyan=$'\e[1;36m'
fg_white=$'\e[1;37m'
#Text Background Colors
bg_red=$'\e[0;41m'
bg_green=$'\e[0;42m'
bg_brown=$'\e[0;43m'
bg_blue=$'\e[0;44m'
bg_purple=$'\e[0;45m'
bg_cyan=$'\e[0;46m'
bg_gray=$'\e[0;47m'
#Attributes
at_normal=$'\e[0m'
at_bold=$'\e[1m'
at_italics=$'\e[3m'
at_underl=$'\e[4m'
at_blink=$'\e[5m'
at_outline=$'\e[6m'
at_reverse=$'\e[7m'
at_nondisp=$'\e[8m'
at_strike=$'\e[9m'
at_boldoff=$'\e[22m'
at_italicsoff=$'\e[23m'
at_underloff=$'\e[24m'
at_blinkoff=$'\e[25m'
at_reverseoff=$'\e[27m'
at_strikeoff=$'\e[29m'

user_color=$fg_purple
root_color=$fg_red

host_prompt="%{$at_bold%}%{$fg_lgreen%}@%m%{$reset_color%}%{$at_normal%}"

jobs_prompt="%{$at_bold%}%{$fg_yellow%}(%j)%{$reset_color%}%{$at_normal%}"

history_prompt="%{$at_bold%}%{$fg_white%}[%{$at_normal%}%{$fg_yellow%}%T%{$at_bold%}%{$fg_white%}]%{$reset_color%}%{$at_normal%}"

error_prompt="%{$at_bold%}%{$fg_lred%}<%?>%{$reset_color%}%{$at_normal%}"

case "$TERM" in
  (screen)
    function precmd() { print -Pn "\033]0;S $TTY:t{%100<...<%~%<<}\007" }
  ;;
  (*)
    directory_prompt="%{$at_bold%}%{$fg_lcyan%}%~%{$reset_color%}%{$at_normal%}"
  ;;
esac

if [[ $USER == root ]]; then
    user_prompt="%{$at_bold%}%{$fg_lred%}%n%{$reset_color%}%{$at_normal%}"
    post_prompt="%{$at_bold%}%{$fg_lred%}%#%{$reset_color%}%{$at_normal%}"
else
    user_prompt="%{$at_bold%}%{$fg_pink%}%n%{$reset_color%}%{$at_normal%}"
    post_prompt="%{$at_bold%}%{$fg_white%}☄%{$reset_color%}%{$at_normal%}"
fi

PS1="${user_prompt}${host_prompt}${history_prompt} ${directory_prompt} ${post_prompt} "

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}

edit-command-output() {
 BUFFER=$(eval $BUFFER)
 CURSOR=0
}
zle -N edit-command-output

################################################################################
# zsh-syntax-highlighting
################################################################################

export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=$HOME/homebrew/share/zsh-syntax-highlighting/highlighters
source $HOME/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

################################################################################
# zsh-history-substring-search
################################################################################

source $HOME/homebrew/share/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey '\e[A' history-substring-search-up
bindkey '\e[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

################################################################################
# Conveniences
################################################################################

# color is nice
if [[ `uname -s` == 'Darwin' ]] {
  alias ls='ls -G'
} else {
  alias ls='ls --color'
}

alias l='ls -lFh'     #size, show type, human readable
alias la='ls -lAFh'   #long list, show almost all, show type, human readable
alias lr='ls -tRFh'   #sorted by date, recursive, show type, human readable
alias lt='ls -ltFh'   #long list, sorted by date, show type, human readable

# cd, because typing the backslash is ALOT of work!!
alias .='cd ../'
alias ..='cd ../../'
alias ...='cd ../../../'
alias ....='cd ../../../../'

# copy with a progress bar.
alias cpv="rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress --"

# Alt-S inserts "sudo "
insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo
