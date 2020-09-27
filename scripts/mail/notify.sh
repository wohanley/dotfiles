#!/bin/bash
# This is a simple script which uses notmuch and the standard 'notify-send'
# utility to create notification popups with email subjects.
# Due to the way notify-send handles special characters you will also need
# the recode utility to change from UTF-8 to HTML.

# Original source: https://github.com/natmey/dotfiles/blob/master/notmuch/notmuch-notification.sh

# Most of the settings are set below.

# USAGE:
#   notmuch-notification.sh [--show-none]
#
#   --show-none  show a notification even if there are no new messages.

# Necessary to show notification when running as root, see https://bbs.archlinux.org/viewtopic.php?id=205867
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/1000/bus"

# The notmuch search that will generate subjects you want
SEARCH="tag:notify"

# the sort order of subjects
#   corresponds to the --sort option of notmuch search
SORT="newest-first"

# The number of subjects to show in the notification
#   corresponds to the --limit option of notmuch search
LIMIT=10

# the icon in the notification window
NOTIFICATION_ICON='/usr/share/icons/oxygen/base/32x32/apps/internet-mail.png'

# have notmuch count the number of messages in the search
NOTIFY_COUNT=$(notmuch count --output=messages "$SEARCH")
if [ "$NOTIFY_COUNT" -gt 0 ]; then
  # have notmuch pull the specified number of mail subjects from the search.
  # also, do some rought formatting of the result, to pull thread string,
  # sender etc. leaving just the subject text.
  TXT_SUBS=$(notmuch search --format=text --output=summary --limit="$LIMIT" --sort="$SORT" "$SEARCH" | sed 's/^[^;]*; //' | sed 's/$/\n'/)

  notify-send -i "$NOTIFICATION_ICON" "$NOTIFY_COUNT unread messages." "$TXT_SUBS"
elif [ -z "$1" ]; then
  exit 0
elif [ "$1" == "--show-none" ]; then
  notify-send -t 4000 -i "$NOTIFICATION_ICON" "No unread messages."
fi

notmuch tag -notify -- tag:notify

exit 0
