#!/bin/bash

LOG_FILE="${HOME}/.log/mail.log"
LOGGING_ENABLED=false

BASEDIR="${HOME}/scripts/mail"

source "${BASEDIR}/venv/bin/activate"

if [[ $LOGGING_ENABLED = false ]]; then
    mbsync "${1:---all}"
    notmuch new # see home-manager for post-new hook reading notmuch-post-new-tags
    python "${BASEDIR}/spam-filter.py"
else # verbose
    echo "$(date): PATH is $PATH" >> $LOG_FILE
    printf "\n" >> "${LOG_FILE}"

    echo '### mbsync output and exit status ###' >> $LOG_FILE
    printf "\n" >> $LOG_FILE
    mbsync "${1---all}" >> $LOG_FILE 2>&1
    echo "$(date): mbsync exited with status $?" >> $LOG_FILE
    printf "\n" >> $LOG_FILE

    echo '### notmuch output and exit status ###' >> $LOG_FILE
    printf "\n" >> $LOG_FILE
    notmuch new >> $LOG_FILE 2>&1
    echo "$(date): notmuch exited with status $?" >> $LOG_FILE
    printf "\n" >> $LOG_FILE

    echo '### bogofilter script output ###' >> $LOG_FILE
    printf "\n" >> $LOG_FILE
    python "${HOME}/scripts/mail/spam-filter.py" >> $LOG_FILE 2>&1
    echo "$(date): spam-filter.py exited with status $?" >> $LOG_FILE
    printf "\n" >> $LOG_FILE

    echo '### mail-notify.sh output and exit status ###' >> $LOG_FILE
    printf "\n" >> $LOG_FILE
    bash "${HOME}/scripts/mail/notmuch-tag.sh" >> $LOG_FILE 2>&1
    echo "$(date): mail-notify.sh exited with status $?" >> $LOG_FILE
    printf "\n" >> $LOG_FILE
fi
