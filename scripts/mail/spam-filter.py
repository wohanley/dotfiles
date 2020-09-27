# This script uses bogofilter to classify spam.

import subprocess
import notmuch
import sys
import os

# define object that we'll use to store a file path
# and bogofilter spam classification
class pOutput(object):
    path = ""
    mailType = ""

# function for creating objects out of
# bogofilter output
def processOutput(path, mailType):
    processed = pOutput()
    processed.path = path
    processed.mailType = mailType
    return processed

bogofilter = '/home/wohanley/.nix-profile/bin/bogofilter'

# define classification function
def isSpam(path):
    p = subprocess.run([bogofilter, "-BT", path], stdout=subprocess.PIPE)
    output = p.stdout.decode('ascii')
    output = output.split(" ")
    processed = processOutput(output[0], output[1])
    # output has to be decoded
    return processed.mailType

for msg in notmuch.Database(mode=1).create_query('tag:spam-unchecked').search_messages():
    for filepath in msg.get_filenames():
        spam_status = isSpam(filepath)
        if spam_status == 'S':
            msg.remove_tag('maybe-spam')
            msg.add_tag('spam')
            msg.remove_tag('inbox')
            msg.remove_tag('notify')
        elif spam_status == 'H':
            msg.remove_tag('maybe-spam')

        msg.remove_tag('spam-unchecked')

sys.exit()
