#!/bin/bash

BASEDIR="/home/wohanley/scripts/mail"

${BASEDIR}/sync.sh "$1"
${BASEDIR}/notify.sh
