#!/bin/bash

set -e

BASEDIR="/home/wohanley/scripts/mail"

${BASEDIR}/sync.sh "$1"
${BASEDIR}/notify.sh
