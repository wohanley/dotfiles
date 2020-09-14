#!/bin/bash

# source: https://gist.github.com/stefanschmidt/5248592

set -e

pdftk "$1" output "${1%.pdf}_uncompressed.pdf" uncompress
LANG=C LC_CTYPE=C sed -n '/^\/Annots/!p' "${1%.pdf}_uncompressed.pdf" > "${1%.pdf}_stripped.pdf"
pdftk "${1%.pdf}_stripped.pdf" output "${1%.pdf}_annotations_hidden.pdf" compress
rm "${1%.pdf}_uncompressed.pdf" "${1%.pdf}_stripped.pdf"
