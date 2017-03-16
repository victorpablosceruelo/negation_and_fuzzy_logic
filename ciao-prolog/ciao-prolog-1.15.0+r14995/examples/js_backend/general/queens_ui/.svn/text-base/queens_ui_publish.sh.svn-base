#!/bin/sh
# Script to publish queens_ui demo (at clip)
FILES="queens_ui.out.js queens_ui.html queens_ui.css queens8.pl queens_ui_qrcode.png"

USERHOST=jfran@clip.dia.fi.upm.es

# Compile
ciaotool js-backend --target-platform chrome try-exec queens_ui || return 0

# And publish
cd ~/.ciao-cache/js-out
PUBDIR=/home/jfran/public_html/ptojs/queens_ui
ssh ${USERHOST} mkdir -p ${PUBDIR}
scp ${FILES} ${USERHOST}:${PUBDIR}


