#!/bin/sh
curl -s -H "Accept-Language: ${LANG%_*}" --compressed "wttr.in/${1-}?0"
