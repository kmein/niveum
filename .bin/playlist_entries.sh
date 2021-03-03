#!/bin/sh
youtube-dl -ij "$*" | jq -sr '.[] | .webpage_url'
