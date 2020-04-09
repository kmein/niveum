#!/usr/bin/env bash
file="$(rg "$@" | fzf -0 -1 | awk -F: '{print $1}')"

if [[ -n $file ]]
then
   ${EDITOR:-vim} "$file"
fi
