#!/bin/sh

endpoint=prism.r:8001

case "$1" in
  good|like|cool|nice|yes|+)
    curl -sS -XPOST "$endpoint/good";;
  skip|next|bad|sucks|no|nope|-)
    curl -sS -XPOST "$endpoint/skip";;
  *)
    curl -sS -XGET "$endpoint/current" | jq;;
esac
