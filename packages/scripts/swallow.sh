#!/bin/sh
# https://github.com/salman-abedin/devour/blob/master/devour.sh

id=$(xdo id)
xdo hide
$("$@") > /dev/null 2>&1
xdo show "$id"
