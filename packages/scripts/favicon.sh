#!/bin/sh
convert "$1" -define icon:auto-resize=64,48,32,16 "${2-favicon.ico}"
