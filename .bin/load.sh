#!/bin/sh
uptime | sed 's/.*load average: \(.*\), \(.*\), \(.*\)/\1 \2 \3/'
