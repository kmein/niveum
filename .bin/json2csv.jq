#!/usr/bin/env -S jq -r -f
(map(keys) | add | unique) as $cols
| map(. as $row | $cols | map($row[.])) as $rows
| $cols, $rows[]
| @csv
