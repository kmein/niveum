#!/bin/sh
curl -sSL https://www.netzwelt.de/proxy/index.html \
  | pup ".tblc" \
  | xml-to-json /dev/stdin \
  | jq '
    .div.table.tbody.tr
    | map(
      .td
      | {
        ip: .[0].a.value,
        port: .[1],
        country: .[2] | (if type == "string" then . else .a.value end),
        security: .[3],
        protocol: .[4]
      }
    )
  '
