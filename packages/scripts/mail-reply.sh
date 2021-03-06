#! /bin/sh
set -efu

reply_to=$(much-current-query)

if ! test "$(notmuch search --output=messages "$reply_to" | wc -l)" = 1; then
  echo "current query doesn't point to exactly one message. abort." >&2
  exit 1
fi

# TODO update headers

notmuch reply "$reply_to" |
sed '
  /^Non-text part: /d
  /^Attachment: /d
' |
jq -Rrs '
  # TODO dedup with mail-send
  split("\n") |
  index("") as $i |
  .[:$i] as $head |
  .[$i:] as $body |

  {
    "MIME-Version": "1.0",
    "Content-Type": "text/plain; charset=UTF-8; format=flowed",
    "Content-Transfer-Encoding": "8bit"
  } as $extra_head |

  ($extra_head | keys | join("|")) as $extra_head_regex |
  ($extra_head | to_entries | map("\(.key): \(.value)")) as $extra_head_lines |

  # TODO each of these could be followed by multiple lines starting with spaces
  ($head | map(select(test("^(\($extra_head_regex)):";"i") | not))) as $head |

  ($head + $extra_head_lines) as $head |

  ($head + $body) | join("\n")
'


# TODO fix From:
# TODO tune quote

# TODO write draft
# TODO send mail
