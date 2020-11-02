#! /bin/sh
# usage: mail-send < FILE
set -efu

get_in_reply_to() {
  sed -n '/^In-Reply-to:/I{s/In-Reply-to:\s*//I;h;:a;n;/^\s/{s/^\s*//;H;ba};x;p;q}' |
  sed -n 's/^<\(.*\)>$/\1/p' |
  grep .
}

now=$(date --rfc-email)
id=$(whoami)+$(date +%s -d "$now")@$(hostname -f)

# TODO check if mail with that ID already exists

# TODO encode subject https://ncona.com/2011/06/using-utf-8-characters-on-an-e-mail-subject/
#       and maybe recipients

# TODO use tmpfile instead?
mail=$(
  env now="$now" id="$id" \
  jq -Rrs '
    # TODO dedup with mail-reply
    split("\n") |
    index("") as $i |
    .[:$i] as $head |
    .[$i:] as $body |

    # TODO each of these could be followed by multiple lines starting with spaces
    ($head | map(select(test("^(Date|Message-ID|User-Agent):";"i") | not))) as $head |

    ($head + [
      "Date: \(env.now)",
      "Message-ID: <\(env.id)>",
      "User-Agent: much"
    ]) as $head |

    ($head + $body) | join("\n")
  '
)

printf %s "$mail" | msmtpq --read-recipients --read-envelope-from

# insertion is done upstream (by gmail, posteo, and the like)
# printf %s "$mail" | notmuch insert

if in_reply_to=$(printf %s "$mail" | get_in_reply_to); then
  if test "$(notmuch search --output=messages "id:$in_reply_to")" != "id:$in_reply_to"; then
    echo "while trying to put replied tag, failed to find exactly one message" >&2
    echo "  query = id:$in_reply_to" >&2
    exit 1
  fi

  notmuch tag +replied -unread -- "id:$in_reply_to"
fi

echo "id:$id"
