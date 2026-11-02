{
  writers,
  lib,
  gnused,
  curl,
  jq,
  yq,
}:
let
  model = "gemini-2.5-flash-lite";
in
writers.writeBashBin "radio-news" ''
  set -efu
  PATH=$PATH:${
    lib.makeBinPath [
      gnused
      curl
      jq
      yq
    ]
  }

  # stdout carries gemini's answer, so the log goes to stderr
  log() { echo "$@" >&2; }

  log "fetching the good news feed"
  EVENTS=$(
    curl -sS --fail https://www.goodnewsnetwork.org/feed/ \
      | xq '
        .rss.channel.item
        | map(select((.pubDate|strptime("%a, %d %b %Y %H:%M:%S %z")) as $date | ($date | mktime) > (now - (60 * 60 * 24))) | {title, description})
      '
  )
  log "$(jq length <<< "$EVENTS") items from the last 24 hours:"
  jq -r '.[] | "- \(.title)"' <<< "$EVENTS" >&2

  SYSTEM_PROMPT=$(cat <<EOF
  You are a news anchor writing a short news digest for a radio broadcast.
  Summarize the following news headlines into a cohesive, engaging script under 100 words.
  Only take the best of news. They should be internationally applicable.
  Keep it professional, concise as possible, and easy to follow.
  Please no unnecessary explanations why the news is good.

  Begin the digest with: "Here's your good news update."
  EOF
  )

  REQUEST=$(cat <<EOF
  {
      "system_instruction": {
        "parts": [
          {
            "text": $(jq -Rs <<< "$SYSTEM_PROMPT")
          }
        ]
      },
      "contents": [
        {
          "parts": [
            {
              "text": $(jq -Rs <<< "$EVENTS")
            }
          ]
        }
      ]
    }
  EOF
  )

  log "asking ${model} for a digest"
  # the error body is the only clue to why gemini refused, so keep it in the journal
  RESPONSE=$(echo "$REQUEST" | curl "https://generativelanguage.googleapis.com/v1beta/models/${model}:generateContent?key=$GEMINI_API_KEY" -sS --fail-with-body -H "Content-Type: application/json" -d @-) || {
    log "$RESPONSE"
    exit 1
  }
  jq -r '"gemini finished (\(.candidates[0].finishReason), \(.usageMetadata.totalTokenCount) tokens)"' <<< "$RESPONSE" >&2
  echo "$RESPONSE"
''
