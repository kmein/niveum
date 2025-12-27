{
  writers,
  lib,
  gnused,
  curl,
  jq,
  yq,
}:
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

  EVENTS=$(
    curl https://www.goodnewsnetwork.org/feed/ \
      | xq '
        .rss.channel.item
        | map(select((.pubDate|strptime("%a, %d %b %Y %H:%M:%S %z")) as $date | ($date | mktime) > (now - (60 * 60 * 24))) | {title, description})
      '
  )

  SYSTEM_PROMPT=$(cat <<EOF
  You are a news anchor writing a short news digest for a radio broadcast.
  Summarize the following news headlines into a cohesive, engaging script under 400 words.
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

  echo "$REQUEST" | curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-8b:generateContent?key=$GEMINI_API_KEY" -s -H "Content-Type: application/json" -d @-
''
