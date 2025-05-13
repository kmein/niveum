{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import ../../lib) serveHtml;
  remote = "https://cgit.lassul.us/stockholm";
in {
  services.nginx.virtualHosts."redaktion.r".locations."/".extraConfig = serveHtml ../../lib/radio-news.html pkgs;

  age.secrets = {
    gemini-api-key.file = ../../secrets/gemini-api-key.age;
  };

  systemd.services.news-digest = {
    enable = true;
    wantedBy = ["multi-user.target"];
    wants = ["network-online.target"];
    serviceConfig.LoadCredential = [
      "gemini-api-key:${config.age.secrets.gemini-api-key.path}"
    ];
    startAt = "hourly";
    script = ''
      set -efu
      PATH=$PATH:${lib.makeBinPath [pkgs.w3m pkgs.gnused pkgs.curl pkgs.jq]}

      export GEMINI_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/gemini-api-key")"

      WIKI_URL="https://en.wikipedia.org/wiki/Portal:Current_events"

      EVENTS=$(w3m -dump "$WIKI_URL" | sed -n "/$(date -I)/,/$(date -I -d yesterday)/p" | head -n -1)

      SYSTEM_PROMPT=$(cat <<EOF
      You are a news anchor writing a short news digest for a radio broadcast.
      Summarize the following news headlines into a cohesive, engaging script under 400 words.
      Keep it professional, concise, and easy to follow.

      Begin the digest with: "Good news everybody! Here's your news update for $(date -u +"%B %d, %Y")..."
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
                  "text": "Current events (from Wikipedia): $(echo "$EVENTS")"

                }
              ]
            }
          ]
        }
      EOF
      )

      RESPONSE=$(echo "$REQUEST" | curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-8b:generateContent?key=$GEMINI_API_KEY" -s -H "Content-Type: application/json" -d @-)

      echo "$RESPONSE" | jq --arg from "$(date -u -Is | sed 's/+00:00/Z/')" --arg to "$(date -u -Is -d 'next hour' | sed 's/+00:00/Z/')" '
        {
          from: $from,
          to: $to,
          text: .candidates[].content.parts[].text
        }' | curl -s -X POST http://radio-news.r -H "Content-Type: application/json" -d @-
    '';
  };

  niveum.passport.services = [
    {
      title = "Retiolum Radio News";
      link = "http://redaktion.r";
      description = "supplies git history news to radio lassulus and lets you enter your own.";
    }
  ];
}
