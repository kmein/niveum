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
    startAt = "*:50";
    script = ''
      PATH=$PATH:${lib.makeBinPath [pkgs.gnused pkgs.curl pkgs.jq]}

      GEMINI_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/gemini-api-key")" ${pkgs.radio-news}/bin/radio-news | jq --arg from "$(date -u -Is | sed 's/+00:00/Z/')" --arg to "$(date -u -Is -d 'now + 30 minutes' | sed 's/+00:00/Z/')" '
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
