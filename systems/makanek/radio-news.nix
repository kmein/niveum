{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.nginx.virtualHosts."redaktion.r" = {
    extraConfig = with pkgs.lib.niveum; nginxAllow (retiolumNetwork ++ ownAddresses);
    locations."/".extraConfig = pkgs.lib.niveum.serveHtml ../../packages/radio-news/index.html pkgs;
  };

  age.secrets = {
    gemini-api-key.file = ../../secrets/gemini-api-key.age;
  };

  systemd.services.news-digest = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = pkgs.lib.niveum.hardening // {
      DynamicUser = true;
      LoadCredential = [
        "gemini-api-key:${config.age.secrets.gemini-api-key.path}"
      ];
    };
    startAt = "*:50";
    script = ''
      set -o pipefail
      PATH=$PATH:${
        lib.makeBinPath [
          pkgs.gnused
          pkgs.curl
          pkgs.jq
        ]
      }

      DIGEST=$(GEMINI_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/gemini-api-key")" ${pkgs.radio-news}/bin/radio-news | jq --arg from "$(date -u -Is | sed 's/+00:00/Z/')" --arg to "$(date -u -Is -d 'now + 30 minutes' | sed 's/+00:00/Z/')" '
      {
        from: $from,
        to: $to,
        text: .candidates[].content.parts[].text
      }')
      echo "posting to radio-news.r: $DIGEST"
      curl -sS --fail -X POST http://radio-news.r -H "Content-Type: application/json" -d "$DIGEST" -w '\nradio-news.r answered %{http_code}\n'
    '';
  };

}
