{
  pkgs,
  lib,
  ...
}: let
  inherit (import <niveum/lib>) serveHtml;
  remote = "https://cgit.lassul.us/stockholm";
in {
  services.nginx.virtualHosts."redaktion.r".locations."/".extraConfig = serveHtml <niveum/lib/radio-news.html> pkgs;

  niveum.passport.services = [
    {
      title = "Retiolum Radio News";
      link = "http://redaktion.r";
      description = "supplies git history news to radio lassulus and lets you enter your own.";
    }
  ];

  systemd.services.stockholm-history = {
    startAt = "*:58:00";
    script = ''
      stockholm=$(mktemp -d)
      trap clean EXIT
      clean() {
        rm -rf "$stockholm"
      }
      ${pkgs.git}/bin/git clone ${remote} "$stockholm"
      ${pkgs.git}/bin/git --git-dir "$stockholm"/.git log --pretty='"%s" by %an, %ar.' --since "$(${pkgs.coreutils}/bin/date -d '1 hours ago')" \
        | ${pkgs.jq}/bin/jq -R '{text: ., from: now | todateiso8601, to: (now + (60 * 60)) | todateiso8601}' \
        | ${pkgs.curl}/bin/curl -Ssfd @- http://radio-news.r/
    '';
  };
}
