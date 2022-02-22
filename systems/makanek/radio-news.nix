{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) serveHtml;
  remote = "https://cgit.lassul.us/stockholm";
in
{
  services.nginx.virtualHosts."redaktion.r".locations."/".extraConfig = serveHtml <niveum/lib/radio-news.html> pkgs;

  systemd.services.stockholm-history = {
    startAt = "hourly";
    script = ''
      stockholm=$(mktemp -d)
      trap clean EXIT
      clean() {
        rm -rf "$stockholm"
      }
      ${pkgs.git}/bin/git clone ${remote} "$stockholm"
      ${pkgs.git}/bin/git --git-dir "$stockholm"/.git log --pretty='"%s" by %an, %ar' --since "$(${pkgs.coreutils}/bin/date -I -d "yesterday")" \
        | ${pkgs.jq}/bin/jq -R '{text: ., from: now | todateiso8601, to: (now + (60 * 60)) | todateiso8601}' \
        | ${pkgs.curl}/bin/curl -Ssfd @- http://prism.r:7999/
    '';
  };
}
