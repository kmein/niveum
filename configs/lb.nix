{
  lib,
  pkgs,
  ...
}:
let
  directory = "/home/kfm/cloud/nextcloud/Books/Germanistik/LB";
in
{
  systemd.services.lb-subscription = {
    enable = true;
    wants = [ "network-online.target" ];
    startAt = "weekly";
    serviceConfig = pkgs.lib.niveum.hardening // {
      User = "kfm";
      WorkingDirectory = directory;
      # an empty home with only the download directory in it
      ProtectHome = "tmpfs";
      BindPaths = [ directory ];
    };
    script = ''
      first_year=2019
      for year in $(${pkgs.coreutils}/bin/seq "$first_year" "$(date +%Y)"); do
        ${pkgs.curl}/bin/curl -sSL "https://www.literarische-blaetter.de/jahrgang-$year/" \
          | ${pkgs.htmlq}/bin/htmlq --attribute href 'ul.slides a' \
          | while read -r month; do
            ${pkgs.curl}/bin/curl -sSL "$month" \
              | ${pkgs.htmlq}/bin/htmlq --attribute src iframe \
              | ${pkgs.gnused}/bin/sed 's/.*?pdf=//;s/?wp-hosted.*//'
          done
      done | ${pkgs.findutils}/bin/xargs ${pkgs.wget}/bin/wget --no-clobber
    '';
  };
}
