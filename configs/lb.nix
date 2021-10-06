{ lib, pkgs, ... }:
let
  nixpkgs-unstable = import <nixpkgs-unstable> {};
in
{
  systemd.services.lb-subscription = {
    enable = true;
    wants = [ "network-online.target" ];
    startAt = "weekly";
    serviceConfig = {
      user = "kfm";
      WorkingDirectory = "/home/kfm/cloud/Seafile/Books/Germanistik/LB";
    };
    script = ''
      first_year=2019
      for year in $(${pkgs.coreutils}/bin/seq "$first_year" "$(date +%Y)"); do
        ${pkgs.curl}/bin/curl -sSL "https://www.literarische-blaetter.de/jahrgang-$year/" \
          | ${nixpkgs-unstable.htmlq}/bin/htmlq --attribute href 'ul.slides a' \
          | while read -r month; do
            ${pkgs.curl}/bin/curl -sSL "$month" \
              | ${nixpkgs-unstable.htmlq}/bin/htmlq --attribute src iframe \
              | ${pkgs.gnused}/bin/sed 's/.*?pdf=//;s/?wp-hosted.*//'
          done
      done | ${pkgs.findutils}/bin/xargs ${pkgs.wget}/bin/wget --no-clobber
    '';
  };
}
