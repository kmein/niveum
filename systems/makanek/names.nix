{ pkgs, lib, ... }:
let
  port = 5703;
  geogen = (pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "scripts";
    rev = "8945430f27a8c6fd632dd35382cb094abe3543ff";
    sha256 = "1djyxkynypxsrmdf6idgjszqpcgqyq607rrsvl58p2bpymmwibzb";
  }) + "/onomastics";
  inherit (pkgs.callPackage geogen {}) dependencyEnv;
in
{
  systemd.services.names = {
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    description = "Better clone of geogen.stoepel.net";
    serviceConfig = {
      DynamicUser = true;
    };
    script = ''
      cd $(mktemp -d)
      ln -s "${geogen}/wsgi.py" wsgi.py
      ${dependencyEnv}/bin/gunicorn wsgi:app -b :${toString port}
    '';
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  services.nginx.virtualHosts."names.kmein.r" = {
    locations."/".proxyPass = "http://127.0.0.1:${toString port}";
  };
}
