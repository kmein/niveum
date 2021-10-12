{ pkgs, lib, ... }:
let
  port = 5703;
  geogen = (pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "scripts";
    rev = "c553c212efb04c300edf675c39a87fffd32d4def";
    sha256 = "0b5xjr7qqgvwc76pqr84p81svzk6nqr5pdvc6daw94rcgdy02mva";
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
