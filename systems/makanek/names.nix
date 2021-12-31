{ pkgs, lib, ... }:
let
  port = 5703;
  geogen = pkgs.callPackage "${<scripts>}/onomastics" {};
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
      ${geogen.dependencyEnv}/bin/gunicorn wsgi:app -b :${toString port}
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
