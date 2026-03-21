{
  config,
  lib,
  pkgs,
  ...
}:
let
  publicLocations = builtins.listToAttrs (
    map (name: lib.nameValuePair name { }) [
      "= /index.html"
      "= /veroeffentlichungen.html"
      "= /lesungen.html"
      "= /kolophon.html"
      "= /impressum.html"
      "= /credo.html"
      "= /"
      "= /meteora.css"
      "= /favicon.ico"
      "/img/"
      "/fonts/"
    ]
  );
in
{
  age.secrets.meteora-auth = {
    file = ../../secrets/meteora-auth.age;
    owner = "nginx";
  };

  services.nginx = {
    enable = true;
    virtualHosts."meteora.xn--kiern-0qa.de" = {
      forceSSL = true;
      enableACME = true;
      root = "${pkgs.meteora-website}";
      locations = {
        "/" = {
          basicAuthFile = config.age.secrets.meteora-auth.path;
        };
      }
      // publicLocations;
    };
  };
}
