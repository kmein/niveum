{ pkgs, ... }:
let
  network = "retiolum";

  stateDirectory = "retiolum-map";

  geo-ip-database = pkgs.fetchurl {
    url = "http://c.krebsco.de/GeoLite2-City.mmdb";
    sha256 = "01lcmphcw4lgy02v9sa5xly991nsk0x0w6vm0dcr1mq6zg4b15v5";
  };
  tinc-graph-source = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "tinc-graph";
    rev = "50593204faa663422f95d7df2931382e7301f8d6";
    sha256 = "16cqsnyz6iyw8p395bmam4zvnmzvfz9lhxviscai9sa0imd7rwz2";
  };
  tinc-graph = pkgs.callPackage tinc-graph-source {};
in
{
  systemd.services.retiolum-index = {
    description = "Retiolum indexing service";
    wants = [ "tinc.${network}.service" ];
    script = ''
      ${tinc-graph}/bin/tinc-graph --geoip-file ${geo-ip-database} --network ${network} \
        | ${pkgs.coreutils}/bin/tee network.json \
        | ${tinc-graph}/bin/tinc-statistics > statistics.json

      cp ${tinc-graph}/static/map.html map.html
      cp ${tinc-graph}/static/graph.html graph.html
    '';
    startAt = "hourly";
    path = [ pkgs.coreutils pkgs.jq pkgs.tinc_pre ];
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      StateDirectory = stateDirectory;
      WorkingDirectory = "/var/lib/${stateDirectory}";
    };
  };

  services.geoip-updater = {
    enable = true;
    databases = [ "GeoLite2-City.mmdb.gz" ];
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."graph.r".locations."/".root = "/var/lib/${stateDirectory}";
  };
}
