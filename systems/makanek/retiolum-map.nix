{
  config,
  pkgs,
  lib,
  ...
}:
let
  network = "retiolum";

  stateDirectory = "retiolum-map";

  geo-ip-database = "${lib.head config.services.geoipupdate.settings.EditionIDs}.mmdb";
  geo-ip-database-path = "${config.services.geoipupdate.settings.DatabaseDirectory}/${geo-ip-database}";
in
{
  systemd.services.retiolum-index = {
    description = "Retiolum indexing service";
    wants = [ "tinc.${network}.service" ];
    script = ''
      ${pkgs.tinc-graph}/bin/tinc-graph --geoip-file ${geo-ip-database-path} --network ${network} \
        | ${pkgs.coreutils}/bin/tee network.json \
        | ${pkgs.tinc-graph}/bin/tinc-midpoint > midpoint.json

      cp ${pkgs.tinc-graph}/static/map.html map.html
      cp ${pkgs.tinc-graph}/static/map.html index.html
      cp ${pkgs.tinc-graph}/static/graph.html graph.html
    '';
    startAt = "hourly";
    path = [
      pkgs.coreutils
      pkgs.jq
      pkgs.tinc_pre
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      StateDirectory = stateDirectory;
      WorkingDirectory = "/var/lib/${stateDirectory}";
    };
  };

  services.geoipupdate = {
    enable = true;
    settings = {
      AccountID = 608777;
      LicenseKey._secret = config.age.secrets.maxmind-license-key.path;
      EditionIDs = [ "GeoLite2-City" ];
    };
  };

  age.secrets.maxmind-license-key.file = ../../secrets/maxmind-license-key.age;

  niveum.passport.services = [
    {
      link = "http://graph.r";
      title = "Retiolum Realtime Map";
      description = "displays geographical information about the retiolum network. <a href=\"http://graph.r/graph.html\">Graph</a> info also available.";
    }
    {
      link = "http://c.r/${geo-ip-database}";
      title = "GeoIP";
      description = "shares MaxMind's GeoIP database with the krebs world. Updated weekly.";
    }
  ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."graph.r".locations."/".root = "/var/lib/${stateDirectory}";
    # RRM @ https://github.com/krebs/cholerab/blob/master/thesauron.adoc
    virtualHosts."rrm.r".locations."/".root = "/var/lib/${stateDirectory}";
  };

  systemd.services.geoip-share = {
    after = [ "geoipupdate.service" ];
    wantedBy = [ "geoipupdate.service" ];
    script = "${pkgs.curl}/bin/curl -fSs --data-binary @${geo-ip-database-path} http://c.r/${geo-ip-database} ";
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
    };
  };
}
