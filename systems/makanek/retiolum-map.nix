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

  tincd = config.systemd.services."tincr-${network}";
  # tinc-graph calls plain `tinc -n NET`, which would look for the pidfile under /var/run
  tinc = pkgs.writers.writeDashBin "tinc" ''
    exec ${config.services.tincr.networks.${network}.package}/bin/tinc --pidfile=${tincd.serviceConfig.PIDFile} "$@"
  '';
in
{
  systemd.services.retiolum-index = {
    description = "Retiolum indexing service";
    wants = [ "tincr-${network}.service" ];
    after = [ "tincr-${network}.service" ];
    script = ''
      ${pkgs.tinc-graph}/bin/tinc-graph --geoip-file ${geo-ip-database-path} --network ${network} \
        | ${pkgs.coreutils}/bin/tee network.json \
        | ${pkgs.tinc-graph}/bin/tinc-midpoint > midpoint.json

      ln -sf ${pkgs.tinc-graph}/static/map.html map.html
      ln -sf ${pkgs.tinc-graph}/static/map.html index.html
      ln -sf ${pkgs.tinc-graph}/static/graph.html graph.html
    '';
    startAt = "hourly";
    path = [
      pkgs.coreutils
      pkgs.jq
      tinc
    ];
    # the control socket is 0700 and owned by tincd's user
    serviceConfig = pkgs.lib.niveum.hardening // {
      Type = "oneshot";
      User = tincd.serviceConfig.User;
      StateDirectory = stateDirectory;
      WorkingDirectory = "/var/lib/${stateDirectory}";
      UMask = "0022"; # served by nginx
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

  services.nginx = {
    virtualHosts."graph.r".locations."/".root = "/var/lib/${stateDirectory}";
    # RRM @ https://github.com/krebs/cholerab/blob/master/thesauron.adoc
    virtualHosts."rrm.r".locations."/".root = "/var/lib/${stateDirectory}";
  };

  systemd.services.geoip-share = {
    after = [ "geoipupdate.service" ];
    wantedBy = [ "geoipupdate.service" ];
    script = "${pkgs.curl}/bin/curl -fSs --data-binary @${geo-ip-database-path} http://c.r/${geo-ip-database} ";
    serviceConfig = pkgs.lib.niveum.hardening // {
      Type = "oneshot";
      DynamicUser = true;
    };
  };
}
