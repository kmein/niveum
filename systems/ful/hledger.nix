{
  config,
  pkgs,
  ...
}: {
  services.hledger-web = {
    enable = true;
    allow = "edit";
    serveApi = false; # serve only the JSON API
    baseUrl = "https://ledger.kmein.de";
    journalFiles = [
      "privat.journal"
    ];
  };

  systemd.services.hledger-backup = {
    enable = true;
    startAt = "hourly";
    wants = ["network-online.target"];
    wantedBy = ["multi-user.target"];
    script = ''
      ${pkgs.git}/bin/git config user.name "hledger-web"
      ${pkgs.git}/bin/git config user.email "hledger-web@${config.networking.hostName}"
      ${pkgs.git}/bin/git commit -am $(date -Ih) || :
      ${pkgs.git}/bin/git pull --rebase
      ${pkgs.git}/bin/git push
    '';
    serviceConfig = {
      User = "root";
      Group = "root";
      WorkingDirectory = config.services.hledger-web.stateDir;
    };
  };

  age.secrets = {
    ledger-basicAuth = {
      file = ../../secrets/ledger-basicAuth.age;
      owner = "nginx";
      group = "nginx";
      mode = "400";
    };
  };

  services.nginx.virtualHosts."ledger.kmein.de" = {
    enableACME = true;
    basicAuthFile = config.age.secrets.ledger-basicAuth.path;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString config.services.hledger-web.port}";
  };
}
