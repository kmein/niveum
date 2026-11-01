{
  config,
  pkgs,
  ...
}:
let
  domain = "ledger.${pkgs.lib.niveum.domain}";
in
{
  services.hledger-web = {
    enable = true;
    allow = "edit";
    serveApi = false; # serve only the JSON API
    baseUrl = "https://${domain}";
    journalFiles = [
      "privat.journal"
    ];
  };

  # the journals are private finances; keep them away from other service users
  systemd.services.hledger-web.serviceConfig = {
    StateDirectoryMode = "0700";
    UMask = "0077";
  };

  age.secrets = {
    ledger-basicAuth = {
      file = ../../secrets/ledger-basicAuth.age;
      owner = "nginx";
      group = "nginx";
      mode = "400";
    };
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    basicAuthFile = config.age.secrets.ledger-basicAuth.path;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString config.services.hledger-web.port}";
  };
}
