let
  domain = "feed.kmein.de";
in {
  services.freshrss = {
    enable = true;
    baseUrl = "https://${domain}";
    dataDir = "/var/lib/freshrss";
    database.type = "sqlite";
    defaultUser = "k";
    passwordFile = toString <secrets/freshrss/password>;
    virtualHost = domain;
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
  };
}
