{ config, pkgs, ... }:
{
  services.nginx.virtualHosts."ical-ephemeris.kmein.de" = {
    addSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://localhost:9090";
    };
  };

  services.ical-ephemeris = {
    enable = true;
    port = 9090;
    brevoApiKeyFile = config.age.secrets.brevo-key.path;
    baseUrl = "https://ical-ephemeris.kmein.de";
  };

  age.secrets.brevo-key = {
    file = ../../secrets/brevo-key.age;
    owner = "ical-ephemeris";
    group = "ical-ephemeris";
    mode = "0400";
  };
}
