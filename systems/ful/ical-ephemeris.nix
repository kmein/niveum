{ config, ... }:
let
  port = 9090;
in
{
  services.nginx.virtualHosts."ical-ephemeris.kmein.de" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString port}/";
    };
  };

  services.ical-ephemeris = {
    enable = true;
    inherit port;
    brevoApiKeyFile = config.age.secrets.brevo-key.path;
    copecartIpnKeyFile = config.age.secrets.copecart-ipn.path;
    baseUrl = "https://ical-ephemeris.kmein.de";
  };

  age.secrets.copecart-ipn = {
    file = ../../secrets/copecart-ipn.age;
  };

  age.secrets.brevo-key = {
    file = ../../secrets/brevo-key.age;
  };
}
