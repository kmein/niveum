let
  port = 9505;
in
{
  services.pr-notifier = {
    enable = true;
    inherit port;
    domain = "https://pr.kmein.de";
  };

  services.nginx.virtualHosts."pr.kmein.de" = {
    enable = true;
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString port}";
    };
  };
}
