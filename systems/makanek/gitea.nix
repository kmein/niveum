{
  services.gitea = {
    enable = true;
    disableRegistration = true;
    rootUrl = "https://code.kmein.de";
    appName = "code.kmein.de";
  };
  services.nginx.virtualHosts."code.kmein.de" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig = "proxy_pass http://localhost:3000;";
  };
}
