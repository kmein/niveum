{
  services.gitea = {
    enable = true;
    disableRegistration = true;
    rootUrl = "https://code.xn--kiern-0qa.de";
    appName = "code.kierán.de";
  };
  services.nginx.virtualHosts."code.xn--kiern-0qa.de" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig = "proxy_pass http://localhost:3000;";
  };
}
