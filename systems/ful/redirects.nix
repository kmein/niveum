{
  services.nginx.virtualHosts."xn--kiern-0qa.de" = {
    forceSSL = true;
    enableACME = true;
    globalRedirect = "kieranmeinhardt.de";
  };
}
