{ pkgs, ... }:
{
  services.nginx.virtualHosts."xn--kiern-0qa.de" = {
    forceSSL = true;
    enableACME = true;
    globalRedirect = "kieranmeinhardt.de";
  };

  services.nginx.virtualHosts.${pkgs.lib.niveum.domain} = {
    forceSSL = true;
    enableACME = true;
    locations."/".return = "301 https://kieranmeinhardt.de/tech/";
  };
}
