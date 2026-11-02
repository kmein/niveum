{ lib, pkgs, ... }:
{
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
    # without an explicit default, nginx serves the lexicographically first vhost
    # to any unknown Host, including ones meant only for the overlays
    virtualHosts."_" = {
      default = lib.mkDefault true;
      rejectSSL = true;
      locations."/".return = "444";
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = pkgs.lib.niveum.kieran.email;
  };
}
