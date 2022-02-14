{ pkgs, lib, ... }:
let
  port = 5040;
  punkt = pkgs.fetchzip {
    url = "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/tokenizers/punkt.zip";
    sha256 = "113cv87dj5ml7g8pjm7psk4q1hrf0zqpmc945lmpdz91vp2wn1nc";
  };
  horoscopy-src = pkgs.fetchzip {
    url = "http://c.krebsco.de/horoscopy.tar.gz";
    stripRoot = false;
    hash = "sha256-KBAbCvayTEr4+cOHnMXHCBA+8RWDMiQF65xzP4fOdaE=";
  };
  horoscopy = import horoscopy-src;
in
{
  systemd.services.horoscopy = {
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    description = "AI astrologer";
    serviceConfig = {
      DynamicUser = true;
    };
    environment.NLTK_DATA = pkgs.linkFarm "punkt-tokenizers" [
      { name = "tokenizers/punkt"; path = punkt; }
    ];
    script = ''
      cd ${horoscopy-src}
      ${horoscopy.dependencyEnv}/bin/gunicorn wsgi:app -b :${toString port}
    '';
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  services.nginx.virtualHosts."horoscopy.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString port}";
  };
}
