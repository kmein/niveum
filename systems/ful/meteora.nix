{ config, pkgs, ... }: {
  age.secrets.meteora-auth = {
    file = ../../secrets/meteora-auth.age;
    owner = "nginx";
  };

  services.nginx = {
    enable = true;
    virtualHosts."meteora.xn--kiern-0qa.de" = {
      forceSSL = true;
      enableACME = true;
      root = "${pkgs.meteora-website}";
      locations."/" = {
        basicAuthFile = config.age.secrets.meteora-auth.path;
      };
    };
  };
}
