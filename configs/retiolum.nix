{ config, pkgs, ... }:
{
  imports = [ <niveum/modules/retiolum.nix> ];

  networking.hosts = {
    "42:0:ca48:f98f:63d7:31ce:922b:245d" = [ "go" ];
  };

  networking.retiolum = {
    scardanelli = {
      ipv4 = "10.243.2.2";
      ipv6 = "42:0:3c46:4007:5bce:f1bc:606b:2b18";
    };
    homeros = {
      ipv4 = "10.243.2.1";
      ipv6 = "42:0:3c46:53e:e63d:e62a:56ea:c705";
    };
  }.${config.networking.hostName};

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = (import <niveum/secrets.nix>).retiolum.privateKey.${config.networking.hostName};
    mode = "400";
  };
}
