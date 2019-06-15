{ config, pkgs, ... }:
{
  imports = [ <modules/retiolum.nix> ];

  fileSystems."/mnt/lassulusflix" = {
    device = "prism.r:/export";
    fsType = "nfs";
  };

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
    catullus = {
      ipv4 = "10.243.2.3";
      ipv6 = "42:0:3c46:3ec0:7aad:d1d5:9842:da4c";
    };
    wilde = {
      ipv4 = "10.243.2.4";
      ipv6 = "42:0:3c46:907c:1fb8:b74f:c59b:1ee3";
    };
  }.${config.networking.hostName};

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = (import <dot/secrets.nix>).retiolum.privateKey.${config.networking.hostName};
    mode = "400";
  };
}
