{
  config,
  pkgs,
  ...
}:
let
  retiolumKey = algorithm: {
    file = ../secrets/${config.networking.hostName}-retiolum-privateKey-${algorithm}.age;
    mode = "400";
    owner = "tinc-retiolum";
    group = "tinc-retiolum";
  };
in
{
  networking.hosts = {
    "42:0:ca48:f98f:63d7:31ce:922b:245d" = [ "go" ];
  };

  age.secrets = {
    retiolum-rsa = retiolumKey "rsa";
    retiolum-ed25519 = retiolumKey "ed25519";
  };

  services.tinc.networks.retiolum = {
    rsaPrivateKeyFile = config.age.secrets.retiolum-rsa.path;
    ed25519PrivateKeyFile = config.age.secrets.retiolum-ed25519.path;
  };
}
