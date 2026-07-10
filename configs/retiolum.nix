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
    # prism is down; its services (cyberlocker c.r, paste p.r/paste.r, wallpaper.r)
    # moved to neoprism, but krebs/retiolum's etc.hosts still points them at prism.
    "42:0:ce16::99" = [ "c.r" "p.r" "paste.r" "wallpaper.r" ];
    "10.243.0.99" = [ "c.r" "p.r" "paste.r" "wallpaper.r" ];
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
