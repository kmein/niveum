{
  config,
  ...
}:
let
  retiolumKey = algorithm: {
    file = ../secrets/${config.networking.hostName}-retiolum-privateKey-${algorithm}.age;
    mode = "400";
  };
in
{
  networking.hosts = {
    # "42:0:ca48:f98f:63d7:31ce:922b:245d" = [ "go" ];
    # prism is down; cyberlocker (c.r), paste (p.r) and wallpaper.r moved to
    # neoprism, but kartei still lists them as prism aliases (lass/prism.nix).
    # paste.r is deliberately absent: neoprism has no vhost for it.
    "42:0:ce16::99" = [ "c.r" "p.r" "wallpaper.r" ];
    "10.243.0.99" = [ "c.r" "p.r" "wallpaper.r" ];
  };

  age.secrets = {
    retiolum-rsa = retiolumKey "rsa";
    retiolum-ed25519 = retiolumKey "ed25519";
  };

  networking.retiolum.ed25519PrivateKeyFile = config.age.secrets.retiolum-ed25519.path;
}
