{
  config,
  ...
}:
{
  # tincr speaks only sptps, so the ed25519 key is the only one it reads
  age.secrets.retiolum-ed25519 = {
    file = ../secrets/${config.networking.hostName}-retiolum-privateKey-ed25519.age;
    mode = "400";
  };

  networking.retiolum.ed25519PrivateKeyFile = config.age.secrets.retiolum-ed25519.path;
}
