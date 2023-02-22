{
  config,
  pkgs,
  ...
}: {
  networking.hosts = {"42:0:ca48:f98f:63d7:31ce:922b:245d" = ["go"];};

  services.tinc.networks.retiolum = {
    rsaPrivateKeyFile = config.age.secrets.retiolum-rsa.path;
    ed25519PrivateKeyFile = config.age.secrets.retiolum-ed25519.path;
  };
}
