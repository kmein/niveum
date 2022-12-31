{
  config,
  pkgs,
  ...
}: {
  imports = [
    <retiolum/modules/retiolum>
  ];

  networking.hosts = {"42:0:ca48:f98f:63d7:31ce:922b:245d" = ["go"];};

  services.tinc.networks.retiolum = {
    rsaPrivateKeyFile = toString <system-secrets/retiolum.key>;
    ed25519PrivateKeyFile = toString <system-secrets/retiolum.ed25519>;
  };
}
