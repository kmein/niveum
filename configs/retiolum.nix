{
  config,
  pkgs,
  ...
}: {
  imports = [
    <retiolum/modules/retiolum>
  ];

  networking.hosts = {"42:0:ca48:f98f:63d7:31ce:922b:245d" = ["go"];};
}
