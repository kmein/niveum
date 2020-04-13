{ config, pkgs, ... }:
{
  imports = [ <niveum/modules/retiolum.nix> ];

  fileSystems."/media/flix" = {
    device = "prism.r:/export";
    fsType = "nfs";
  };

  networking.hosts = {
    "42:0:ca48:f98f:63d7:31ce:922b:245d" = [ "go" ];
  };

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <secrets/retiolum.key>;
    mode = "400";
  };
}
