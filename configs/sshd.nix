{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) sshPort kieran;
in {
  users.motd = "Welcome to ${config.networking.hostName}!";

  services.openssh = {
    enable = true;
    ports = [sshPort];
    passwordAuthentication = false;
    forwardX11 = true;
  };

  users.users.root.openssh.authorizedKeys.keys = kieran.sshKeys pkgs;
}
