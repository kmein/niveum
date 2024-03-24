{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (import ../lib) sshPort kieran;
in {
  users.motd = "Welcome to ${config.networking.hostName}!";

  services.openssh = {
    enable = true;
    ports = [sshPort];
    settings = {
      PasswordAuthentication = false;
      X11Forwarding = true;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = kieran.sshKeys;
}
