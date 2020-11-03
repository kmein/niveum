{ pkgs, config, lib, ... }:
let
  inherit (import <niveum/lib>) sshPort kieran;
in {
  services.xserver.displayManager.sessionCommands = "${pkgs.openssh}/bin/ssh-add";

  programs.ssh.startAgent = true;

  users.users.me.openssh.authorizedKeys.keys = kieran.sshKeys pkgs;

  home-manager.users.me.programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
      };
      zaatar = {
        hostname = "zaatar.local";
        user = "root";
        port = sshPort;
      };
      makanek = {
        hostname = "makanek.r";
        user = "root";
        port = sshPort;
      };
      homeros = {
        hostname = "homeros.r";
        user = "kfm";
        port = sshPort;
      };
      toum = {
        hostname = "toum.r";
        user = "root";
        port = sshPort;
      };
      wilde = {
        hostname = "wilde.r";
        user = "kfm";
        port = sshPort;
      };
    };
  };
}
