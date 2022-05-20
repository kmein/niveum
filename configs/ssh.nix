{
  pkgs,
  config,
  lib,
  ...
}: let
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
        hostname = "zaatar.r";
        user = "root";
        port = sshPort;
      };
      makanek = {
        hostname = "88.99.83.173";
        user = "root";
        port = sshPort;
      };
      "makanek.r" = {
        hostname = "makanek.r";
        user = "root";
        port = sshPort;
      };
      tahina = {
        hostname = "tahina.r";
        user = "root";
        port = sshPort;
      };
      manakish = {
        hostname = "manakish.r";
        user = "kfm";
        port = sshPort;
      };
      toum = {
        hostname = "toum.r";
        user = "root";
        port = sshPort;
      };
      kabsa = {
        hostname = "kabsa.r";
        user = "kfm";
        port = sshPort;
      };
      "nextcloud.fysi.dev" = {
        hostname = "116.203.82.203";
        user = "root";
      };
      "lingua.miaengiadina.ch" = {
        hostname = "135.181.85.233";
        user = "root";
      };
      "ful.r" = {
        hostname = "ful.r";
        user = "root";
        port = sshPort;
      };
      ful = {
        hostname = "130.61.209.15";
        user = "root";
        port = sshPort;
      };
    };
  };
}
