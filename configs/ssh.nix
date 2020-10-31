{ pkgs, config, lib, ... }:
let
  inherit (import <niveum/lib>) sshPort;
  kmeinKeys = lib.strings.splitString "\n" (lib.strings.fileContents (pkgs.fetchurl {
    url = "https://github.com/kmein.keys";
    sha256 = "1b9gbpgihg7zc89ivsz0gs3najp0zg53rcknvzvkm0851fdzkryx";
  }));
in {
  services.xserver.displayManager.sessionCommands = "${pkgs.openssh}/bin/ssh-add";

  programs.ssh.startAgent = true;

  users.users.me.openssh.authorizedKeys.keys = kmeinKeys;

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
        hostname = "88.99.83.173";
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
        user = "kfm";
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
