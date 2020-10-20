{ pkgs, config, lib, ... }:
let
  sshPort = 22022;
  kmeinKeys = lib.strings.splitString "\n" (lib.strings.fileContents (pkgs.fetchurl {
    url = "https://github.com/kmein.keys";
    sha256 = "1b9gbpgihg7zc89ivsz0gs3najp0zg53rcknvzvkm0851fdzkryx";
  }));
in {
  services.xserver.displayManager.sessionCommands =
    "${pkgs.openssh}/bin/ssh-add";

  programs.ssh.startAgent = true;

  services.openssh = {
    ports = [ sshPort ];
    enable = true;
    passwordAuthentication = false;
    forwardX11 = true;
  };

  users.motd = "Welcome to ${config.networking.hostName}!";

  users.users.root.openssh.authorizedKeys.keys = kmeinKeys;
  users.users.me.openssh.authorizedKeys.keys = kmeinKeys;

  home-manager.users.me.programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
      };
      scardanelli = {
        hostname = "scardanelli.r";
        user = "kfm";
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
