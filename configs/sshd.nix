{ config, lib, pkgs, ... }:
let
  inherit (import <niveum/lib>) sshPort;
  kmeinKeys = lib.strings.splitString "\n" (lib.strings.fileContents (pkgs.fetchurl {
    url = "https://github.com/kmein.keys";
    sha256 = "1b9gbpgihg7zc89ivsz0gs3najp0zg53rcknvzvkm0851fdzkryx";
  }));
in
{
  users.motd = "Welcome to ${config.networking.hostName}!";

  services.openssh = {
    enable = true;
    ports = [ sshPort ];
    passwordAuthentication = false;
    forwardX11 = true;
  };

  users.users.root.openssh.authorizedKeys.keys = kmeinKeys;
}
