{ config, pkgs, ... }:
let
  sshKey.rilke = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDWRcTILWzSzOAWqwBjJC97K9wKm+pfxk15L5AiTUlUMyBzVQsU+d2jUEe1rmp+kjVXL0qgNMI+eBzEAzaafUMdCPeIkMyeiOlXaYxq8NHApcZUSYV9CmzWqePj8Dlu8uexoF3TJi6tj3mmGM8BY50qkwk4mlT/5xLPPnh/HHyFmoZlj6183Z4vJRnK8UZ6x2gevVaE36lP27MikZ/MQ6/PaHJ5TNZy63rQwzKXw6ZQMx4JU22CwyZqPn1wjlGEkpJFOBKtnypURdVBsPydaZd/I7b/13FMwso2hSUoXqeV6iaeno2FWOrB3cAaFogNWKPRkEacKEE5mQOvLGGdG1Xp u0_a138@localhost";
in {
  imports = [
    <niveum/configs/default.nix>
    ./hardware-configuration.nix
  ];

  services.xserver.xrandrHeads = [ "eDP1" ];

  users.users.me.openssh.authorizedKeys.keys = [
    sshKey.rilke
  ];

  niveum = {
    batteryBlocks.default = "BAT1";
    networkInterfaces.wireless = "wlp2s0";
    promptColours.success = "yellow";
  };

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "scardanelli";

  networking.retiolum = {
    ipv4 = "10.243.2.2";
    ipv6 = "42:0:3c46:4007:5bce:f1bc:606b:2b18";
  };

  system.stateVersion = "18.09";
}
