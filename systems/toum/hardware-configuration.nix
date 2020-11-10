{ config, pkgs, ... }: {
  boot.kernelPackages = pkgs.linuxPackages; # https://github.com/NixOS/nixpkgs/issues/97064#issuecomment-708986741
  boot.kernelParams = [ "cma=32M" ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  zramSwap.enable = true;
}
