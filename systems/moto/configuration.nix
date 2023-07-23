{
  config,
  lib,
  pkgs,
  inputs,
}: let
  sshPort = 8022;
in {
  environment.packages = with pkgs; [
    vim
    openssh
    curl
    findutils
    coreutils
    utillinux
    gnugrep
    gnused
    gnutar
    iproute2
    git
    which
    procps
    (pkgs.writers.writeDashBin "start-ssh" ''
      ${pkgs.openssh}/bin/sshd -f ${pkgs.writeText "sshd_config" ''
        HostKey /data/data/com.termux.nix/files/home/.ssh/ssh_host_rsa_key
        Port ${toString sshPort}
      ''}
    '')
  ];

  nixPath = ["nixpkgs=${inputs.nixpkgs}"];

  home-manager = {
    useGlobalPkgs = true;
    backupFileExtension = "hm-bak";
    config = {pkgs, ...}: {
      home.stateVersion = "23.05";
    };
  };

  user.shell = "${pkgs.fish}/bin/fish";

  environment.etcBackupExtension = ".bak";

  system.stateVersion = "23.05";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
