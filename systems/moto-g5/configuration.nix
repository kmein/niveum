{
  config,
  lib,
  pkgs,
  ...
}: let
  sshPort = 8022;
in {
  environment.packages = with pkgs; [
    vim
    openssh
    curl
    findutils
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

  environment.etcBackupExtension = ".bak";

  system.stateVersion = "23.05";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
