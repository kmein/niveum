{
  pkgs,
  niveumPackages,
  inputs,
  ...
}: let
  sshPort = 8022;
in {
  environment.packages = with pkgs; [
    nil
    bzip2
    coreutils
    curl
    diffutils
    findutils
    git
    gnugrep
    gnupg
    gnused
    gnutar
    gzip
    hostname
    iproute2
    man
    openssh
    procps
    tzdata
    unzip
    util-linux
    vim
    which
    xz
    zip
    hledger
    hledger-ui
    niveumPackages.vim
    gitAndTools.gh
    ripgrep
    (pkgs.writers.writeDashBin "start-ssh" ''
      ${pkgs.openssh}/bin/sshd -f ${pkgs.writeText "sshd_config" ''
        HostKey /data/data/com.termux.nix/files/home/.ssh/ssh_host_rsa_key
        Port ${toString sshPort}
      ''}
    '')
  ];

  nix.nixPath = ["nixpkgs=${inputs.nixpkgs}"];

  home-manager = {
    useGlobalPkgs = true;
    backupFileExtension = "hm-bak";
    config = {pkgs, ...}: {
      home.stateVersion = "23.05";
      programs.tmux.enable = true;
    };
  };

  environment.sessionVariables = {
    LEDGER_FILE = "/data/data/com.termux.nix/files/home/src/ledger/privat.journal";
  };

  user.shell = "${pkgs.fish}/bin/fish";

  environment.etcBackupExtension = ".bak";

  system.stateVersion = "23.05";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
