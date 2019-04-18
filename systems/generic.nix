{ config, lib, pkgs, ... }:
let
  helpers = import <lib>;
in {
  imports = [
    <home-manager/nixos>
    <modules/defaultApplications.nix>
    <configs/hu-berlin.nix>
    <configs/shells.nix>
    <configs/editors.nix>
    <configs/graphics.nix>
    <configs/packages.nix>
    <configs/networks.nix>
    <configs/scripts.nix>
    <configs/retiolum.nix>
  ];

  boot.cleanTmpDir = true;
  boot.loader.timeout = 1;
  boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];

  time.timeZone = "Europe/Berlin";

  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull; # for bluetooth sound output
  };

  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  security.sudo = {
    enable = true;
    extraConfig = ''
      Defaults pwfeedback
    '';
  };

  systemd.services.google-drive = {
    description = "Google Drive synchronisation service";
    wants = [ "network-online.target" ];
    script = ''
      ${pkgs.grive2}/bin/grive -p ${config.users.users.kfm.home}/cloud/gdrive
    '';
    startAt = "*:0/5";
    serviceConfig = {
      Type = "oneshot";
      User = config.users.users.kfm.name;
    };
  };

  programs.tmux = {
    enable = true;
    extraTmuxConf = import <dot/tmux.nix>;
    keyMode = "vi";
    terminal = "screen-256color";
  };

  users.mutableUsers = false;

  users.users.kfm = {
    name = "kfm";
    description = config.constants.user.name;
    home = "/home/kfm";
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" "audio" "docker" ];
    hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
    shell = pkgs.zsh;
  };

  home-manager.users.kfm = {
    programs.git = {
      enable = true;
      userName = config.constants.user.name;
      userEmail = config.constants.user.email;
      aliases = {
        br = "branch";
        co = "checkout";
        ci = "commit";
        cm = "commit -m";
        amend = "commit --amend";
        st = "status";
        unstage = "reset HEAD --";
        diffs = "diff --staged";
        last = "log -1 HEAD";
        logs = "log --pretty=oneline";
        pull-all = "!pull-all"; # from dot/scripts.nix
      };
      ignores = config.constants.ignore;
    };

    xdg.configFile = {
      "mpv/input.conf".text = import <dot/mpv.nix>;
      "htop/htoprc".text = builtins.readFile <dot/htoprc>;
      "zathura/zathurarc".text = "set selection-clipboard clipboard";
      "pycodestyle".text = ''
        [pycodestyle]
        max-line-length = 110
      '';
    };

    home.file = {
      ".ghc/ghci.conf".text = import <dot/ghci.nix> { inherit pkgs; };
      ".stack/config.yaml".text = import <dot/stack.nix> { user = config.constants.user; };
      ".zshrc".text = "# nothing to see here";
    };
  };
}
