{ config, lib, pkgs, ... }:
let
  helpers = import <niveum/lib>;
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-18.09.tar.gz}/nixos"
    <niveum/options.nix>
    <niveum/configs/hu-berlin.nix>
    <niveum/configs/shells.nix>
    <niveum/configs/editors.nix>
    <niveum/configs/graphics.nix>
    <niveum/configs/packages.nix>
    <niveum/configs/networks.nix>
    <niveum/configs/scripts.nix>
    <niveum/configs/retiolum.nix>
  ];

  boot.cleanTmpDir = true;
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
    extraTmuxConf = import <niveum/dot/tmux.nix>;
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
      "mpv/input.conf".text = import <niveum/dot/mpv.nix>;
      "htop/htoprc".text = builtins.readFile <niveum/dot/htoprc>;
      "zathura/zathurarc".text = "set selection-clipboard clipboard";
      "pycodestyle".text = ''
        [pycodestyle]
        max-line-length = 110
      '';
    };

    home.file = {
      ".ghc/ghci.conf".text = import <niveum/dot/ghci.nix> { inherit pkgs; };
      ".stack/config.yaml".text = import <niveum/dot/stack.nix> { user = config.constants.user; };
      ".zshrc".text = "# nothing to see here";
    };
  };
}
