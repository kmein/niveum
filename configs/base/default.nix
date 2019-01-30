{ config, pkgs, ... }:
{
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    ./editors.nix
    ./networks.nix
    ./scripts.nix
    ./shells.nix
    ../../options.nix
  ];

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

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    consoleKeyMap = "de";
    # consoleColors = with import ../theme.nix; map (c: lib.strings.removePrefix "#" c) colorPalette;
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

  users.mutableUsers = false;

  users.users.kfm = {
    name = "kfm";
    description = config.constants.user.name;
    home = "/home/kfm";
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" "audio" ];
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
        amend = "commit --amend";
        st = "status";
        unstage = "reset HEAD --";
        sdiff = "diff --staged";
        last = "log -1 HEAD";
        pull-all = "!pull-all"; # from dot/scripts.nix
      };
      ignores = config.constants.ignore;
    };

    home.file = {
      ".config/htop/htoprc".text = import ../../dot/htop.nix;
      ".zshrc".text = "# nothing to see here";
    };
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
  ] ++ [ # internet
    aria2
    w3m
    wget
    curl
    httpie
    whois
  ] ++ [ # media
    imagemagick
  ] ++ [ # archive
    unzip
    unrar
    p7zip
    zip
  ] ++ [ # monitor
    htop
    iotop
    iftop
    lsof
    psmisc
  ] ++ [ # shell
    bat
    dos2unix
    fd
    file
    git
    gitAndTools.hub
    gitstats
    jo
    jq
    manpages
    patch
    patchutils
    posix_man_pages
    most
    ranger
    ripgrep
    rlwrap
    tree
  ] ++ [ # hardware
    pmount
    usbutils
    pciutils
  ];

  programs.command-not-found.enable = true;
  programs.java = {
    enable = true;
    package = pkgs.openjdk;
  };

}
