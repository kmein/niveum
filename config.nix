{ config, lib, pkgs, ... }:
let
  scripts = import ./dot/scripts.nix pkgs;
  constants = import ./constants.nix;
  wallpaper = pkgs.copyPathToStore ./art/haskell-grey.png;
  theme = {
    gtk = { name = "Numix-SX-Dark"; package = pkgs.numix-sx-gtk-theme; };
    icon = { name = "Papirus-Adapta-Nokto"; package = pkgs.papirus-icon-theme; };
  };
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    ./configs/shells.nix
    ./configs/editors.nix
    ./configs/graphics.nix
    ./configs/packages.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides =
      let nix-writers = builtins.fetchGit {
        url = https://cgit.krebsco.de/nix-writers/;
        ref = "tags/v3.0.0";
        # sha256 = "066y18q19d35x5jjr3kdn1dwi7s1l12icr90s2vxwzif6ahnzmb3";
      }; in import "${nix-writers}/pkgs" pkgs;
  };

  time.timeZone = "Europe/Berlin";

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  security.sudo.enable = true;
  security.sudo.extraConfig = "Defaults insults";

  fonts.enableDefaultFonts = true;

  users.mutableUsers = false;

  users.users.kfm = {
    name = "kfm";
    home = "/home/kfm";
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" "audio" ];
    password = "kfm";
    shell = pkgs.zsh;
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 18 * * * ${scripts.bing-wallpaper}"
    ];
  };

  services.openssh.enable = true;
  programs.ssh = {
    startAgent = true;
    knownHosts = [];
  };

  services.redshift = {
    enable = true;
    latitude = "52";
    longitude = "13";
    temperature = { night = 25000; day = 1000; };
  };

  programs.tmux = {
    enable = true;
    extraTmuxConf = import ./dot/tmux.nix;
    keyMode = "vi";
    terminal = "screen-256color";
  };

  # networking.hostName = "scardanelli";
  networking.hosts = {
    "192.168.178.27" = [ "printer.local" ];
  };

  networking.wireless.enable = true;
  networking.wireless.networks = {
    Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
    "Asoziales Netzwerk" = { pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6"; };
  };

  home-manager.users.kfm = {
    gtk = {
      enable = true;
      font = with import ./theme.nix; { package = pkgs.roboto; name = uiFont.name; };
      iconTheme = theme.icon;
      theme = theme.gtk;
    };

    programs.git = {
      enable = true;
      userName = constants.user.name;
      userEmail = constants.user.email;
      aliases = {
        br = "branch";
        co = "checkout";
        ci = "commit";
        amend = "commit --amend";
        st = "status";
        unstage = "reset HEAD --";
        sdiff = "diff --staged";
        last = "log -1 HEAD";
        pull-all = "!${scripts.git-pull-all}";
      };
      ignores = constants.ignoredFiles;
    };

    programs.rofi = with import ./theme.nix; {
      enable = true;
      separator = "solid";
      scrollbar = false;
      borderWidth = 0;
      lines = 5;
      font = "${uiFont.name} ${toString (uiFont.size + 1)}";
      colors = {
        window = { background = black; border = black; separator = gray.dark; };
        rows = {
          normal = {
            background = black;
            foreground = gray.light;
            backgroundAlt = black;
            highlight = { background = black; foreground = white; };
          };
        };
      };
    };

    services.kdeconnect.enable = true;

    services.dunst = with import ./theme.nix; {
      enable = true;
      iconTheme = theme.icon;
      settings = {
        global = {
          transparency = 10;
          font = "${uiFont.name} ${toString uiFont.size}";
          geometry = "200x5-30+20";
          frame_color = veryDark;
          follow = "mouse";
          indicate_hidden = true;
          notification_height = 0;
          separator_height = 2;
          padding = 8;
          horizontal_padding = 8;
          separator_color = "auto";
          sort = true;
          markup = "full";
          format = ''%a\n<b>%s</b>\n%b'';
          alignment = "left";
          show_age_threshold = 60;
          bounce_freq = 0;
          word_wrap = true;
          ellipsize = "middle";
          ignore_newline = false;
          stack_duplicates = true;
          hide_duplicate_count = false;
          max_icon_size = 32;
          sticky_history = true;
          history_length = 20;
          dmenu = "${pkgs.rofi}/bin/rofi -display-run dunst -show run";
          browser = "${pkgs.xdg_utils}/bin/xdg-open";
          verbosity = "mesg";
          corner_radius = 0;
          mouse_left_click = "do_action";
          mouse_right_click = "close_current";
          mouse_middle_click = "close_all";
        };
        urgency_low = { frame_color = veryDark; background = veryDark; foreground = gray.light; timeout = 5; };
        urgency_normal = { frame_color = veryDark; background = gray.light; foreground = veryDark; timeout = 10; };
        urgency_critical = { frame_color = veryDark; background = red.dark; foreground = veryDark; timeout = 0; };
      };
    };

    home.file = {
      ".background-image".source = wallpaper;
      ".ghci".text = import ./dot/ghci.nix pkgs;
      ".stack/config.yaml".text = import ./dot/stack.nix constants.user;
      ".config/zathura/zathurarc".text = "set selection-clipboard clipboard";
      ".config/mpv/input.conf".text = import ./dot/mpv.nix;
      ".config/xfce4/terminal/terminalrc".text = import ./dot/terminal.nix;
      ".zshrc".text = "# nothing to see here";
    };
  };
}
