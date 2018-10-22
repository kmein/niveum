{ config, lib, pkgs, ... }:
let
  scripts = import ./dot/scripts.nix { inherit pkgs; };
  helpers = import ./helpers.nix;
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    ./options.nix
    ./configs/shells.nix
    ./configs/editors.nix
    ./configs/graphics.nix
    ./configs/packages.nix
    ./configs/networks.nix
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

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  security.sudo.enable = true;

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

  programs.ssh = {
    startAgent = true;
    agentTimeout = "10m";
    knownHosts = [];
  };
  services.openssh.forwardX11 = true;

  programs.tmux = {
    enable = true;
    extraTmuxConf = import ./dot/tmux.nix;
    keyMode = "vi";
    terminal = "screen-256color";
  };

  # for kdeconnect
  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  };

  home-manager.users.kfm = {
    gtk = {
      enable = true;
      font = with import ./theme.nix; { package = pkgs.roboto; name = uiFont.name; };
      iconTheme = config.constants.theme.icon;
      theme = config.constants.theme.gtk;
    };

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
        pull-all = "!${scripts.git-pull-all}";
      };
      ignores = config.constants.ignore;
    };

    programs.rofi = with import ./theme.nix; {
      enable = true;
      separator = "solid";
      scrollbar = false;
      terminal = config.defaultApplications.terminal;
      borderWidth = 0;
      lines = 5;
      font = "${uiFont.name} ${toString (uiFont.size + 1)}";
      colors = rec {
        window = { background = black; border = black; separator = gray.dark; };
        rows = {
          normal = {
            background = window.background;
            backgroundAlt = window.background;
            foreground = gray.light;
            highlight = { foreground = white; inherit (window) background; };
          };
          active = {
            background = window.background;
            backgroundAlt = window.background;
            foreground = blue.dark;
            highlight = { foreground = blue.light; inherit (window) background; };
          };
          urgent = {
            background = window.background;
            backgroundAlt = window.background;
            foreground = red.dark;
            highlight = { foreground = red.light; inherit (window) background; };
          };
        };
      };
    };

    services.kdeconnect = {
      enable = true;
      indicator = true;
    };

    services.dunst = with import ./theme.nix; {
      enable = true;
      iconTheme = config.constants.theme.icon;
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
      ".background-image".source = ./art/37333571_p0_master1200.jpg;
      ".config/mpv/input.conf".text = import ./dot/mpv.nix;
      ".config/xfce4/terminal/terminalrc".text = import ./dot/terminal.nix;
      ".config/zathura/zathurarc".text = "set selection-clipboard clipboard";
      ".ghci".text = import ./dot/ghci.nix { inherit pkgs; };
      ".ssh/config".text = import ./dot/ssh.nix { inherit lib; };
      ".stack/config.yaml".text = import ./dot/stack.nix { user = config.constants.user; };
      ".zshrc".text = "# nothing to see here";
    };
  };
}
