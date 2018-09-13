{ config, lib, pkgs, ... }:
let
  theme = {
    gtk = { name = "Numix-SX-Dark"; package = pkgs.numix-sx-gtk-theme; };
    icon = { name = "Papirus-Adapta-Nokto"; package = pkgs.papirus-icon-theme; };
  };
  defaultApplications = {
    terminal = "${pkgs.xfce.terminal}/bin/xfce4-terminal";
    webBrowser = "${pkgs.chromium}/bin/chromium-browser";
    fileManager = "${pkgs.gnome3.nautilus}/bin/nautilus";
    screenLocker = with import ./theme.nix;
      "${pkgs.i3lock}/bin/i3lock -e -c ${lib.strings.removePrefix "#" black}";
  };
  wallpaper = pkgs.copyPathToStore ./art/haskell-grey.png;
  scripts = import ./dot/scripts.nix pkgs defaultApplications;
  constants = import ./constants.nix;
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides =
      let nix-writers = builtins.fetchGit { url = https://cgit.krebsco.de/nix-writers/; ref = "tags/v2.1.0"; };
      in import "${nix-writers}/pkgs" pkgs;
  };

  security.sudo.enable = true;
  security.sudo.extraConfig = "Defaults	insults";

  fonts.enableDefaultFonts = true;
  fonts.fonts = with pkgs; [ powerline-fonts roboto font-awesome-ttf fira-code eb-garamond lmodern ];

  environment.systemPackages = with pkgs; [
    theme.icon.package theme.gtk.package
    ffmpeg mpv youtubeDL
    imagemagick
    zathura
    chromium google-chrome firefox lynx w3m firefoxPackages.tor-browser
    lxappearance
    libnotify
    xfce.terminal
    pamixer
    gnome3.nautilus
    git
    ripgrep tree
    whois
    wget htop zip unzip
    rlwrap
    pmount
    gnumake
    (import ./dot/vim.nix pkgs)
  ];
  users.users.kfm = {
    createHome = true;
    description = constants.user.name;
    extraGroups = [ "wheel" ];
    group = "users";
    home = "/home/kfm";
    shell = pkgs.zsh;
    password = "kfm";
    packages = with pkgs; [
      texlive.combined.scheme-tetex
      franz
      grive2
      gnuplot maxima
      libreoffice-fresh
      kdeconnect
      par haskellPackages.pandoc haskellPackages.pandoc-citeproc biber
      haskellPackages.hakyll
      spotify gnome3.gnome-music audacity
      calibre
      inkscape
      stack haskellPackages.hasktags haskellPackages.hindent haskellPackages.ghcid haskellPackages.hoogle
      rustup
      gcc tinycc ctags
      python3 mypy
      nodejs jo
      perl ruby lua
      nasm
      ocaml fsharp swiProlog haskellPackages.idris
      clojure racket-minimal
      scala
    ];
  };

  environment.variables.EDITOR = pkgs.lib.mkForce "vim";

  environment.shellAliases =
    let rlwrap = cmd: "${pkgs.rlwrap}/bin/rlwrap ${cmd}";
    in {
      ":r" = ''echo "You stupid!"'';
      chrome-no-traces = "${pkgs.google-chrome}/bin/google-chrome-stable -incognito --user-data-dir=$HOME/.config/google-chrome/Incognito --disk-cache-dir=/dev/null --disk-cache-size=1";
      clipboard = "${pkgs.xclip}/bin/xclip -se c";
      external-ip = "${pkgs.dnsutils}/bin/dig +short myip.opendns.com @resolver1.opendns.com";
      ghc = "${pkgs.stack}/bin/stack ghc --";
      ghci = "${pkgs.stack}/bin/stack ghc -- --interactive";
      ip = "${pkgs.iproute}/bin/ip -c";
      ocaml = rlwrap "${pkgs.ocaml}/bin/ocaml";
      tmux = "${pkgs.tmux}/bin/tmux -2";
      vi = "vim";
    } // scripts;

  services.xserver = with import ./constants.nix; {
    enable = true;
    layout = commaSep [ "de" "gr" "ru" ];
    xkbVariant = commaSep [ "T3" "polytonic" "phonetic_winkeys" ];
    xkbOptions = commaSep [ "terminate:ctrl_alt_bksp" "grp:alt_space_toggle" ];
    libinput.enable = true;
    xautolock = {
      enable = true;
      time = 15;
      locker = defaultApplications.screenLocker;
      nowlocker = defaultApplications.screenLocker;
      enableNotifier = true;
      notifier = ''${pkgs.libnotify}/bin/notify-send -u normal -a xautolock "Locking soon" "The screen will lock in 10 seconds."'';
    };
    displayManager.auto = {
      enable = true;
      user = "kfm";
    };
    desktopManager.xterm.enable = false;
    desktopManager.wallpaper.mode = "fill";
    windowManager.default = "i3";
    windowManager.i3 = {
      enable = true;
      configFile = pkgs.writeText "i3.conf" (import ./dot/i3.nix pkgs defaultApplications);
      extraPackages = [];
    };
  };
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    consoleUseXkbConfig = true;
    consoleColors = with import ./theme.nix; map (c: lib.strings.removePrefix "#" c) colorPalette;
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 18 * * * ${scripts.bing-wallpaper}"
    ];
  };

  services.compton = {
    enable = true;
    fade = true;
    shadow = true;
    menuOpacity = "0.9";
    shadowOpacity = "0.5";
    fadeDelta = 2;
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

  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    syntaxHighlighting.highlighters = [ "main" "brackets" "pattern" "cursor" "root" "line" ];
    interactiveShellInit = ''
      setopt INTERACTIVE_COMMENTS
      setopt MULTIOS
      setopt AUTO_PUSHD
      setopt AUTO_NAME_DIRS
      setopt PUSHD_MINUS
      setopt PUSHD_TO_HOME
    '';
    promptInit = ''
      PROMPT="%{$fg_bold[white]%}%~ \$([[ \$? == 0 ]] && echo \"%{$fg_bold[green]%}\" || echo \"%{$fg_bold[red]%}\")%#%{$reset_color%} "
      RPROMPT='$(git_prompt_info)'

      ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[cyan]%}"
      ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
      ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*"
    '';
    ohMyZsh.enable = true;
    ohMyZsh.plugins = [ "common-aliases" ];

  };
  programs.bash = {
    promptInit = ''PS1="$(tput bold)\w \$([[ \$? == 0 ]] && echo \"\[\033[1;32m\]\" || echo \"\[\033[1;31m\]\")\$$(tput sgr0) '';
    enableCompletion = true;
  };

  programs.command-not-found.enable = true;

  programs.java.enable = true;
  programs.light.enable = true;

  programs.tmux = {
    enable = true;
    extraTmuxConf = import ./dot/tmux.nix;
    keyMode = "vi";
    terminal = "screen-256color";
  };

  programs.nano.nanorc = import ./dot/nano.nix;

  # networking.hostName = "scardanelli";
  networking.hosts = {
    "192.168.178.27" = [ "printer.local" ];
  };

  networking.wireless.enable = true;
  networking.wireless.networks = {
    Aether = { psk = "Kein ding sei wo das wort gebricht."; };
    "Asoziales Netzwerk" = { psk = "WirFragenDichNicht"; };
  };
  networking.wireless.userControlled.enable = true;

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
          dmenu = "${pkgs.rofi}/bin/rofi -display-run 'dunst: ' -show run";
          browser = defaultApplications.webBrowser;
          verbosity = "mesg";
          corner_radius = 0;
          mouse_left_click = "do_action";
          mouse_right_click = "close_current";
          mouse_middle_click = "close_all";
        };
        urgency_low = {
          frame_color = veryDark;
          background = veryDark;
          foreground = gray.light;
          timeout = 5;
        };
        urgency_normal = {
          frame_color = veryDark;
          background = gray.light;
          foreground = veryDark;
          timeout = 10;
        };
        urgency_critical = {
          frame_color = veryDark;
          background = red.dark;
          foreground = veryDark;
          timeout = 0;
        };
      };
    };

    home.file = {
      ".background-image".source = wallpaper;
      ".ghci".text = import ./dot/ghci.nix pkgs;
      ".stack/config.yaml".text = import ./dot/stack.nix constants.user;
      ".config/zathura/zathurarc".text = "set selection-clipboard clipboard";
      ".config/mpv/input.conf".text = import ./dot/mpv.nix;
      ".config/xfce4/terminal/terminalrc".text = import ./dot/terminal.nix defaultApplications;
      ".zshrc".text = "# nothing to see here";
    };
  };
}
