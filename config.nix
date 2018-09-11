{ config, lib, pkgs, ... }:
let
  fullName = "Kierán Meinhardt";
  fullEmail = "kieran.meinhardt@gmail.com";
  theme = {
    # gtk = { name = "Adwaita"; package = pkgs.gnome3.gnome_themes_standard; };
    gtk = { name = "Numix-SX-FullDark"; package = pkgs.numix-sx-gtk-theme; };
    icon = { name = "Papirus-Adapta-Nokto"; package = pkgs.papirus-icon-theme; };
    # icon = { name = "Adwaita"; package = pkgs.gnome3.adwaita-icon-theme; };
  };
  defaultApplications = {
    terminal = "${pkgs.xfce.terminal}/bin/xfce4-terminal";
    webBrowser = "${pkgs.google-chrome}/bin/google-chrome-stable";
    fileManager = "${pkgs.gnome3.nautilus}/bin/nautilus";
  };
  wallpaper = pkgs.copyPathToStore ./art/haskell-grey.png;
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];

  nixpkgs.config = {
    packageOverrides =
      let nix-writers = builtins.fetchGit {
        url = https://cgit.krebsco.de/nix-writers/;
        ref = "tags/v2.1.0";
      }; in import "${nix-writers}/pkgs" pkgs;
    allowUnfree = true;
  };

  security.sudo.enable = true;

  fonts.fonts = with pkgs; [ powerline-fonts roboto font-awesome-ttf fira-code eb-garamond lmodern ];
  environment.systemPackages = [ theme.icon.package theme.gtk.package] ++ (with pkgs; [
    ffmpeg mpv youtubeDL
    imagemagick
    zathura
    google-chrome firefox lynx w3m firefoxPackages.tor-browser
    lxappearance
    libnotify
    xfce.terminal
    xorg.xbacklight pamixer
    gnome3.nautilus
    git
    ripgrep tree
    whois
    wget htop zip unzip tmux
    rlwrap
    pmount
    gnumake
  ]);
  users.users.kfm = {
    createHome = true;
    description = fullName;
    extraGroups = [ "wheel" "networkmanager" ];
    group = "users";
    home = "/home/kfm";
    shell = pkgs.zsh;
    password = "kfm";
    packages = with pkgs; [
      texlive.combined.scheme-minimal
      franz
      grive2
      gnuplot maxima
      libreoffice-fresh
      kdeconnect
      par haskellPackages.pandoc biber
      spotify gnome3.gnome-music audacity
      calibre
      inkscape
      stack haskellPackages.hasktags
      rustup
      gcc tinycc ctags
      python3 mypy
      nodejs jo
      perl ruby lua
      nasm
      ocaml fsharp swiProlog haskellPackages.idris
      clojure racket-minimal
      jdk scala
    ];
  };


  environment.shellAliases =
  let rlwrap = cmd: "${pkgs.rlwrap}/bin/rlwrap ${cmd}"; in
  {
    ip = "${pkgs.iproute}/bin/ip -c";
    vi = "vim";
    ocaml = rlwrap "${pkgs.ocaml}/bin/ocaml";
    tmux = "${pkgs.tmux}/bin/tmux -2";
    clipboard = "${pkgs.xclip}/bin/xclip -se c";
    ghc = "${pkgs.stack}/bin/stack ghc --";
    ghci = "${pkgs.stack}/bin/stack ghc -- --interactive";
    external-ip = "${pkgs.dnsutils}/bin/dig +short myip.opendns.com @resolver1.opendns.com";
  };

  programs.slock.enable = true;

  services.xserver = {
    enable = true;
    layout = "de, gr, ru";
    xkbVariant = "T3, polytonic, phonetic_winkeys";
    xkbOptions = "terminate:ctrl_alt_bksp, grp:alt_space_toggle";
    libinput.enable = true;
    xautolock = {
      enable = true;
      time = 15;
      locker = "${pkgs.slock}/bin/slock";
      nowlocker = "${pkgs.slock}/bin/slock";
      enableNotifier = true;
      notifier = ''${pkgs.libnotify}/bin/notify-send "Locking soon."'';
    };
    /*
    displayManager.lightdm = {
      enable = true;
      background = wallpaper;
      greeters.gtk = {
        clock-format = "%F";
        iconTheme = theme.icon;
        theme = theme.gtk;
        extraConfig = with import ./theme.nix; ''
          font-name = ${uiFont.name} ${toString uiFont.size}
          xft-antialias = true
          xft-dpi = 96
          xft-hintstyle = slight
          xft-rgba = rgb
        '';
      };
    };
    */
    displayManager.auto = {
      enable = true;
      user = "kfm";
    };
    desktopManager.xterm.enable = false;
    desktopManager.wallpaper.mode = "fill";
    windowManager.default = "i3";
    windowManager.i3 = {
      enable = true;
      configFile = pkgs.writeText "i3.conf" (import ./dot/i3.nix {
        inherit pkgs defaultApplications wallpaper;
      });
    };
  };
  i18n.consoleUseXkbConfig = true;

  services.compton = {
    enable = true;
    fade = true;
    shadow = true;
    fadeDelta = 2;
    menuOpacity = "0.95";
  };

  services.openssh.enable = true;

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
      PROMPT="%{$fg_bold[white]%}%~ \$([[ \$? == 0 ]] && echo \"%{$fg_bold[blue]%}\" || echo \"%{$fg_bold[red]%}\")%#%{$reset_color%} "
      RPROMPT='$(git_prompt_info)'

      ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[cyan]%}"
      ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
      ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*"
    '';
    ohMyZsh.enable = true;
    ohMyZsh.plugins = [ "common-aliases" "git" "git-extras" "history" "jsontools" ];
  };
  programs.bash = {
    promptInit = ''PS1="[\$(exit=\$?; [[ \$exit == 0 ]] && echo \"\[\033[1;32m\]\$exit\" || echo \"\033[1;31m\]\$exit\")$(tput sgr0)]$(tput bold) \w $(tput setaf 1)\$$(tput sgr0) '';
    enableCompletion = true;
  };

  # programs.vim.defaultEditor = true;

  networking.networkmanager.enable = true;

  home-manager.users.kfm = {
    programs.command-not-found.enable = true;

    gtk = {
      enable = true;
      font = with import ./theme.nix; { package = pkgs.roboto; name = uiFont.name; };
      iconTheme = theme.icon;
      theme = theme.gtk;
    };

    programs.git = {
      enable = true;
      userName = fullName;
      userEmail = fullEmail;
      aliases = {
        br = "branch";
        co = "checkout";
        ci = "commit";
        amend = "commit --amend";
        st = "status";
        unstage = "reset HEAD --";
        sdiff = "diff --staged";
        last = "log -1 HEAD";
      };
      ignores = [ "*~" ".stack-work/" "__pycache__/" ".mypy_cache/" "*.o" "*.hi" "*.aux" ];
    };

    programs.vim = {
      enable = true;
      plugins = [
        "ctrlp"
        "deoplete-rust"
        "deoplete-nvim"
        "idris-vim"
        "latex-box"
        "rust-vim"
        "supertab"
        "syntastic"
        "tabular"
        "typescript-vim"
        "vim-airline"
        "vim-airline-themes"
        "vim-commentary"
        "vim-eunuch"
        "vim-fugitive"
        "vim-gitgutter"
        "vim-javascript"
        "vim-nix"
        "vim-pandoc"
        "vim-pandoc-after"
        "vim-pandoc-syntax"
        "vim-repeat"
        "vim-sensible"
        "vim-startify"
        "vim-surround"
      ];
      settings = {
        smartcase = true;
        shiftwidth = 4;
        tabstop = 4;
        expandtab = true;
        number = true;
      };
      extraConfig = import ./dot/vim.nix;
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
          geometry = "300x5-30+20";
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
          foreground = light;
          timeout = 5;
        };
        urgency_normal = {
          frame_color = veryDark;
          background = light;
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
      ".ghci".text = import ./dot/ghci.nix { inherit pkgs; };
      ".tmux.conf".text = import ./dot/tmux.nix;
      ".stack/config.yaml".text = import ./dot/stack.nix {
        githubUser = "kmein";
        inherit fullName fullEmail;
      };
      ".config/zathura/zathurarc".text = "set selection-clipboard clipboard";
      ".config/mpv/input.conf".text = import ./dot/mpv.nix;
      ".config/xfce4/terminal/terminalrc".text = import ./dot/terminal.nix { inherit defaultApplications; };
      ".background-image".source = wallpaper;
    };
  };
}
