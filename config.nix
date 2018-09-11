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
  imports = [ "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos" ];
  nixpkgs.config.allowUnfree = true;

  security.sudo.enable = true;

  fonts.fonts = with pkgs; [ powerline-fonts roboto font-awesome-ttf fira-code eb-garamond lmodern ];
  environment.systemPackages = [ theme.icon.package theme.gtk.package] ++ (with pkgs; [
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

    ffmpeg mpv youtubeDL
    inkscape imagemagick
    zathura calibre
    spotify gnome3.gnome-music audacity

    par haskellPackages.pandoc biber

    google-chrome firefox lynx w3m firefoxPackages.tor-browser

    lxappearance
    xfce.terminal
    xorg.xbacklight pamixer
    gnome3.nautilus
    kdeconnect
    git
    ripgrep tree
    whois
    wget htop zip unzip tmux
    texlive.combined.scheme-minimal
    rlwrap
    libreoffice-fresh
    pmount
    gnumake
    franz
    grive2
    geogebra gnuplot maxima
  ]);

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

  # networking.hostname = "scardanelli";
  services.xserver = {
    enable = true;
    layout = "de, gr, ru";
    xkbVariant = "T3, polytonic, phonetic_winkeys";
    xkbOptions = "terminate:ctrl_alt_bksp, grp:alt_space_toggle";
    libinput.enable = true;
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
    windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        configFile = pkgs.writeText "i3.conf" (import ./dot/i3.nix {
          inherit pkgs defaultApplications wallpaper;
        });
      };
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

      ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}"
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

  programs.vim.defaultEditor = true;

  users.users.kfm = {
    createHome = true;
    description = fullName;
    extraGroups = [ "wheel" "networkmanager" ];
    group = "users";
    home = "/home/kfm";
    shell = pkgs.zsh;
    password = "kfm";
  };

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
      font = "${uiFont.name} ${toString uiFont.size}";
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
      iconTheme = { package = theme.icon.package; name = theme.icon.name; size = "64x64"; };
      settings.global = {
        transparency = 20;
        font = "${uiFont.name} ${toString uiFont.size}";
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
    };
  };

}
