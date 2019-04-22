{ pkgs, lib, config, ... }:
{
  imports = [
    <modules/constants.nix>
    {
      services.dbus.packages = [ pkgs.gnome3.dconf ];
    }
    <home-manager/nixos>
    <configs/sudo.nix>
    <configs/chromium.nix>
    <configs/fonts.nix>
    <configs/sncli.nix>
    <configs/bash.nix>
    <configs/cloud.nix>
    <configs/compton.nix>
    <configs/random-background.nix>
    <configs/docker.nix>
    <configs/vscode.nix>
    <configs/htop.nix>
    <configs/dunst.nix>
    # <configs/mopidy.nix>
    <configs/nixpkgs-unstable.nix>
    <configs/mail.nix>
    <configs/default.nix>
    <configs/python.nix>
    <configs/haskell>
    <configs/hu-berlin.nix>
    <configs/i3.nix>
    <configs/mpv.nix>
    <configs/kdeconnect.nix>
    <configs/git.nix>
    <configs/keyboard.nix>
    <configs/nano.nix>
    <configs/neovim.nix>
    <configs/printing.nix>
    <configs/redshift.nix>
    <configs/retiolum.nix>
    <configs/rofi.nix>
    <configs/ssh.nix>
    <configs/unclutter.nix>
    <configs/urxvt.nix>
    <configs/todoist.nix>
    <configs/xautolock.nix>
    <configs/xresources.nix>
    <configs/hledger.nix>
    <configs/zsh.nix>
    <configs/bluetooth.nix>
    <configs/theming.nix>
    <configs/distrobump.nix>
    <configs/tmux.nix>
    <configs/themes/owickstrom-dark.nix>
    {
      niveum.user = {
        github = "kmein";
        email = "kieran.meinhardt@gmail.com";
        name = "Kierán Meinhardt";
      };

      niveum.applications = rec {
        fileManager = "${config.niveum.applications.terminal} -e ${pkgs.ranger}/bin/ranger";
      };

      niveum.theme = {
        gtk = { name = "Arc"; package = pkgs.arc-theme; };
        icon = { name = "Arc"; package = pkgs.arc-icon-theme; };
        cursor = { name = "capitaine-cursors"; package = pkgs.capitaine-cursors; };
      };
    }
    {
      nixpkgs.config = {
        allowUnfree = true;
        packageOverrides = pkgs: {
          autorenkalender = pkgs.callPackage <packages/autorenkalender.nix> {};
          bvg = pkgs.callPackage <packages/bvg.nix> {};
          daybook = pkgs.callPackage <packages/daybook.nix> {};
          font-size = pkgs.callPackage <packages/font-size.nix> { font = config.niveum.fonts.terminal; };
          genius = pkgs.callPackage <packages/genius.nix> {};
          instaget = pkgs.callPackage <packages/instaget.nix> {};
          instaloader = pkgs.python3Packages.callPackage <packages/instaloader.nix> {};
          iolanguage = pkgs.callPackage <packages/iolanguage.nix> {};
          literature-quote = pkgs.callPackage <packages/literature-quote.nix> {};
          nix-git = pkgs.callPackage <packages/nix-git.nix> {};
          sncli = pkgs.python3Packages.callPackage <packages/sncli.nix> {};
          spotify-cli-linux = pkgs.python3Packages.callPackage <packages/spotify-cli-linux.nix> {};
          wttr = pkgs.callPackage <packages/wttr.nix> {};
          n = pkgs.callPackage <packages/n.nix> {};

          dic = pkgs.callPackage <stockholm/krebs/5pkgs/simple/dic> {};
          yt-next = pkgs.callPackage <stockholm/lass/5pkgs/yt-next> {};
          acronym = pkgs.callPackage <stockholm/lass/5pkgs/acronym> {};
          urban = pkgs.callPackage <stockholm/lass/5pkgs/urban> {};
          mpv-poll = pkgs.callPackage <stockholm/lass/5pkgs/mpv-poll> {};
        };
      };
    }
    {
      boot.cleanTmpDir = true;
      boot.loader.timeout = 1;
      boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
    }
    {
      time.timeZone = "Europe/Berlin";
    }
    {
      home-manager.users.me = {
        programs.zathura = {
          enable = true;
          options.selection-clipboard = "clipboard";
        };
      };
    }
    {
      users.mutableUsers = false;

      users.users.me = {
        name = "kfm";
        description = "Kierán Meinhardt";
        home = "/home/kfm";
        createHome = true;
        group = "users";
        hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
        shell = pkgs.zsh;
      };
    }
    {
      sound.enable = true;

      hardware.pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull; # for bluetooth sound output
      };

      users.users.me.extraGroups = [ "audio" ];

      environment.systemPackages = [ pkgs.pavucontrol pkgs.pamixer ];
    }
    {
      environment.systemPackages = [
        (pkgs.unstable.writers.writeDashBin "niveum-deploy" ''
          NIVEUM_DIR=/home/kfm/prog/git/niveum

          for system in "$@"; do
            eval $(nix-build --no-out-link "$NIVEUM_DIR/deploy.nix" -A "$system") &
          done

          wait
        '')
        (pkgs.unstable.writers.writeDashBin "niveum-update" ''
          NIVEUM_DIR=/home/kfm/prog/git/niveum

          nix-prefetch-git --url https://github.com/NixOS/nixpkgs-channels --rev refs/heads/nixos-18.09 > "$NIVEUM_DIR/nixpkgs.json"
        '')
      ];
    }
    {
      environment.interactiveShellInit = "export PATH=$PATH:$HOME/.local/bin:$HOME/.cargo/bin";
      environment.shellAliases = {
        clipboard = "${pkgs.xclip}/bin/xclip -se c";
        o = "${pkgs.xdg_utils}/bin/xdg-open";
        tmux = "${pkgs.tmux}/bin/tmux -2";
        ip = "${pkgs.iproute}/bin/ip -c";
        ns = "nix-shell --run zsh";
        nixi = ''nix repl "<nixpkgs>"'';
        rm = "rm -i";
        cp = "cp -i";
        mv = "mv -i";
        l = "${pkgs.exa}/bin/exa -a";
        ls = "${pkgs.exa}/bin/exa";
        ll = "${pkgs.exa}/bin/exa -l";
        la = "${pkgs.exa}/bin/exa -la";
      };
    }
    {
      networking.wireless = {
        enable = true;
        userControlled.enable = true;
        networks = {
          Aether.pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
          EasyBox-927376.pskRaw = "dbd490ab69b39bd67cfa06daf70fc3ef3ee90f482972a668ed758f90f5577c22";
          "Asoziales Netzwerk".pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6";
          c-base-public = {};
          security-by-obscurity.psk = "44629828256481964386";
          discord.psk = "baraustrinken";
        };
      };

      environment.systemPackages = [ pkgs.wpa_supplicant_gui ];
    }
    {
      networking.hosts = {
        "192.168.178.1" = [ "fritz.box" ];
        "192.168.178.21" = [ "scardanelli" ];
        "192.168.178.22" = [ "homeros" ];
        "192.168.178.24" = [ "catullus" ];
      };
    }
    {
      i18n.defaultLocale = "en_GB.UTF-8";
    }
    {
      services.illum.enable = true;
    }
    {
      services.xserver = {
        enable = true;
        displayManager.lightdm.greeters.gtk = {
          enable = true;
          indicators = [ "~spacer" "~host" "~spacer" "~session" "~power" ];
        };
        desktopManager.xterm.enable = false;
      };
    }
    {
      security.wrappers = {
        pmount.source = "${pkgs.pmount}/bin/pmount";
        pumount.source = "${pkgs.pmount}/bin/pumount";
      };
    }
    {
      programs.command-not-found.enable = true;
    }
    {
      programs.java = {
        enable = true;
        package = pkgs.openjdk;
      };
    }
    {
      environment.systemPackages = with pkgs; [
      ] ++ [ # office
        libreoffice
      ] ++ [ # internet
        aria2
        firefox
        tor-browser-bundle-bin
        thunderbird
        tdesktop
        w3m
        wget
        httpie
        whois
        ddgr
        instaloader
      ] ++ [ # media
        ffmpeg
        imagemagick
        sxiv
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
        du-dust
        fd
        file
        jq
        manpages
        moreutils
        posix_man_pages
        ranger
        ripgrep
        rlwrap
        tree
      ] ++ [ # hardware
        usbutils
        pciutils
      ] ++ [ # graphical
        arandr
        libnotify
        xclip
        xorg.xkill
      ] ++ [ # typesetting
        (texlive.combine {
          inherit (pkgs.texlive) scheme-full texdoc latex2e-help-texinfo;
          pkgFilter = pkg: pkg.tlType == "run" || pkg.tlType == "bin" || pkg.pname == "latex2e-help-texinfo";
        })
        pandoc
        haskellPackages.pandoc-citeproc
        # haskellPackages.patat
        asciidoctor
        proselint
      ] ++ [ # programming
        tokei
        gnumake
        gcc
        binutils-unwrapped
        htmlTidy
        iolanguage
        nix-prefetch-git
        nodePackages.csslint
        nodePackages.prettier
        nodePackages.jsonlint
        ruby
        rubocop
        rustup
        shellcheck
      ] ++ [ # media
        audacity
        calibre
        inkscape
        xpdf
        pdfgrep
        pdftk
        spotify
        spotify-cli-linux
        youtubeDL
      ] ++ [ # math
        bc
      ] ++ [ # shell
        (aspellWithDicts (dict: [dict.de dict.en dict.la dict.en-computers dict.ru]))
        bvg
        autorenkalender
        literature-quote
        dic
        yt-next
        acronym
        urban
        daybook
        gnupg
        jo
        memo
        par
        fzf
        (pass.withExtensions (ext: [ext.pass-otp]))
        qrencode
        tmuxp
        unstable.zola
        unstable.hugo
        wordnet
        xsv
        wttr
        instaget
        genius
        nix-git
        n
      ];
    }
  ];
}
