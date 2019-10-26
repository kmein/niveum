{ pkgs, lib, config, ... }:
let
  inherit (lib.strings) makeBinPath;
in
{
  imports = [
    <niveum/modules/constants.nix>
    {
      services.dbus.packages = [ pkgs.gnome3.dconf ];
    }
    <home-manager/nixos>
    # ./mopidy.nix
    ./alacritty.nix
    ./bash.nix
    ./bluetooth.nix
    ./chromium.nix
    ./cloud.nix
    ./compton.nix
    ./default.nix
    ./direnv.nix
    # ./home-assistant.nix
    ./distrobump.nix
    ./docker.nix
    ./dunst.nix
    ./fonts.nix
    ./git.nix
    ./hledger.nix
    ./htop.nix
    ./hu-berlin.nix
    ./i3.nix
    ./kdeconnect.nix
    ./keybase.nix
    ./keyboard.nix
    ./mail.nix
    ./mpv.nix
    ./nano.nix
    ./neovim.nix
    # ./newsboat.nix (broken)
    ./nixpkgs-unstable.nix
    ./packages
    ./printing.nix
    ./random-background.nix
    ./redshift.nix
    ./retiolum.nix
    ./rofi.nix
    ./sncli.nix
    ./ssh.nix
    ./sudo.nix
    ./themes/mac-os.nix
    ./theming.nix
    ./tmux.nix
    ./todo-txt.nix
    ./traadfri.nix
    ./unclutter.nix
    # ./urxvt.nix
    ./vscode.nix
    ./xautolock.nix
    # ./xresources.nix
    ./zsh.nix
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
      nix.nixPath = [ "/var/src" ];
    }
    {
      nixpkgs.config = {
        allowUnfree = true;
        packageOverrides = pkgs: {
          python3Packages = pkgs.python3Packages.override {
            overrides = new: old: {
              spotify-cli-linux = new.callPackage <niveum/packages/spotify-cli-linux.nix> {};
              instaloader = new.callPackage <niveum/packages/instaloader.nix> {};
              sncli = new.callPackage <niveum/packages/sncli.nix> {};
            };
          };
          haskellPackages = pkgs.haskellPackages.override {
            overrides = new: old: {
              blessings = new.callPackage <niveum/packages/blessings.nix> {};
              scanner = new.callPackage <stockholm/krebs/5pkgs/haskell/scanner.nix> {};
            };
          };

          git-quick-stats = pkgs.callPackage <niveum/packages/git-quick-stats.nix> {};
          writeDash = pkgs.writers.writeDash;
          writeDashBin = pkgs.writers.writeDashBin;
          iolanguage = pkgs.callPackage <niveum/packages/iolanguage.nix> {};
          nix-git = pkgs.callPackage <niveum/packages/nix-git.nix> {};
          gfs-fonts = pkgs.callPackage <niveum/packages/gfs-fonts.nix> {};

          kmein = {
            autorenkalender = pkgs.callPackage <niveum/packages/autorenkalender.nix> {};
            bvg = pkgs.callPackage <niveum/packages/bvg.nix> {};
            daybook = pkgs.callPackage <niveum/packages/daybook.nix> {};
            genius = pkgs.callPackage <niveum/packages/genius.nix> {};
            instaget = pkgs.callPackage <niveum/packages/instaget.nix> {};
            literature-quote = pkgs.callPackage <niveum/packages/literature-quote.nix> {};
            n = pkgs.callPackage <niveum/packages/n.nix> {};
            depp = pkgs.callPackage <niveum/packages/depp.nix> {};
            odyssey = pkgs.callPackage <niveum/packages/odyssey.nix> {};
            wttr = pkgs.callPackage <niveum/packages/wttr.nix> {};
            nav = pkgs.callPackage <niveum/packages/nav.nix> {};
            dirmir = pkgs.callPackage <niveum/packages/dirmir.nix> {};
            favicon = pkgs.callPackage <niveum/packages/favicon.nix> {};
            tolino-screensaver = pkgs.callPackage <niveum/packages/tolino-screensaver.nix> {};
            # fzf-wrappers = pkgs.callPackage <niveum/packages/fzf-wrappers.nix> {}; (broken)
            slide =
              let slide-package = pkgs.fetchFromGitHub {
                owner = "kmein";
                repo = "slide";
                rev = "0470583d22212745eab4f46076267addf4d2346c";
                sha256 = "0skcp3va9v4hmxy5ramghpz53gnyxv10wsacgmc2jr0v1wrqlzbh";
              };
              in pkgs.callPackage slide-package {};
            haskellPackages.mnemosyne =
              let mnemosyne-package = pkgs.fetchFromGitHub {
                repo = "mnemosyne";
                owner = "kmein";
                rev = "6bfa13c88db176af80be90840ff03573d67d679f";
                sha256 = "1rimv5c5q9602y501hbkgkfbimqnmdkcr5hp1434q06gcazhjhca";
              };
              in pkgs.haskellPackages.callPackage mnemosyne-package {};
          };
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
      location = {
        latitude = 52.517;
        longitude = 13.3872;
      };
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

      users.defaultUserShell = pkgs.zsh;

      users.users.me = {
        name = "kfm";
        description = config.niveum.user.name;
        hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
        isNormalUser = true;
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
      environment.systemPackages =
      let niveumPath = "${config.users.users.me.home}/prog/git/niveum";
      in [
        (pkgs.writers.writeDashBin "niveum-deploy" ''
          [ $# -eq 1 ] || echo >&2 "Usage: niveum-deploy SYSTEM"

          eval $(${pkgs.nix}/bin/nix-build --no-out-link "${niveumPath}/deploy.nix" -A "$1")
        '')
        (pkgs.writers.writeDashBin "niveum-update" ''
          ${pkgs.nix-prefetch-git}/bin/nix-prefetch-git \
            --url https://github.com/NixOS/nixpkgs-channels \
            --rev refs/heads/nixos-${config.system.stateVersion} \
            > "${niveumPath}/nixpkgs.json"
        '')
      ];
    }
    {
      environment.interactiveShellInit = "export PATH=$PATH:$HOME/.cargo/bin";
      environment.shellAliases =
      let
        wcd = pkgs.writers.writeDash "wcd" ''
          cd "$(readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname)/.."
        '';
        where = pkgs.writers.writeDash "where" ''
          readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname
        '';
        take = pkgs.writers.writeDash "take" ''
          mkdir "$1" && cd "$1"
        '';
      in {
        "ix.io" = "${pkgs.curl}/bin/curl -F 'f:1=<-' ix.io";
        cat = "${pkgs.bat}/bin/bat --style=plain";
        chromium-incognito = "chromium --user-data-dir=$(mktemp -d /tmp/chr.XXXXXX) --no-first-run --incognito";
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard -in";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -out";
        cp = "cp -i";
        dig = "dig +short";
        ip = "${pkgs.iproute}/bin/ip -c";
        l = "${pkgs.exa}/bin/exa -s type -a";
        la = "${pkgs.exa}/bin/exa -s type -la";
        ll = "${pkgs.exa}/bin/exa -s type -l";
        ls = "${pkgs.exa}/bin/exa -s type";
        mv = "mv -i";
        nixi = "nix repl '<nixos/nixpkgs>'";
        ns = "nix-shell --run zsh";
        o = "${pkgs.xdg_utils}/bin/xdg-open";
        rm = "rm -i";
        take = "source ${take}";
        tmux = "${pkgs.tmux}/bin/tmux -2";
        tree = "${pkgs.exa}/bin/exa --tree";
        wcd = "source ${wcd}";
        weechat = "${pkgs.openssh}/bin/ssh kmein@prism.r -t tmux attach";
        where = "source ${where}";
      };
    }
    {
      networking.wireless = {
        enable = true;
        userControlled.enable = true;
        networks = {
          "Aether" = {
            pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
            priority = 10;
          };
          "Asoziales Netzwerk" = {
            pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6";
            priority = 10;
          };
          "Libertarian WiFi" = {
            pskRaw = "e9beaae6ffa55d10e80b8a2e7d997411d676a3cc6f1f29d0b080391f04555050";
            priority = 9;
          };
          "EasyBox-927376".pskRaw = "dbd490ab69b39bd67cfa06daf70fc3ef3ee90f482972a668ed758f90f5577c22";
          "FlixBus Wi-Fi" = {};
          "FlixBus" = {};
          "FlixTrain" = {};
          "BVG Wi-Fi" = {};
          "Ni/Schukajlow".pskRaw = "ffc47f6829da59c48aea878a32252223303f5c47a3859edc90971ffc63346781";
          "WIFIonICE" = {};
          "WLAN-914742".psk = "67647139648174545446";
          "WLAN-XVMU6T".pskRaw = "46ea807283255a3d7029233bd79c18837df582666c007c86a8d591f65fae17cc";
          "c-base-public" = {};
          "discord".psk = "baraustrinken";
          "security-by-obscurity".psk = "44629828256481964386";
          "Mayflower".psk = "Fr31EsLan";
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
        displayManager.lightdm = {
          enable = true;
          autoLogin = {
            enable = true;
            user = config.users.users.me.name;
          };
          greeters.gtk = {
            enable = true;
            indicators = [ "~spacer" "~host" "~spacer" "~session" "~power" ];
          };
        };
        desktopManager.default = "none";
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
      systemd.services.restart-vpn = {
        description = "Restart VPNs after suspend";
        wantedBy = [ "suspend.target" ];
        after = [ "suspend.target" ];
        serviceConfig.Type = "oneshot";
        script = ''
          set -efu

          export PATH=${makeBinPath [ pkgs.procps ]}

          pkill -HUP --exact openvpn
          pkill -ALRM --exact tincd
        '';
      };
    }
  ];
}
