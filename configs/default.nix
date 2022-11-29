{
  pkgs,
  lib,
  config,
  options,
  ...
}: let
  inherit (lib.strings) makeBinPath;
  inherit (import <niveum/lib>) localAddresses kieran;
  scripts = import <niveum/packages/scripts> {inherit pkgs lib;};
in {
  imports = [
    <home-manager/nixos>
    <niveum/modules/system-dependent.nix>
    {
      boot.supportedFilesystems = ["ntfs"];
    }
    {
      nix.nixPath = [
        "/var/src"
        "nixpkgs-overlays=${toString ../overlays}"
      ];
    }
    {
      nixpkgs = {
        config = {
          allowUnfree = true;
          packageOverrides = pkgs: {
            dmenu = pkgs.writers.writeDashBin "dmenu" ''exec ${pkgs.rofi}/bin/rofi -dmenu "$@"'';
            gfs-fonts = pkgs.callPackage <niveum/packages/gfs-fonts.nix> {};
            tocharian-font = pkgs.callPackage <niveum/packages/tocharian-font.nix> {};
            iolanguage = pkgs.callPackage <niveum/packages/iolanguage.nix> {};
            ix = pkgs.callPackage <niveum/packages/ix.nix> {};
          };
        };
      };
    }
    {
      boot.cleanTmpDir = true;
      boot.loader.timeout = 1;
    }
    {
      home-manager.users.me = {
        programs.zathura = {
          enable = true;
          options = {
            selection-clipboard = "clipboard";
            recolor-keephue = true;
            # first-page-column = "1:1"; # makes side-by-side mode start on the left side
          };
        };
      };
    }
    {
      users.mutableUsers = false;

      users.defaultUserShell = pkgs.zsh;

      users.users.me = {
        name = "kfm";
        description = kieran.name;
        hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
        isNormalUser = true;
        uid = 1000;
      };

      home-manager.users.me.xdg.enable = true;
      home-manager.users.me.dconf.enable = false;
    }
    {
      sound.enable = true;

      hardware.pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull;
        # copy server:/run/pulse/.config/pulse/cookie to client:~/.config/pulse/cookie to authenticate a client machine
        zeroconf.discovery.enable = true;
        extraConfig = ''
          load-module ${
            toString [
              "module-tunnel-sink-new"
              "server=zaatar.r"
              "sink_name=zaatar"
              "channels=2"
              "rate=44100"
            ]
          }
        '';
      };

      users.users.me.extraGroups = ["audio"];

      environment.systemPackages = [pkgs.pavucontrol pkgs.ncpamixer pkgs.pamixer pkgs.pulsemixer];
    }
    {
      environment.interactiveShellInit = "export PATH=$PATH:$HOME/projects/niveum";
      environment.shellAliases = let
        wcd = pkgs.writers.writeDash "wcd" ''
          cd "$(readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname)/.."
        '';
        where = pkgs.writers.writeDash "where" ''
          readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname
        '';
        take = pkgs.writers.writeDash "take" ''
          mkdir "$1" && cd "$1"
        '';
        cdt = pkgs.writers.writeDash "cdt" ''
          cd "$(mktemp -d)"
          pwd
        '';
        swallow = command: "${scripts.swallow}/bin/swallow ${command}";
      in {
        "ß" = "${pkgs.util-linux}/bin/setsid";
        cat = "${pkgs.bat}/bin/bat --style=plain";
        chromium-incognito = "chromium --user-data-dir=$(mktemp -d /tmp/chr.XXXXXX) --no-first-run --incognito";
        cp = "cp --interactive";
        ip = "${pkgs.iproute2}/bin/ip -c";
        l = "ls --color=auto --time-style=long-iso --almost-all";
        ls = "ls --color=auto --time-style=long-iso";
        ll = "ls --color=auto --time-style=long-iso -l";
        la = "ls --color=auto --time-style=long-iso --almost-all -l";
        mv = "mv --interactive";
        nixi = "nix repl '<nixpkgs>'";
        ns = "nix-shell --run zsh";
        o = "${pkgs.xdg-utils}/bin/xdg-open";
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard -in";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -out";
        rm = "rm --interactive";
        s = "${pkgs.systemd}/bin/systemctl";
        take = "source ${take}";
        cdt = "source ${cdt}";
        vit = "$EDITOR $(mktemp)";
        tmux = "${pkgs.tmux}/bin/tmux -2";
        sxiv = swallow "${pkgs.nsxiv}/bin/nsxiv";
        zathura = swallow "${pkgs.zathura}/bin/zathura";
        us = "${pkgs.systemd}/bin/systemctl --user";
        wcd = "source ${wcd}";
        im = "${pkgs.openssh}/bin/ssh weechat@makanek -t tmux attach-session -t IM";
        where = "source ${where}";
        yt = "${pkgs.yt-dlp}/bin/yt-dlp --add-metadata -ic"; # Download video link
        yta = "${pkgs.yt-dlp}/bin/yt-dlp --add-metadata -xic"; # Download with audio
      };
    }
    {
      i18n = {
        defaultLocale = "en_DK.UTF-8";
        supportedLocales = ["all"];
      };
    }
    {
      services.xserver = {
        enable = true;
        displayManager = {
          autoLogin = {
            enable = true;
            user = config.users.users.me.name;
          };
          lightdm = {
            enable = true;
            greeters.gtk = {
              enable = true;
              indicators = ["~spacer" "~host" "~spacer" "~session" "~power"];
            };
          };
        };
      };
    }
    {
      security.wrappers = {
        pmount = {
          setuid = true;
          owner = "root";
          group = "root";
          source = "${pkgs.pmount}/bin/pmount";
        };
        pumount = {
          setuid = true;
          owner = "root";
          group = "root";
          source = "${pkgs.pmount}/bin/pumount";
        };
      };
    }
    {programs.command-not-found.enable = true;}
    {
      programs.gnupg.agent.enable = true;

      environment.systemPackages = [
        pkgs.gnupg
        (pkgs.pass.withExtensions (e: [e.pass-otp e.pass-import e.pass-genphrase]))
      ];
    }
    {
      services.atd.enable = true;
    }
    {
      services.getty = {
        greetingLine = lib.mkForce "";
        helpLine = lib.mkForce "";
      };
    }
    {
      networking.hosts =
        lib.mapAttrs' (name: address: {
          name = address;
          value = ["${name}.local"];
        })
        localAddresses;
    }
    {
      home-manager.users.me.home.stateVersion = "22.05";
    }
    ./alacritty.nix
    ./backup.nix
    ./bash.nix
    ./beets.nix
    ./bluetooth.nix
    ./ccc.nix
    ./khal.nix
    ./chromium.nix
    ./clipboard.nix
    ./cloud.nix
    ./compton.nix
    ./direnv.nix
    ./distrobump.nix
    ./docker.nix
    ./dunst.nix
    ./flix.nix
    ./fonts.nix
    ./fzf.nix
    ./git.nix
    ./hledger.nix
    ./htop.nix
    ./hu-berlin.nix
    ./i3.nix
    ./keyboard.nix
    ./lb.nix
    ./mpv.nix
    ./mime.nix
    ./neovim.nix
    ./neomutt.nix
    ./nix.nix
    ./newsboat.nix
    ./flameshot-once.nix
    ./packages.nix
    ./stardict.nix
    ./polkit.nix
    ./power-action.nix
    ./printing.nix
    # ./openweathermap.nix
    ./wallpaper.nix
    ./redshift.nix
    ./retiolum.nix
    ./rofi.nix
    ./spacetime.nix
    ./seafile.nix
    ./ssh.nix
    ./sshd.nix
    ./sudo.nix
    ./nsxiv.nix
    ./themes.nix
    ./tmux.nix
    # ./traadfri.nix
    ./unclutter.nix
    ./vscode.nix
    ./watson.nix
    ./zsh.nix
    ./tor.nix
  ];
}
