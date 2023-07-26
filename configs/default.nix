{
  pkgs,
  lib,
  config,
  niveumPackages,
  inputs,
  ...
}: let
  inherit (lib.strings) makeBinPath;
  inherit (import ../lib) localAddresses kieran;
  defaultApplications = (import ../lib).defaultApplications {inherit pkgs;};
in {
  imports = [
    inputs.self.nixosModules.system-dependent
    inputs.self.nixosModules.traadfri
    inputs.self.nixosModules.power-action
    {
      boot.supportedFilesystems = ["ntfs"];
    }
    {
      nixpkgs = {
        config = {
          allowUnfree = true;
          packageOverrides = pkgs: {
            dmenu = pkgs.writers.writeDashBin "dmenu" ''exec ${pkgs.rofi}/bin/rofi -dmenu "$@"'';
          };
          permittedInsecurePackages = [
            "qtwebkit-5.212.0-alpha4"
          ];
        };
      };
    }
    {
      boot.tmp.cleanOnBoot = true;
      boot.loader.timeout = 1;
    }
    {
      age.secrets = {
        di-fm-key = {
          file = ../secrets/di-fm-key.age;
          owner = config.users.users.me.name;
          group = config.users.users.me.group;
          mode = "400";
        };
        restic = {
          file = ../secrets/restic.age;
          owner = config.users.users.me.name;
          group = config.users.users.me.group;
          mode = "400";
        };
      };
    }
    {
      home-manager.users.me = {
        programs.zathura = {
          enable = true;
          options = {
            selection-clipboard = "clipboard";
            # first-page-column = "1:1"; # makes side-by-side mode start on the left side
          };
        };
      };
    }
    {
      environment.systemPackages = [
        pkgs.capitaine-cursors
      ];

      home-manager.users.me = {
        home.pointerCursor = {
          name = "capitaine-cursors-white";
          package = pkgs.capitaine-cursors;
          size = 12;
        };
      };
    }
    {
      users.mutableUsers = false;

      users.defaultUserShell = pkgs.zsh;

      users.users.me = {
        name = "kfm";
        description = kieran.name;
        passwordFile = config.age.secrets.kfm-password.path;
        isNormalUser = true;
        uid = 1000;
      };

      age.secrets = {
        kfm-password.file = ../secrets/kfm-password.age;
      };

      home-manager.users.me.xdg.enable = true;
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
        swallow = command: "${niveumPackages.swallow}/bin/swallow ${command}";
      in {
        "ß" = "${pkgs.util-linux}/bin/setsid";
        cat = "${pkgs.bat}/bin/bat --theme=ansi --style=plain";
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
      home-manager.users.me = {
        services.gpg-agent = rec {
          enable = true;
          enableZshIntegration = true;
          defaultCacheTtl = 2 * 60 * 60;
          maxCacheTtl = 4 * defaultCacheTtl;
        };
      };

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
    {
      systemd.user.services.udiskie = {
        after = ["udisks2.service"];
        wants = ["udisks2.service"];
        wantedBy = ["graphical-session.target"];
        serviceConfig = {
          ExecStart = "${pkgs.udiskie}/bin/udiskie --verbose --no-config --notify";
        };
      };
      services.udisks2.enable = true;
      programs.dconf.enable = true;
      home-manager.users.me = {
        dconf.enable = true;
        dconf.settings = {
          # Change the default terminal for Nemo
          "org/cinnamon/desktop/applications/terminal".exec = defaultApplications.terminal;
        };
      };
    }
    ./android.nix
    ./stylix.nix
    ./alacritty.nix
    ./backup.nix
    ./bash.nix
    ./bluetooth.nix
    ./aerc.nix
    ./ccc.nix
    ./khal.nix
    ./chromium.nix
    ./clipboard.nix
    ./cloud.nix
    ./direnv.nix
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
    ./i3status-rust.nix
    ./keyboard.nix
    ./lb.nix
    ./mpv.nix
    ./mime.nix
    ./neovim.nix
    ./nix.nix
    ./newsboat.nix
    ./flameshot.nix
    ./packages.nix
    ./picom.nix
    ./stardict.nix
    ./polkit.nix
    ./power-action.nix
    ./printing.nix
    ./redshift.nix
    ./retiolum.nix
    ./rofi.nix
    ./spacetime.nix
    ./ssh.nix
    ./sshd.nix
    ./sound.nix
    ./sudo.nix
    ./tmux.nix
    ./traadfri.nix
    ./unclutter.nix
    ./vscode.nix
    ./watson.nix
    ./zsh.nix
    ./tor.nix
    ./mastodon-bot.nix
  ];
}
