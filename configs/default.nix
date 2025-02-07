{
  pkgs,
  lib,
  config,
  niveumPackages,
  unstablePackages,
  inputs,
  ...
}: let
  inherit (lib.strings) makeBinPath;
  inherit (import ../lib) localAddresses kieran remoteDir;
  defaultApplications = (import ../lib).defaultApplications {inherit pkgs;};
in {
  imports = [
    inputs.self.nixosModules.system-dependent
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
            "zotero-6.0.26"
            "electron-25.9.0"
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
      users.mutableUsers = false;

      users.defaultUserShell = pkgs.zsh;

      users.users.me = {
        name = "kfm";
        description = kieran.name;
        hashedPasswordFile = config.age.secrets.kfm-password.path;
        isNormalUser = true;
        uid = 1000;
        extraGroups = ["pipewire" "audio"];
      };

      nix.settings.trusted-users = [ config.users.users.me.name ];

      age.secrets = {
        kfm-password.file = ../secrets/kfm-password.age;
      };

      home-manager.users.me.xdg.enable = true;
    }
    {
      environment.interactiveShellInit = "export PATH=$PATH";
      environment.shellAliases = let
        swallow = command: "${niveumPackages.swallow}/bin/swallow ${command}";
      in {
        o = "${pkgs.xdg-utils}/bin/xdg-open";
        ns = "nix-shell --run zsh";
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard -in";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -out";
        tmux = "${pkgs.tmux}/bin/tmux -2";
        sxiv = swallow "${pkgs.nsxiv}/bin/nsxiv";
        zathura = swallow "${pkgs.zathura}/bin/zathura";
        im = "${pkgs.openssh}/bin/ssh weechat@makanek -t tmux attach-session -t IM";
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
      services.displayManager = {
        autoLogin = {
          enable = true;
          user = config.users.users.me.name;
        };
      };
      services.xserver = {
        enable = true;
        displayManager.lightdm = {
          enable = true;
          greeters.gtk = {
            enable = true;
            indicators = ["~spacer" "~host" "~spacer" "~session" "~power"];
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
      programs.gnupg = {
        agent = {
          enable = true;
          pinentryPackage = pkgs.pinentry-qt;
          settings = rec {
            default-cache-ttl = 2 * 60 * 60;
            max-cache-ttl = 4 * default-cache-ttl;
          };
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
      home-manager.backupFileExtension = "bak";
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
    ./admin-essentials.nix
    ./stylix.nix
    ./alacritty.nix
    ./backup.nix
    ./bash.nix
    ./bluetooth.nix
    ./aerc.nix
    ./ccc.nix
    ./khal.nix
    ./browser.nix
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
    ./fu-berlin.nix
    ./i3.nix
    ./i3status-rust.nix
    ./keyboard.nix
    ./kdeconnect.nix
    {home-manager.users.me.home.file.".XCompose".source = ../lib/keyboards/XCompose;}
    ./lb.nix
    ./mpv.nix
    ./mime.nix
    ./neovim.nix
    ./nix.nix
    ./newsboat.nix
    ./flameshot.nix
    ./fritzbox.nix
    ./packages.nix
    ./picom.nix
    ./stardict.nix
    ./polkit.nix
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
    ./unclutter.nix
    ./vscode.nix
    ./watson.nix
    ./wallpaper.nix
    ./zsh.nix
    ./tor.nix
    ./stw-berlin.nix
    ./mastodon-bot.nix
    {
      fileSystems."${remoteDir}/fritz" = {
        device = "//192.168.178.1/FRITZ.NAS/Backup";
        fsType = "cifs";
        options = [
          "username=ftpuser"
          "password=ftppassword"
          "noauto"
          "nounix"
          "rw"
          # "noserverino" # ref https://askubuntu.com/a/1265165
          "uid=${toString config.users.users.me.uid}"
          "gid=${toString config.users.groups.users.gid}"
          "x-systemd.automount"
          "x-systemd.device-timeout=1"
          "x-systemd.idle-timeout=1min"
        ];
      };
    }
    {
      home-manager.users.me = {
        xdg.userDirs = rec {
          enable = true;
          documents = "${config.users.users.me.home}/cloud/nextcloud/Documents";
          desktop = "/tmp";
          download = "${config.users.users.me.home}/sync/Downloads";
          music = "${config.users.users.me.home}/mobile/audio";
          pictures = "${config.users.users.me.home}/cloud/nextcloud/Bilder";
          publicShare =  "${config.users.users.me.home}/cloud/nextcloud/tmp";
          videos = pictures;
        };
      };
    }
  ];
}
