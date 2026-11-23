{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    {
      boot.supportedFilesystems = [ "ntfs" ];
    }
    { nixpkgs.config.allowUnfree = true; }
    {
      boot.tmp.cleanOnBoot = true;
      boot.loader.timeout = 1;
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
        description = pkgs.lib.niveum.kieran.name;
        isNormalUser = true;
        uid = 1000;
        extraGroups = [
          "pipewire"
          "audio"
          "lp"
          "scanner"
        ];
      };

      home-manager.users.me.xdg.enable = true;
    }
    {
      environment.interactiveShellInit = "export PATH=$PATH";
      environment.shellAliases = {
        sxiv = "${pkgs.nsxiv}/bin/nsxiv";
        yt = "${pkgs.yt-dlp}/bin/yt-dlp --add-metadata -ic"; # Download video link
        yta = "${pkgs.yt-dlp}/bin/yt-dlp --add-metadata --audio-format mp3 --audio-quality 0 -xic"; # Download with audio
      };
    }
    {
      i18n.defaultLocale = "en_DK.UTF-8";
    }
    {
      services.power-profiles-daemon.enable = true;
    }
    {
      programs.gnupg = {
        agent = {
          enable = true;
          pinentryPackage = pkgs.pinentry-qt;
          settings =
            let
              defaultCacheTtl = 2 * 60 * 60;
            in
            {
              default-cache-ttl = defaultCacheTtl;
              max-cache-ttl = 4 * defaultCacheTtl;
            };
        };
      };

      # always encrypt to myself too, so sent encrypted mail stays readable
      environment.etc."gnupg/gpg.conf".text = ''
        encrypt-to ${pkgs.lib.niveum.kieran.pgpFingerprint}
      '';

      environment.systemPackages = [
        pkgs.gnupg
        pkgs.passage
      ];
    }
    {
      services.getty = {
        greetingLine = lib.mkForce "ΑΓΕΩΜΕΤΡHΤΟΣ ΜHΔΕΙΣ ΕΙΣΙΤΩ";
        helpLine = lib.mkForce "";
      };
    }
    {
      networking.hosts = lib.mapAttrs' (name: address: {
        name = address;
        value = [ "${name}.local" ];
      }) pkgs.lib.niveum.localAddresses;
    }
    {
      home-manager.users.me.home.stateVersion = "22.05";
      home-manager.backupFileExtension = "bak";
    }
    {
      programs.dconf.enable = true;
      home-manager.users.me = {
        dconf.enable = true;
        dconf.settings = {
          # Change the default terminal for Nemo
          "org/cinnamon/desktop/applications/terminal".exec = lib.getExe config.niphas.terminal.package;

          # Pin simple-scan's page size to A4 (tenths of a mm). Its "automatic"
          # setting means "ask the device for its maximum", and the OfficeJet's
          # ADF advertises 355.6mm (Legal) with no end-of-page detection over
          # eSCL, so every A4 sheet came out with 58mm of smeared rollers below
          # it.
          "org/gnome/SimpleScan" = {
            paper-width = 2100;
            paper-height = 2970;
          };
        };
      };
    }
    { services.upower.enable = true; }
    {
      home-manager.users.me.home.file.".zshrc".text = ''
        # nothing to see here
      '';
    }
    ./stylix.nix
    ./alacritty.nix
    ./bluetooth.nix
    ./browser.nix
    ./clipboard.nix
    ./fonts.nix
    ./htop.nix
    ./lix.nix
    ./graphical
    ./keyboard
    ./mpv.nix
    ./mime.nix
    ./flameshot.nix
    ./rofi.nix
    ./ssh.nix
    ./sound.nix
    ./sudo.nix
  ];
}
