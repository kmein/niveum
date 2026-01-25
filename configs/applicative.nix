{
  config,
  pkgs,
  lib,
  ...
}:
{
  users.users.applicative = {
    name = "asg";
    description = "Applicative Systems";
    hashedPasswordFile = config.age.secrets.kfm-password.path;
    home = "/home/applicative";
    uid = 1001;
    isNormalUser = true;
    extraGroups = [
      "pipewire"
      "audio"
    ];
  };

  services.displayManager.autoLogin.enable = false;

  # if we have multiple users, they should be able to log in through a greeter
  programs.regreet =
    let
      wallpaper =
        pkgs.runCommand "textured-monochrome-wallpaper.png"
          {
            buildInputs = [ pkgs.imagemagick ];
          }
          ''
            magick -size 2560x1440 plasma:fractal \
              -colorspace Gray \
              -normalize \
              -fill ${lib.escapeShellArg config.lib.stylix.colors.withHashtag.base00} -colorize 100% \
              -attenuate 0.15 +noise Gaussian \
              $out
          '';
    in
    {
      enable = true;
      settings = {
        background = {
          path = wallpaper;
          fit = "Fill";
        };
        appearance.greeting_msg = "स्वागतम्";
        widget.clock.format = "%F %H:%M";
      };
      font = {
        inherit (config.stylix.fonts.sansSerif) name;
        size = config.stylix.fonts.sizes.applications;
      };
      iconTheme = {
        inherit (config.home-manager.users.me.gtk.iconTheme) package name;
      };
    };


  # to run nspawn in nix sandbox
  nix.settings = {
    auto-allocate-uids = true;
    system-features = [ "uid-range" ];
    experimental-features = [
      "auto-allocate-uids"
      "cgroups"
    ];
    extra-sandbox-paths = [ "/dev/net" ]; # needed for tests of VPNs

    trusted-users = [ config.users.users.applicative.name ];
  };

  services.restic.backups.niveum = {
    extraBackupArgs = [
      "--exclude=${config.users.users.applicative.home}/src/nixpkgs/.git"
    ];
    paths = [
      config.users.users.applicative.home
    ];
  };

  security.sudo.extraRules = [
    {
      # still required for systemd-nspawn
      users = [ config.users.users.applicative.name ];
      commands = [ "ALL" ];
    }
  ];

  home-manager.users.applicative =
    let
      mainGitConfig = (import ./git.nix {
        inherit pkgs lib config;
      }).home-manager.users.me.programs.git;
      mainAlacrittyConfig = (import ./alacritty.nix {
        inherit pkgs lib config;
      }).home-manager.users.me.programs.alacritty;
    in
    lib.recursiveUpdate
      (import ./graphical/home-manager.nix {
        inherit lib pkgs config;
      })
      {
        xdg.enable = true;
        home.stateVersion = "25.11";

        programs.git = {
          enable = true;
          settings = {
            inherit (mainGitConfig.settings) alias;
          };
        };

        programs.alacritty = mainAlacrittyConfig;

        services.hyprpaper =
          let
            # asgWallpaper = pkgs.fetchurl {
            #   url = "http://c.krebsco.de/asg-wallpaper.png";
            #   hash = "sha256-XLp8XcUgFgDKW5Qae0//0Xw9lhribcevQTVSQvClEB4=";
            # };

            backgroundColor = config.lib.stylix.colors.withHashtag.base06;
            foregroundColor = config.lib.stylix.colors.withHashtag.base01;
            width = 1920;
            height = 1080;

            svgUrl = "https://applicative.systems/_astro/logo-full.D8zRvqBZ.svg";
            logoSvg = pkgs.fetchurl {
              url = svgUrl;
              hash = "sha256-qXDIEZsAPn4eUJ3kb5U6L3PMUCtWGYqhqyIaBt7FntE=";
            };

            asgWallpaper =
              pkgs.runCommand "applicative-wallpaper.png"
                {
                  nativeBuildInputs = [ pkgs.imagemagick ];
                }
                ''
                  # 1. We use -background to set the canvas color
                  # 2. We use -fuzz and -opaque to replace the logo's internal colors
                  # 3. We use -gravity and -extent to center it on a wallpaper-sized canvas

                  convert \
                    -background none \
                    -density 300 \
                    "${logoSvg}" \
                    -fuzz 100% -fill "${foregroundColor}" -opaque black \
                    -resize 800x800 \
                    -gravity center \
                    -background "${backgroundColor}" \
                    -extent ${toString width}x${toString height} \
                    $out
                '';
          in
          {
            enable = true;
            settings = {
              ipc = false;
              splash = true;
              preload = [ "${asgWallpaper}" ];
              wallpaper = [ ",${asgWallpaper}" ];
            };
          };

      };
}
