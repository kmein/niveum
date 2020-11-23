{ pkgs, lib, config, options, ... }:
let
  inherit (lib.strings) makeBinPath;
  inherit (import <niveum/lib>) localAddresses kieran;
in {
  imports = [
    <home-manager/nixos>
    <niveum/modules/system-dependent.nix>
    {
      boot.supportedFilesystems = [ "ntfs" ];
    }
    {
      nix.nixPath = [
        "/var/src"
        "nixpkgs-overlays=${toString ../overlays}"
      ];
    }
    { services.dbus.packages = [ pkgs.gnome3.dconf ]; }
    {
      nixpkgs = {
        config = {
          allowUnfree = true;
          packageOverrides = pkgs: {
            nur = import (builtins.fetchTarball
              "https://github.com/nix-community/NUR/archive/aea85375c7a82297d977904de8dd7f41baf2d59a.tar.gz") {
                inherit pkgs;
              };
            writeDashBin = pkgs.writers.writeDashBin;
            writeDash = pkgs.writers.writeDash;
            gfs-fonts = pkgs.callPackage <niveum/packages/gfs-fonts.nix> {};
            iolanguage = pkgs.callPackage <niveum/packages/iolanguage.nix> { };
            ix = pkgs.callPackage <niveum/packages/ix.nix> { };
          };
        };
        overlays = [
          (self: super: {
            scripts = import <niveum/packages/scripts> { pkgs = super; lib = super.lib; };
          })
          (import <niveum/overlays/toml.nix>)
          (import <stockholm/krebs/5pkgs/haskell>)
          (import <stockholm/submodules/nix-writers/pkgs>)
          (import <stockholm/krebs/5pkgs/override>)
        ];
      };
    }
    {
      boot.cleanTmpDir = true;
      boot.loader.timeout = 1;
      boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
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
        hashedPassword =
          "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
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

      environment.systemPackages = [ pkgs.pavucontrol pkgs.pamixer pkgs.pulsemixer ];
    }
    {
      environment.interactiveShellInit =
        "export PATH=$PATH:$HOME/projects/niveum";
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
        swallow = command: "${pkgs.scripts.swallow}/bin/swallow ${command}";
      in {
        "ß" = "${pkgs.utillinux}/bin/setsid";
        cat = "${pkgs.bat}/bin/bat --style=plain";
        chromium-incognito =
          "chromium --user-data-dir=$(mktemp -d /tmp/chr.XXXXXX) --no-first-run --incognito";
        cp = "cp --interactive";
        ip = "${pkgs.iproute}/bin/ip -c";
        l  = "ls --color=auto --time-style=long-iso";
        ls = "ls --color=auto --time-style=long-iso --almost-all";
        ll = "ls --color=auto --time-style=long-iso -l";
        la = "ls --color=auto --time-style=long-iso --almost-all -l";
        mv = "mv --interactive";
        nixi = "nix repl '<nixpkgs>'";
        ns = "nix-shell --run zsh";
        o = "${pkgs.xdg_utils}/bin/xdg-open";
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard -in";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -out";
        rm = "rm --interactive";
        s = "${pkgs.systemd}/bin/systemctl";
        take = "source ${take}";
        tmux = "${pkgs.tmux}/bin/tmux -2";
        sxiv = swallow "${pkgs.sxiv}/bin/sxiv";
        zathura = swallow "${pkgs.zathura}/bin/zathura";
        us = "${pkgs.systemd}/bin/systemctl --user";
        wcd = "source ${wcd}";
        weechat = "${pkgs.openssh}/bin/ssh weechat@toum -t screen -x weechat-screen";
        where = "source ${where}";
        yt =
          "${pkgs.youtube-dl}/bin/youtube-dl --add-metadata -ic"; # Download video link
        yta =
          "${pkgs.youtube-dl}/bin/youtube-dl --add-metadata -xic"; # Download with audio
      };
    }
    { i18n.defaultLocale = "en_GB.UTF-8"; }
    { services.illum.enable = true; }
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
              indicators = [ "~spacer" "~host" "~spacer" "~session" "~power" ];
            };
          };
        };
      };
    }
    {
      security.wrappers = {
        pmount.source = "${pkgs.pmount}/bin/pmount";
        pumount.source = "${pkgs.pmount}/bin/pumount";
      };
    }
    { programs.command-not-found.enable = true; }
    {
      programs.gnupg.agent.enable = true;

      environment.systemPackages = [
        pkgs.gnupg
        (pkgs.pass.withExtensions (e: [e.pass-otp]))
      ];
    }
    {
      services.atd.enable = true;
    }
    {
      services.mingetty = {
        greetingLine = lib.mkForce "";
        helpLine = lib.mkForce "";
      };
    }
    {
      networking.hosts = lib.mapAttrs' (name: address: {
        name = address;
        value = [ "${name}.local" ];
      }) localAddresses;
    }
    ./alacritty.nix
    ./bash.nix
    ./bluetooth.nix
    ./ccc.nix
    ./kleiter.nix
    ./calcurse.nix
    ./chromium.nix
    ./cloud.nix
    ./compton.nix
    ./direnv.nix
    ./distrobump.nix
    ./docker.nix
    ./dunst.nix
    ./flix.nix
    ./fonts.nix
    ./fzf.nix
    ./gaslight.nix
    ./git.nix
    ./hledger.nix
    ./htop.nix
    ./hu-berlin.nix
    ./i3.nix
    ./keybase.nix
    ./keyboard.nix
    ./mail.nix
    ./mpv.nix
    ./mime.nix
    ./nano.nix
    ./neovim.nix
    ./newsboat.nix
    ./flameshot-once.nix
    ./packages
    ./printing.nix
    ./wallpaper.nix
    ./redshift.nix
    ./retiolum.nix
    ./rofi.nix
    ./spacetime.nix
    ./ssh.nix
    ./sshd.nix
    ./sudo.nix
    ./sxiv.nix
    ./theming.nix
    ./tmux.nix
    ./tor.nix
    ./todo-txt.nix
    ./traadfri.nix
    ./unclutter.nix
    ./version.nix
    ./vscode.nix
    ./watson.nix
    ./wifi.nix
    ./xautolock.nix
    ./zsh.nix
  ];
}
