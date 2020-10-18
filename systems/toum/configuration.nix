{ config, pkgs, lib, ... }:
let
  kmeinKeys = lib.strings.splitString "\n" (lib.strings.fileContents (pkgs.fetchurl {
    url = "https://github.com/kmein.keys";
    sha256 = "1b9gbpgihg7zc89ivsz0gs3najp0zg53rcknvzvkm0851fdzkryx";
  }));
in {
  imports = [
    ./hardware-configuration.nix
    ./telegram-bots
    <niveum/configs/distrobump.nix>
    <niveum/configs/nixpkgs-unstable.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/tmux.nix>
    <niveum/configs/version.nix>
    <niveum/configs/traadfri.nix>
    <niveum/modules/retiolum.nix>
    <niveum/modules/telegram-bot.nix>
    {
      nixpkgs = {
        overlays = [ (import <niveum/overlays/toml.nix>) ];
        config.packageOverrides = pkgs: {
          writeDash = pkgs.writers.writeDash;
          writeDashBin = pkgs.writers.writeDashBin;
        };
      };

      services.matterbridge = {
        enable = true;
        configPath = toString (pkgs.writeTOML (import <niveum/dot/matterbridge.nix> {
          token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
        }));
      };
    }
    { services.keybase.enable = true; }
    {
      sound.enable = true;
      hardware.pulseaudio.enable = true;

      boot.loader.raspberryPi.firmwareConfig = ''
        dtparam=audio=on
      '';
    }
    { boot.kernelParams = [ "console=ttyS1,115200n8" ]; } # Enable serial console
    {
       imports = [ <stockholm/krebs/3modules/urlwatch.nix> ];

       krebs.urlwatch = {
         enable = true;
         onCalendar = "*-*-* 05:00:00";
         sendmail.enable = false;
         telegram = {
           enable = true;
           chatId = [ "18980945" ];
           botToken = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
         };
         urls = [
           # "https://michael-klonovsky.de/acta-diurna"
         ];
       };
     }
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "toum";

  time.timeZone = "Europe/Berlin";

  networking.wireless = {
    enable = false;
    networks.Aether = {
      pskRaw =
        "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
    };
  };

  environment.variables.TERM = "linux";
  environment.variables.HTOPRC = toString <niveum/dot/htoprc>;

  environment.systemPackages = with pkgs; [
    git vim htop wget reptyr

    raspberrypi-tools
  ];

  users.mutableUsers = false;
  users.users.me = {
    name = "kfm";
    home = "/home/kfm";
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" ];
    hashedPassword =
      "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
    shell = pkgs.bash;
  };

  security.sudo.enable = true;

  services.openssh = {
    enable = true;
    ports = [ 22022 ];
    passwordAuthentication = false;
  };

  users.users.root.openssh.authorizedKeys.keys = kmeinKeys;

  users.users.me.openssh.authorizedKeys.keys = kmeinKeys;

  networking.retiolum = {
    ipv4 = "10.243.2.3";
    ipv6 = "42:0:3c46:3ec0:7aad:d1d5:9842:da4c";
  };

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <system-secrets/retiolum.key>;
    mode = "400";
  };

  system.stateVersion = "20.03";
}
