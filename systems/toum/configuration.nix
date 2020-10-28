{ config, pkgs, lib, ... }:
let
  kmeinKeys = lib.strings.splitString "\n" (lib.strings.fileContents (pkgs.fetchurl {
    url = "https://github.com/kmein.keys";
    sha256 = "1b9gbpgihg7zc89ivsz0gs3najp0zg53rcknvzvkm0851fdzkryx";
  }));
in {
  imports = [
    ./hardware-configuration.nix
    ./hass
    ./telegram-bots
    <niveum/configs/distrobump.nix>
    <niveum/configs/nextcloud.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/wifi.nix>
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
    {
      services.weechat.enable = true;
      programs.screen.screenrc = ''
        multiuser on
        acladd ${config.users.users.me.name}
      '';
    }
  ];

  nix.nixPath = [ "/var/src" ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "toum";

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
    openssh.authorizedKeys.keys = kmeinKeys;
  };

  security.sudo.enable = true;


  networking.retiolum = {
    ipv4 = "10.243.2.3";
    ipv6 = "42:0:3c46:56af:d12b:affd:8390:df22";
  };

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <system-secrets/retiolum.key>;
    mode = "400";
  };

  system.stateVersion = "20.03";
}
