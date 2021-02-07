{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    <home-manager/nixos>
    <niveum/configs/wifi.nix>
    <niveum/configs/keyboard.nix>
    <niveum/modules/retiolum.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/mpd.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/version.nix>
    <niveum/configs/spotifyd.nix>
    <niveum/configs/mail/fetcher.nix>
    {
      sound.enable = true;
    }
    {
      environment.systemPackages =
      let
        worldradio = pkgs.callPackage <niveum/packages/worldradio.nix> {};
      in [
        (pkgs.writers.writeDashBin "mpv" ''
          ${pkgs.mpv}/bin/mpv --no-video "$@"
        '')
        (pkgs.writers.writeDashBin "worldradio" ''
          shuf ${worldradio} | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv --no-video
        '')
      ];
    }
    {
      users.extraUsers.kiosk = {
        isNormalUser = true;
        password = "";
        extraGroups = [ "audio" ];
      };
      services.cage = {
        enable = true;
        user = config.users.extraUsers.kiosk.name;
        extraArguments = [ "-s" ]; # allow vt switching
        program =
        let startUrl = "http://localhost:${toString config.services.mpd-fm.webPort}";
        in pkgs.writers.writeDash "kiosk-browser" ''
          while true; do
            ${pkgs.chromium}/bin/chromium \
              --no-first-run --no-message-box --noerrdialogs \
              --default-browser --no-default-browser-check \
              --start-maximized ${startUrl}
            sleep 0.5
          done
        '';
      };
      systemd.services.cage-tty1.environment.XKB_DEFAULT_LAYOUT = "de";
      programs.chromium = {
        enable = true;
        extensions = [
          "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        ];
      };
    }
    {
      environment.systemPackages = [ pkgs.tmux ];
      systemd.services.turntables = {
        description = "music controller session";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.alacritty.terminfo ];
        script = ''
          ${pkgs.tmux}/bin/tmux -2 new-session -d -s turntables ${pkgs.alsaUtils}/bin/alsamixer \; new-window
        '';
        preStop = "${pkgs.tmux}/bin/tmux kill-session -t turntables";
        serviceConfig = {
          User = "root";
          RemainAfterExit = true;
          Type = "oneshot";
        };
      };
    }
  ];

  nix.nixPath = [ "/var/src" ];

  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
  };

  services.illum.enable = true;

  environment.systemPackages = with pkgs; [ git vim htop ];

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zaatar";

  networking.retiolum = {
    ipv4 = "10.243.2.34";
    ipv6 = "42:0:3c46:156e:10b6:3bd6:6e82:b2cd";
  };

  system.stateVersion = "20.09";
}
