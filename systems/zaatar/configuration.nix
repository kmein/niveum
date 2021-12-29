{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) retiolumAddresses;
in
{
  imports = [
    ./hardware-configuration.nix
    "${self.inputs.home-manager}/nixos"
    ../configs/wpa_supplicant.nix
    ../configs/keyboard.nix
    ../modules/retiolum.nix
    ../configs/spacetime.nix
    ../configs/nix.nix
    ../configs/tuna.nix
    ../configs/sshd.nix
    ../configs/version.nix
    ../configs/gaslight.nix
    ../configs/printing.nix
    ../configs/traadfri.nix
    ../configs/bvg.nix
    ../configs/moodle-dl/meinhark.nix
    ../configs/monitoring/push.nix
    {
      nixpkgs.config.allowUnfree = true;
    }
    {
      sound.enable = true;

      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
        tcp = {
          enable = true;
          anonymousClients.allowedIpRanges = [ "127.0.0.1" "10.243.2.0/24" "192.168.0.0/16" ];
        };
        zeroconf.publish.enable = true;
      };
      networking.firewall.allowedTCPPorts = [ 4713 ];
    }
    {
      environment.systemPackages =
      let
        worldradio = pkgs.callPackage ../packages/worldradio.nix {};
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
        let startUrls = [ "https://open.spotify.com" "https://youtube.com" "http://bvg.kmein.r" ];
        in pkgs.writers.writeDash "kiosk-browser" ''
          while true; do
            ${pkgs.chromium}/bin/chromium \
              --no-first-run --no-message-box --noerrdialogs \
              --default-browser --no-default-browser-check \
              --start-maximized ${lib.escapeShellArgs startUrls}
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

  environment.systemPackages = with pkgs; [ git vim htop ncmpcpp ];

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zaatar";
  networking.wireless.interfaces = [ "wlp2s0" ];

  networking.retiolum = retiolumAddresses.zaatar;

  system.stateVersion = "20.09";
}
