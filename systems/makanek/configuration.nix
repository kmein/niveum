{ lib, config, pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran retiolumAddresses;
in
{
  imports = [
    {
      systemd.services.praesenzlehre = {
        description = "Live Ticker zu praesenzlehre-berlin.org";
        wants = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        startAt = "daily";
        path = [ pkgs.curl pkgs.pup pkgs.bc ];
        environment.BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
        script = ''
          set -efu

          count_file=/tmp/praesenzlehre-berlin.org

          set_count() {
            echo $* > "$count_file"
          }

          get_count() {
            cat "$count_file"
          }

          notify() {
            curl -sSL -X POST -H 'Content-Type: application/json' \
               -d "{\"chat_id\": \"@praesenzlehre_berlin\", \"text\": \"$*\"}" \
               "https://api.telegram.org/bot$BOT_TOKEN/sendMessage"
          }

          test -f "$count_file" || set_count 0

          count="$(get_count)"

          new_count="$(curl -sSL https://praesenzlehre-berlin.org/ | pup '.dk-speakout-signature-count span text{}' | tr -dc 0-9)"

          if [ "$new_count" -gt "$count" ]; then
            diff="$(echo "$new_count - $count" | bc)"
            echo "$new_count (+ $diff)"
            notify "$new_count Unterzeichner:innen! (+ $diff)"
            set_count "$new_count"
          fi
        '';
      };
    }
    ./hardware-configuration.nix
    <niveum/configs/hedgedoc.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/nextcloud.nix>
    <niveum/configs/moodle-dl/borsfaye.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/monitoring/pull.nix>
    <niveum/configs/monitoring/push.nix>
    <niveum/configs/version.nix>
    <niveum/configs/radio>
    <niveum/configs/gitea.nix>
    <niveum/configs/names.nix>
    <niveum/configs/menstruation.nix>
    <niveum/configs/telegram-bots>
    <niveum/configs/weechat.nix>
    <niveum/configs/urlwatch.nix>
    <niveum/configs/matterbridge.nix>
    <niveum/modules/retiolum.nix>
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      writeDashBin = pkgs.writers.writeDashBin;
      writeDash = pkgs.writers.writeDash;
    };
  };

  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  networking.hostName = "makanek";

  system.stateVersion = "20.03";

  boot.loader.grub.devices = [ "/dev/sda" ];

  services.openssh.enable = true;

  networking.retiolum = retiolumAddresses.makanek;

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <system-secrets/retiolum.key>;
    mode = "400";
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

   # Only allow PFS-enabled ciphers with AES256
   sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
  };

  security.acme = {
    acceptTerms = true;
    email = kieran.email;
  };

  environment.systemPackages = [ pkgs.vim pkgs.git pkgs.tmux pkgs.python3Packages.python ];
}
