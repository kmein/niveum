{ lib, config, pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran;
in
{
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/hedgedoc.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/nextcloud.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/version.nix>
    <niveum/configs/radio.nix>
    <niveum/configs/gitea.nix>
    <niveum/configs/telegram-bots>
    <niveum/configs/weechat.nix>
    <niveum/configs/matterbridge.nix>
    <niveum/modules/retiolum.nix>
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  nixpkgs.config.allowUnfree = true;

  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  networking.hostName = "makanek";

  system.stateVersion = "20.03";

  boot.loader.grub.devices = [ "/dev/sda" ];

  services.openssh.enable = true;

  networking.retiolum = {
    ipv4 = "10.243.2.84";
    ipv6 = "42:0:3c46:f7a9:1f0a:1b2b:822a:6050";
  };

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

  environment.systemPackages = [ pkgs.vim pkgs.git pkgs.tmux ];

  systemd.services.praesenzlehre = {
    description = "Live Ticker zu praesenzlehre-berlin.de";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.curl pkgs.pup ];
    environment.BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    script = ''
      notify() {
        curl -sSL -X POST -H 'Content-Type: application/json' \
           -d "{\"chat_id\": \"@praesenzlehre_berlin\", \"text\": \"$*\"}" \
           "https://api.telegram.org/bot$BOT_TOKEN/sendMessage"
      }

      count=0

      while true; do
        new_count="$(curl -sSL https://praesenzlehre-berlin.org/ | pup '.dk-speakout-signature-count span text{}')"
        if [ "$new_count" -gt "$count" ]; then
          echo "$new_count"
          notify "$new_count Unterzeichner:innen!"
          count="$new_count"
        fi
        sleep 300
      done
    '';
  };
}
