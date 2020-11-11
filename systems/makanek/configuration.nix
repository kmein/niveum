{ config, pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran;
in
{
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/codimd.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/nextcloud.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/version.nix>
    <niveum/modules/retiolum.nix>
    {
      services.gitea = {
        enable = true;
        disableRegistration = true;
        rootUrl = "https://code.xn--kiern-0qa.de";
        appName = "code.kierán.de";
      };
      networking.firewall.allowedTCPPorts = [ config.services.gitea.httpPort ];
      services.nginx.virtualHosts."code.xn--kiern-0qa.de"  ={
        forceSSL = true;
        enableACME = true;
        locations."/".extraConfig = "proxy_pass http://localhost:3000;";
      };
    }
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

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

  environment.systemPackages = [ pkgs.vim pkgs.git ];
}
