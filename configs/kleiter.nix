{ ... }:
let ssid = "Kleiter Gast";
in {
  networking.wireless.networks.${ssid}.psk = "Kleiter-Gast";

  # fix dns
  systemd.network.networks.kleiter = {
    dns = [ "8.8.8.8" "8.8.4.4" ];
    networkConfig.DHCP = "yes";
    matchConfig.Name = "wlp3s0";
    matchConfig.SSID = ssid;
  };
}
