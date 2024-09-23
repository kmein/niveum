{ config, ... }:
{
  networking.firewall.allowedUDPPorts = [ 51820 ];
  networking.wg-quick.interfaces.aether = {
    dns = ["192.168.178.1" "fritz.box"];
    listenPort = 51820;
    privateKeyFile = config.age.secrets.wireguard-aether-key.path;
    peers = [
      {
        allowedIPs = ["192.168.178.0/24" "0.0.0.0/0"];
        endpoint = "lng5gx2rmssv8ge1.myfritz.net:58997";
        persistentKeepalive = 25;
        presharedKeyFile = config.age.secrets.wireguard-aether-psk.path;
        publicKey = "8Rr7BueC0CGmycBQFS7YM7VF7Adkdc1ZcLFy8YXyOQk=";
      }
    ];
  };
}
