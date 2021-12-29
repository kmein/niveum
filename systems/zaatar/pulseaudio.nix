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
