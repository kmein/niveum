{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.nm-applet.enable = true;

  networking.networkmanager = {
    enable = true;
    plugins = [
      pkgs.networkmanager-openvpn
      pkgs.networkmanager-fortisslvpn
    ];
    wifi.macAddress = "random";
    ethernet.macAddress = "random";
    unmanaged = [ "docker*" ];
  };

  users.users.me.extraGroups = [ "networkmanager" ];

  # networkd would lease a second address on links nm already manages
  networking.useDHCP = lib.mkIf config.networking.useNetworkd false;
  hardware.facter.detected.dhcp.enable = lib.mkIf config.networking.useNetworkd false;

  # NetworkManager owns the physical links, so networkd (enabled by kartei's
  # tincr module) is left managing only tunnel interfaces, which all set
  # RequiredForOnline=no. With nothing left to wait for,
  # systemd-networkd-wait-online never becomes satisfied and fails after its
  # 120s timeout; NetworkManager-wait-online.service reaches
  # network-online.target here anyway.
  systemd.network.wait-online.enable = false;

  environment.systemPackages = [
    pkgs.speedtest-cli
    pkgs.networkmanager-openvpn
    pkgs.networkmanagerapplet
    pkgs.networkmanager-fortisslvpn
  ];
}
