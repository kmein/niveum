{
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

  environment.systemPackages = [
    pkgs.speedtest-cli
    pkgs.networkmanager-openvpn
    pkgs.networkmanagerapplet
    pkgs.networkmanager-fortisslvpn
  ];
}
