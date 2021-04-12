{ pkgs, config, ... }:
{
  services.xserver.displayManager.sessionCommands = "${pkgs.seafile-client}/bin/seafile-applet &";

  home-manager.users.me.xdg.configFile = {
    "Seafile/Seafile Client.conf".source = (pkgs.formats.ini {}).generate "Seafile Client.conf" {
      Behavior = {
        hideDockIcon = false;
        hideMainWindowWhenStarted = true;
      };
      Settings = {
        computerName = config.networking.hostName;
        lastShiburl = "https://box.hu-berlin.de";
      };
      UsedServerAddresses.main = "https://box.hu-berlin.de";
    };
  };

  environment.systemPackages = [ pkgs.seafile-client pkgs.seafile-shared ];
}
