{ pkgs, lib, ... }:
{
  environment.systemPackages = [ pkgs.calcurse ];

  home-manager.users.me = {
    home.file = {
      ".calcurse/conf".text = ''
        appearance.calendarview=monthly
        appearance.layout=1
        daemon.enable=no
        daemon.log=no
        format.inputdate=4
        format.outputdate=%F
        general.confirmquit=no
        general.confirmdelete=yes
        general.firstdayofweek=monday
        appearance.theme=default on default
      '';
      ".calcurse/caldav/config".text = lib.generators.toINI {} {
        General = {
          Binary = "${pkgs.calcurse}/bin/calcurse";
          Hostname = "posteo.de:8443";
          Path = "/calendars/kieran.meinhardt/default/";
          InsecureSSL = "No";
          DryRun = "No";
          Verbose = "Yes";
        };
        Auth = {
          Username = "kieran.meinhardt@posteo.net";
          Password = lib.strings.fileContents <secrets/mail/posteo>;
        };
      };
    };
  };

}
