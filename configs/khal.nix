{ config, pkgs, lib, ... }:
let
  davHome = "~/.local/share/dav";
  davEndpoint = "https://cloud.xn--kiern-0qa.de/remote.php/dav";
  username = "kieran";
  password = lib.fileContents <secrets/nextcloud/password>;
in
{
  environment.systemPackages = [ pkgs.khal pkgs.vdirsyncer ];

  home-manager.users.me = {
    xdg.configFile = {
      "khal/config".text = ''
        [calendars]

        [[kalender_local]]
        path = ${davHome}/calendar/*
        color = 27
        type = discover

        [[kontakte_local]]
        path = ${davHome}/contacts/contacts/
        color = 11
        type = birthdays

        [default]
        highlight_event_days = True
        timedelta = 5d

        [locale]
        timeformat = %H:%M
        dateformat = %Y-%m-%d
        longdateformat = %Y-%m-%d
        datetimeformat = %Y-%m-%d %H:%M
        longdatetimeformat = %Y-%m-%d %H:%M
        local_timezone = ${config.time.timeZone}
        default_timezone = ${config.time.timeZone}
        weeknumbers = left
      '';

      "vdirsyncer/config".text = ''
        [general]
        status_path = "~/.local/share/vdirsyncer/status/"

        [pair kontakte]
        a = "kontakte_local"
        b = "kontakte_cloud"
        collections = ["from a", "from b"]

        [storage kontakte_local]
        type = "filesystem"
        path = "${davHome}/contacts/"
        fileext = ".vcf"

        [storage kontakte_cloud]
        type = "carddav"
        url = "${davEndpoint}/addressbooks/users/${username}/contacts/"
        username = "${username}"
        password = "${password}"

        [pair kalender]
        a = "kalender_local"
        b = "kalender_cloud"
        collections = ["from a", "from b"]

        [storage kalender_local]
        type = "filesystem"
        path = "${davHome}/calendar/"
        fileext = ".ics"

        [storage kalender_cloud]
        type = "caldav"
        url = "${davEndpoint}/calendars/${username}/personal/"
        username = "${username}"
        password = "${password}"
      '';
    };
  };
}
