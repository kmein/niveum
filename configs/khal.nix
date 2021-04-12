{ config, pkgs, lib, ... }:
let
  davHome = "~/.local/share/dav";
  davEndpoint = "https://cloud.xn--kiern-0qa.de/remote.php/dav";
  username = "kieran";
  password = lib.fileContents <secrets/nextcloud/password>;
in
{
  environment.systemPackages = [ pkgs.khal pkgs.vdirsyncer pkgs.khard pkgs.todoman ];

  systemd.user.services.vdirsyncer = {
    enable = true;
    wants = [ "network-online.target" ];
    wantedBy = [ "default.target" ];
    startAt = "*:00/10";
    script = ''
      ${pkgs.vdirsyncer}/bin/vdirsyncer sync
      ${pkgs.khal}/bin/khal printcalendars # https://lostpackets.de/khal/configure.html#syncing
    '';
  };

  home-manager.users.me = {
    xdg.configFile = {
      "khard/khard.conf".text = ''
        [addressbooks]
        [[contacts]]
        path = ${davHome}/contacts/contacts/

        [general]
        debug = no
        default_action = list
        editor = ${config.environment.variables.EDITOR}
        merge_editor = ${pkgs.vim}/bin/vimdiff

        [contact table]
        display = first_name
        group_by_addressbook = no
        reverse = no
        show_nicknames = no
        show_uids = no
        sort = last_name
        localize_dates = yes
        preferred_phone_number_type = pref, cell, home
        preferred_email_address_type = pref, work, home

        [vcard]
        search_in_source_files = no
        skip_unparsable = no
      '';

      "todoman/todoman.conf".text = ''
        [main]
        path = ${davHome}/calendar/*
        date_format = %F
        time_format = %R
        default_due = 0
        default_list = Personal
        startable = True
      '';

      "khal/config".text = ''
        [calendars]

        [[kalender_local]]
        path = ${davHome}/calendar/*
        type = discover

        [default]
        highlight_event_days = True
        timedelta = 5d

        [locale]
        timeformat = %R
        dateformat = %F
        longdateformat = %F
        datetimeformat = %F %R
        longdatetimeformat = %F %R
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
        collections = ["contacts"]
        conflict_resolution = "b wins"

        [pair kalender]
        a = "kalender_local"
        b = "kalender_cloud"
        collections = ["from a", "from b"]
        conflict_resolution = "b wins"

        [storage kontakte_local]
        type = "filesystem"
        path = "${davHome}/contacts/"
        fileext = ".vcf"

        [storage kalender_local]
        type = "filesystem"
        path = "${davHome}/calendar/"
        fileext = ".ics"

        [storage kontakte_cloud]
        type = "carddav"
        url = "${davEndpoint}/addressbooks/users/${username}/"
        username = "${username}"
        password = "${password}"

        [storage kalender_cloud]
        type = "caldav"
        url = "${davEndpoint}/calendars/${username}/"
        username = "${username}"
        password = "${password}"
      '';
    };
  };
}
