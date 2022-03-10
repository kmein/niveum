{
  config,
  pkgs,
  lib,
  ...
}: let
  davHome = "~/.local/share/dav";
  kmeinCloud = {
    davEndpoint = "https://cloud.xn--kiern-0qa.de/remote.php/dav";
    username = "kieran";
    password = lib.fileContents <secrets/nextcloud/password>;
  };
  fysiCloud = {
    davEndpoint = "https://nextcloud.fysi.dev/remote.php/dav";
    username = "kmein";
    password = lib.fileContents <secrets/nextcloud-fysi/password>;
  };
in {
  environment.systemPackages = [pkgs.khal pkgs.vdirsyncer pkgs.khard pkgs.todoman];

  systemd.user.services.vdirsyncer = {
    enable = true;
    wants = ["network-online.target"];
    wantedBy = ["default.target"];
    startAt = "*:00/10";
    script = ''
      ${pkgs.vdirsyncer}/bin/vdirsyncer sync
      ${pkgs.khal}/bin/khal printcalendars # https://lostpackets.de/khal/configure.html#syncing
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
    };
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

      "todoman/config.py".text = ''
        path = "${davHome}/calendar/*"
        date_format = "%Y-%m-%d"
        time_format = "%H:%M"
        default_due = 0
        default_list = "Personal"
        startable = True
      '';

      "khal/config".text = ''
        [calendars]

        [[alew]]
        path = ${davHome}/calendar/alew
        color = "light gray"

        [[personal]]
        path = ${davHome}/calendar/personal
        color = "light cyan"

        [[uni]]
        path = ${davHome}/calendar/uni-1
        color = "yellow"

        [[fysi]]
        path = ${davHome}/calendar/fysi-1
        color = "light magenta"

        [[fysi_team]]
        path = ${davHome}/calendar/personal_shared_by_fdf
        color = "light red"

        [[birthdays]]
        path = ${davHome}/contacts/contacts
        type = birthdays
        color = "light green"

        [default]
        highlight_event_days = True
        timedelta = 5d
        print_new = path
        default_calendar = personal

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
        collections = ["contacts"]
        conflict_resolution = "b wins"

        [pair kalender]
        a = "kalender_local"
        b = "kalender_cloud"
        collections = ["personal", "alew", "uni-1"]
        conflict_resolution = "b wins"

        [pair fysi]
        a = "kalender_local"
        b = "fysi_cloud"
        collections = ["fysi-1", "personal_shared_by_fdf"]
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
        url = "${kmeinCloud.davEndpoint}/addressbooks/users/${kmeinCloud.username}/"
        username = "${kmeinCloud.username}"
        password = "${kmeinCloud.password}"

        [storage kalender_cloud]
        type = "caldav"
        url = "${kmeinCloud.davEndpoint}/calendars/${kmeinCloud.username}/"
        username = "${kmeinCloud.username}"
        password = "${kmeinCloud.password}"

        [storage fysi_cloud]
        type = "caldav"
        url = "${fysiCloud.davEndpoint}/calendars/${fysiCloud.username}/"
        username = "${fysiCloud.username}"
        password = "${fysiCloud.password}"
      '';
    };
  };
}
