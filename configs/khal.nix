{
  config,
  pkgs,
  lib,
  ...
}: let
  davHome = "~/.local/share/dav";
  kmeinCloud = {
    davEndpoint = "https://cloud.kmein.de/remote.php/dav";
    username = "kieran";
    passwordFile = config.age.secrets.nextcloud-password-kieran.path;
  };
in {
  age.secrets = {
    nextcloud-password-kieran = {
      file = ../secrets/nextcloud-password-kieran.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    nextcloud-password-fysi = {
      file = ../secrets/nextcloud-password-fysi.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  environment.systemPackages = [
    pkgs.khal
    pkgs.vdirsyncer
    pkgs.khard
    pkgs.todoman
    (pkgs.writers.writeDashBin "todo-procrastinate" ''
      [ $# -eq 1 ] || {
        echo "Usage: $0 TODO_ID" >&2
        exit 1
      }
      todo_id=$1
      new_timestamp=$(${pkgs.todoman}/bin/todo --porcelain show "$todo_id" | ${pkgs.jq}/bin/jq '.due + 24 * 60 * 60')
      new_date=$(${pkgs.coreutils}/bin/date +"%Y-%m-%d %H:%M" -d "@$new_timestamp")
      ${pkgs.todoman}/bin/todo edit "$todo_id" --due "$new_date"
    '')
  ];

  systemd.user.services.vdirsyncer = {
    enable = true;
    wants = ["network-online.target"];
    wantedBy = ["default.target"];
    startAt = "*:00/10";
    script = ''
      ${pkgs.vdirsyncer}/bin/vdirsyncer sync && ${pkgs.khal}/bin/khal printcalendars # https://lostpackets.de/khal/configure.html#syncing
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

        [[kalender_local]]
        path = ${davHome}/calendar/*
        type = discover

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
        collections = ["from b"]
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
        password.fetch = ["command", "cat", "${kmeinCloud.passwordFile}"]

        [storage kalender_cloud]
        type = "caldav"
        url = "${kmeinCloud.davEndpoint}/calendars/${kmeinCloud.username}/"
        username = "${kmeinCloud.username}"
        password.fetch = ["command", "cat", "${kmeinCloud.passwordFile}"]
      '';
    };
  };
}
