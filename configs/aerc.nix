{
  pkgs,
  config,
  self,
  ...
}:
{
  imports = [ self.nixosModules.mail ];

  age.secrets = {
    email-password-ical-ephemeris = {
      file = ../secrets/email-password-ical-ephemeris.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    email-password-cock = {
      file = ../secrets/email-password-cock.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    email-password-posteo = {
      file = ../secrets/email-password-posteo.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  niveum.mail.users.me = {
    realName = pkgs.lib.niveum.kieran.name;
    maildir = "${config.users.users.me.home}/state/Maildir";
    notmuch = {
      enable = true;
      settings.new.tags = "";
    };

    accounts = {
      cock = {
        address = "2210@cock.li";
        realName = "2210";
        passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-cock.path}";
        imap.host = "mail.cock.li";
        smtp = {
          host = "mail.cock.li";
          port = 587;
          starttls = true;
        };
      };
      ical-ephemeris = {
        address = "ical.ephemeris@web.de";
        realName = "Kieran from iCal Ephemeris";
        passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-ical-ephemeris.path}";
        imap.host = "imap.web.de";
        smtp = {
          host = "smtp.web.de";
          port = 587;
          starttls = true;
        };
      };
      posteo = {
        primary = true;
        address = "kieran.meinhardt@posteo.net";
        aliases = [ "kmein@posteo.de" ];
        passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-posteo.path}";
        imap.host = "posteo.de";
        smtp.host = "posteo.de";
        aerc.pgp-key-id = pkgs.lib.niveum.kieran.pgpFingerprint;
      };
    };

    aerc.binds = {
      # Binds are of the form <key sequence> = <command to run>
      # To use '=' in a key sequence, substitute it with "Eq": "<Ctrl+Eq>"
      # If you wish to bind #, you can wrap the key sequence in quotes: "#" = quit
      global = {
        "<C-p>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab <Enter>";
        "<C-t>" = ":term<Enter>";
        "?" = ":help keys<Enter>";
      };
      messages = {
        q = ":quit<Enter>";
        j = ":next <Enter>";
        "<Down>" = ":next<Enter>";
        "<C-d>" = ":next 50%<Enter>";
        "<C-f>" = ":next 100%<Enter>";
        "<PgDn>" = ":next 100%<Enter>";
        k = ":prev <Enter>";
        "<Up>" = ":prev<Enter>";
        "<C-u>" = ":prev 50%<Enter>";
        "<C-b>" = ":prev 100%<Enter>";
        "<PgUp>" = ":prev 100%<Enter>";
        g = ":select 0 <Enter>";
        G = ":select -1<Enter>";
        J = ":next-folder <Enter>";
        K = ":prev-folder<Enter>";
        H = ":collapse-folder<Enter>";
        L = ":expand-folder<Enter>";
        "<space>" = ":read -t<Enter>:next<Enter>";
        v = ":mark -t<Enter>";
        V = ":mark -v<Enter>";
        F = ":flag -t<Enter>";
        T = ":toggle-threads<Enter>";
        "<Enter>" = ":view<Enter>";
        d = ":prompt 'Delete? ' 'delete-message'<Enter>";
        D = ":move Trash<Enter>";
        A = ":archive flat<Enter>";
        C = ":compose<Enter>";
        rr = ":reply -a<Enter>";
        rq = ":reply -aq<Enter>";
        Rr = ":reply<Enter>";
        Rq = ":reply -q<Enter>";
        c = ":cf<space>";
        "$" = ":term<space>";
        "!" = ":term<space>";
        "|" = ":pipe<space>";
        "/" = ":search<space>";
        "\\" = ":filter <space>";
        n = ":next-result<Enter>";
        N = ":prev-result<Enter>";
        "<Esc>" = ":clear<Enter>";
        "*" = ":filter -x Flagged<Enter>";
      };
      view = {
        tr = ":pipe ${pkgs.trans}/bin/trans -show-original n -b -no-autocorrect<Enter>"; # https://man.sr.ht/~rjarry/aerc/integrations/translator.md
        "/" = ":toggle-key-passthrough <Enter> /";
        q = ":close<Enter>";
        O = ":open<Enter>";
        S = ":save<space>";
        "|" = ":pipe<space>";
        D = ":move Trash<Enter>";
        A = ":archive flat<Enter>";
        "<C-l>" = ":open-link <space>";
        f = ":forward <Enter>";
        rr = ":reply -a<Enter>";
        rq = ":reply -aq<Enter>";
        Rr = ":reply<Enter>";
        Rq = ":reply -q<Enter>";
        H = ":toggle-headers<Enter>";
        "<C-k>" = ":prev-part<Enter>";
        "<C-j>" = ":next-part<Enter>";
        J = ":next <Enter>";
        K = ":prev<Enter>";
      };
      "view::passthrough" = {
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<Esc>" = ":toggle-key-passthrough<Enter>";
      };
      compose = {
        # Keybindings used when the embedded terminal is not selected in the compose
        # view
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<C-k>" = ":prev-field<Enter>";
        "<C-j>" = ":next-field<Enter>";
        "<tab>" = ":next-field<Enter>";
        "<C-p>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
      };

      "compose::editor" = {
        # Keybindings used when the embedded terminal is selected in the compose view
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<C-k>" = ":prev-field<Enter>";
        "<C-j>" = ":next-field<Enter>";
        "<C-p>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
      };

      "compose::review" = {
        # Keybindings used when reviewing a message to be sent
        y = ":send <Enter>";
        n = ":abort<Enter>";
        p = ":postpone<Enter>";
        q = ":choose -o d discard abort -o p postpone postpone<Enter>";
        e = ":edit<Enter>";
        a = ":attach<space>";
        d = ":detach<space>";
      };

      terminal = {
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<C-p>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
      };
    };

    aerc.settings = {
      ui.sort = "-r date";
      ui.timestamp-format = "2006-01-02 15:04";
      ui.mouse-enabled = true;
      ui.spinner = ". , .";
      general.pgp-provider = "gpg";
      viewer = {
        pager = "${pkgs.less}/bin/less -R";
      };
      compose = {
        # address-book-cmd = "khard email --remove-first-line --parsable '%s'";
        reply-to-self = false;
        no-attachment-warning = "(attach|attached|attachments?|anbei|Anhang|angehängt|beigefügt)";
      };
      filters = {
        "text/plain" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
        "text/calendar" = "${pkgs.aerc}/libexec/aerc/filters/calendar";
        "text/html" = "${pkgs.aerc}/libexec/aerc/filters/html"; # Requires w3m, dante
        # "text/html" =
        #   "${pkgs.aerc}/share/aerc/filters/html | ${pkgs.aerc}/share/aerc/filters/colorize";
        # "text/*" =
        #   ''${pkgs.bat}/bin/bat -fP --theme=ansi --file-name="$AERC_FILENAME "'';
        "message/delivery-status" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
        "message/rfc822" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
        "application/x-sh" = "${pkgs.bat}/bin/bat -fP -l sh";
      };
      openers =
        let
          as-pdf = pkgs.writers.writeDash "as-pdf" ''
            d=$(mktemp -p "$XDG_RUNTIME_DIR" -d)
            trap clean EXIT
            clean() {
              rm -rf "$d"
            }
            ${pkgs.libreoffice}/bin/libreoffice --headless --convert-to pdf "$1" --outdir "$d"
            ${pkgs.zathura}/bin/zathura "$d"/*.pdf
          '';
        in
        {
          "image/*" = "${pkgs.nsxiv}/bin/nsxiv";
          "application/pdf" = "${pkgs.zathura}/bin/zathura";
          "text/html" = "${pkgs.firefox}/bin/firefox";
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = toString as-pdf;
          "application/vnd.oasis.opendocument.text" = toString as-pdf;
          "video/*" = "${pkgs.mpv}/bin/mpv";
          "audio/*" = "${pkgs.mpv}/bin/mpv";
        };
    };
  };
}
