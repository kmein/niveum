{
  pkgs,
  config,
  lib,
  ...
}: let
  defaults = {
    aerc.enable = true;
    realName = "Kierán Meinhardt";
    folders.inbox = "INBOX";
  };
  hu-defaults = {
    imap.host = "mailbox.cms.hu-berlin.de";
    imap.port = 993;
    smtp.host = "mailhost.cms.hu-berlin.de";
    smtp.port = 25;
    smtp.tls.useStartTls = true;
  };
in {
  age.secrets = {
    email-password-cock = {
      file = ../secrets/email-password-cock.age;
      owner = config.users.users.me.name;
    };
    email-password-fysi = {
      file = ../secrets/email-password-fysi.age;
      owner = config.users.users.me.name;
    };
    email-password-posteo = {
      file = ../secrets/email-password-posteo.age;
      owner = config.users.users.me.name;
    };
    email-password-meinhark = {
      file = ../secrets/email-password-meinhark.age;
      owner = config.users.users.me.name;
    };
    email-password-meinhaki = {
      file = ../secrets/email-password-meinhaki.age;
      owner = config.users.users.me.name;
    };
    email-password-dslalewa = {
      file = ../secrets/email-password-dslalewa.age;
      owner = config.users.users.me.name;
    };
    email-password-fsklassp = {
      file = ../secrets/email-password-fsklassp.age;
      owner = config.users.users.me.name;
    };
  };

  home-manager.users.me = {
    accounts.email.accounts = rec {
      hu-student =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "meinhark";
            address = "kieran.felix.meinhardt@hu-berlin.de";
            aliases = ["${userName}@hu-berlin.de"];
            passwordCommand = "cat ${config.age.secrets.email-password-meinhark.path}";
          });
      hu-student-cs =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "meinhark";
            address = "kieran.felix.meinhardt@informatik.hu-berlin.de";
            aliases = ["${userName}@informatik.hu-berlin.de"];
            imap.host = "mailbox.informatik.hu-berlin.de";
            smtp.host = "mailhost.informatik.hu-berlin.de";
            passwordCommand = "cat ${config.age.secrets.email-password-meinhark.path}";
          });
      hu-employee =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "meinhaki";
            address = "kieran.meinhardt@hu-berlin.de";
            aliases = ["${userName}@hu-berlin.de"];
            passwordCommand = "cat ${config.age.secrets.email-password-meinhaki.path}";
            aerc.extraAccounts.signature-file = toString (pkgs.writeText "signature" signature.text);
            signature = {
              showSignature = "append";
              text = ''
                ${defaults.realName}
                Studentische Hilfskraft / Administrator ALEW
                Humboldt-Universität zu Berlin

                Telefon: +49 (0)30 2093 9634
                Raum 3.212, Dorotheenstraße 24, 10117 Berlin-Mitte
                https://alew.hu-berlin.de
              '';
            };
          });
      hu-admin =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "dslalewa";
            address = "admin.alew.vglsprwi@hu-berlin.de";
            aliases = ["${userName}@hu-berlin.de"];
            passwordCommand = "cat ${config.age.secrets.email-password-dslalewa.path}";
            inherit (hu-employee) signature;
            aerc.extraAccounts.signature-file = toString (pkgs.writeText "signature" signature.text);
          });
      hu-fsi =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "fsklassp";
            passwordCommand = "cat ${config.age.secrets.email-password-fsklassp.path}";
            address = "${userName}@hu-berlin.de";
            realName = "FSI Klassische Philologie";
            aerc.extraAccounts.signature-file = toString (pkgs.writeText "signature" signature.text);
            signature = {
              showSignature = "append";
              text = ''
                Fachschafts-Initiative

                Humboldt-Universität zu Berlin
                Sprach- und literaturwissenschaftliche Fakultät
                Institut für klassische Philologie
                Unter den Linden 6
                10099 Berlin
              '';
            };
          });
      fysi =
        lib.recursiveUpdate defaults
        rec {
          address = "kieran@fysi.tech";
          userName = address;
          passwordCommand = "cat ${config.age.secrets.email-password-fysi.path}";
          flavor = "fastmail.com";
        };
      cock =
        lib.recursiveUpdate defaults
        rec {
          address = "2210@cock.li";
          userName = address;
          passwordCommand = "cat ${config.age.secrets.email-password-cock.path}";
          realName = "";
          imap.host = "mail.cock.li";
          smtp.host = imap.host;
        };
      posteo =
        lib.recursiveUpdate defaults
        rec {
          address = "kieran.meinhardt@posteo.net";
          aliases = ["kmein@posteo.de"];
          userName = address;
          imap.host = "posteo.de";
          smtp.host = imap.host;
          primary = true;
          passwordCommand = "cat ${config.age.secrets.email-password-posteo.path}";
          # himalaya = { enable = true; backend = "imap"; sender = "smtp"; };
        };
    };

    # programs.himalaya.enable = true;

    programs.aerc = {
      enable = true;

      extraBinds = {
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

      extraConfig = {
        ui.sort = "-r date";
        ui.timestamp-format = "2006-01-02 15:04";
        ui.mouse-enabled = true;
        ui.spinner = ". , .";
        general.unsafe-accounts-conf = true;
        general.pgp-provider = "gpg";
        viewer = {pager = "${pkgs.less}/bin/less -R";};
        compose = {
          address-book-cmd = "khard email --remove-first-line --parsable '%s'";
          no-attachment-warning = "(attach|attached|attachments?|anbei|Anhang|angehängt)";
        };
        filters = {
          "text/plain" = "${pkgs.gawk}/bin/awk -f ${pkgs.aerc}/share/aerc/filters/colorize";
          "text/calendar" = "${pkgs.gawk}/bin/awk -f ${pkgs.aerc}/share/aerc/filters/calendar";
          "text/html" = "${pkgs.aerc}/share/aerc/filters/html"; # Requires w3m, dante
          # "text/html" =
          #   "${pkgs.aerc}/share/aerc/filters/html | ${pkgs.aerc}/share/aerc/filters/colorize";
          # "text/*" =
          #   ''${pkgs.bat}/bin/bat -fP --file-name="$AERC_FILENAME "'';
          "message/delivery-status" = "${pkgs.gawk}/bin/awk -f ${pkgs.aerc}/share/aerc/filters/colorize";
          "message/rfc822" = "${pkgs.gawk}/bin/awk -f ${pkgs.aerc}/share/aerc/filters/colorize";
          "application/x-sh" = "${pkgs.bat}/bin/bat -fP -l sh";
        };
        openers = let
          as-pdf = pkgs.writers.writeDash "as-pdf" ''
            d=$(mktemp -d)
            trap clean EXIT
            clean() {
              rm -rf "$d"
            }
            ${pkgs.libreoffice}/bin/libreoffice --headless --convert-to pdf "$1" --outdir "$d"
            ${pkgs.zathura}/bin/zathura "$d"/*.pdf
          '';
        in {
          "image/*" = "${pkgs.nsxiv}/bin/nsxiv";
          "application/pdf" = "${pkgs.zathura}/bin/zathura";
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = toString as-pdf;
          "application/vnd.oasis.opendocument.text" = toString as-pdf;
          "video/*" = "${pkgs.mpv}/bin/mpv";
          "audio/*" = "${pkgs.mpv}/bin/mpv";
        };
      };

      templates = {
        # new_message = "hello!";
      };
    };
  };
}
