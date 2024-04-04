{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (import ../lib/email.nix) defaults thunderbirdProfile;
in {
  age.secrets = {
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

  home-manager.users.me = {
    accounts.email.maildirBasePath = "${config.users.users.me.home}/sync/Maildir";

    services.mbsync = {
      enable = true;
      frequency = "daily";
      preExec = "${pkgs.coreutils}/bin/mkdir -p ${config.home-manager.users.me.accounts.email.maildirBasePath}";
      postExec = "${pkgs.notmuch}/bin/notmuch new";
    };

    programs.notmuch = {
      enable = true;
      extraConfig = {
        database.path = config.home-manager.users.me.accounts.email.maildirBasePath;
        new.tags = "";
        user.name = defaults.realName;
        user.primary_email = config.home-manager.users.me.accounts.email.accounts.posteo.address;
      };
    };

    programs.mbsync = {
      enable = true;
      extraConfig = lib.concatStringsSep "\n\n" (lib.mapAttrsToList (name: account: ''
          IMAPAccount ${name}
          CertificateFile /etc/ssl/certs/ca-certificates.crt
          Host ${account.imap.host}
          PassCmd "${toString account.passwordCommand}"
          User ${account.userName}
          SSLType IMAPS
          ${lib.optionalString (lib.isInt account.imap.port) "Port ${toString account.imap.port}"}

          IMAPStore ${name}-remote
          Account ${name}

          MaildirStore ${name}-local
          Path ${config.home-manager.users.me.accounts.email.maildirBasePath}/${name}/
          SubFolders Verbatim

          Channel ${name}
          Create Near
          Expunge None
          Far :${name}-remote:
          Near :${name}-local:
          Patterns *
          Remove None
          SyncState *
        '')
        config.home-manager.users.me.accounts.email.accounts);
    };

    accounts.email.accounts = {
      cock =
        lib.recursiveUpdate defaults
        rec {
          address = "2210@cock.li";
          userName = address;
          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-cock.path}";
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
          imap.port = 993;
          imap.tls.enable = true;
          smtp.host = imap.host;
          smtp.port = 465;
          smtp.tls.enable = true;
          primary = true;
          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-posteo.path}";
          himalaya = {
            enable = true;
            backend = "imap";
            sender = "smtp";
          };
        };
    };

    programs.himalaya.enable = true;

    programs.thunderbird = {
      enable = true;
      settings = {
      };
      profiles.${thunderbirdProfile} = {
        isDefault = true;
        settings = {
          "mail.default_send_format" = 1;
          "msgcompose.default_colors" = false;
          "msgcompose.text_color" = config.lib.stylix.colors.withHashtag.base00;
          "msgcompose.background_color" = config.lib.stylix.colors.withHashtag.base05;
        };
        userChrome = ''
        '';
        userContent = ''
        '';
        withExternalGnupg = false;
      };
    };

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
          # address-book-cmd = "khard email --remove-first-line --parsable '%s'";
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
