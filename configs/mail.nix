{ config, pkgs, lib, ... }:
let
  tagRules = [
    {
      query = "to:miaengiadina-pwa@noreply.github.com AND subject:\"PR run failed\"";
      tags = [ "-new" "+deleted" ];
    }
    {
      query = lib.concatStringsSep " OR " [
        "from:noreply-local-guides@google.com"
        "from:google-maps-noreply@google.com"
        "subject:fd-noti"
        "from:nebenan.de"
        "to:miaengiadina-pwa@noreply.github.com"
      ];
      tags = [ "-new" ];
    }
    {
      query = "tag:new";
      tags = [ "-new" "+inbox" ];
    }
  ];

  pass_ = file: "echo ${lib.escapeShellArg (lib.strings.fileContents file)}";

  generateTaggingScript = filters:
    let
      template = { tags, query, message ? "tagging ${query} -> ${lib.concatStringsSep " " tags}", ... }: ''
        echo '${message}'
        ${pkgs.notmuch}/bin/notmuch tag ${lib.concatStringsSep " " tags} -- "${query}"
      '';
    in lib.concatStringsSep "\n" (map template filters);

  enableDefaults = lib.recursiveUpdate {
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
    };
    msmtp.enable = true;
    notmuch.enable = true;
  };

  much-pkg = pkgs.haskellPackages.callCabal2nix "much" <niveum/submodules/much> {};
  much = pkgs.haskell.lib.dontHaddock much-pkg;

  mail-sync = pkgs.writers.writeDashBin "mail-sync" ''
    ${pkgs.isync}/bin/mbsync --all
    ${pkgs.notmuch}/bin/notmuch new
  '';
in {
  environment.variables.NOTMUCH_CONFIG = config.home-manager.users.me.home.sessionVariables.NOTMUCH_CONFIG;

  systemd.services.mail-sync = {
    enable = true;
    wants = [ "network-online.target" ];
    startAt = "*:0/15";
    serviceConfig.User = "kfm";
    environment.NOTMUCH_CONFIG = config.home-manager.users.me.home.sessionVariables.NOTMUCH_CONFIG;
    script = "${mail-sync}/bin/mail-sync";
  };

  environment.systemPackages = [
    pkgs.neomutt

    pkgs.notmuch-addrlookup

    mail-sync

    (pkgs.writers.writeDashBin "mua" ''
      if [ $# -eq 0 ]; then
        ${much}/bin/much-kmein
      else
        ${much}/bin/much-kmein -q "$*"
      fi
    '')

    (pkgs.writers.writeDashBin "mail-clean" ''
      ${pkgs.notmuch}/bin/notmuch search --output files --format=text0 tag:deleted | ${pkgs.findutils}/bin/xargs -r0 rm
      ${pkgs.notmuch}/bin/notmuch new
    '')
  ];


  home-manager.users.me = {
    programs.msmtp.enable = true;

    programs.mbsync.enable = true;

    programs.notmuch = {
      enable = true;
      new.tags = [ "new" ];
      search.excludeTags = [ "deleted" "spam" ];
      hooks.postNew = "${pkgs.afew}/bin/afew --tag --new";
    };

    programs.afew = let
      generateFilters = rules:
        lib.concatStringsSep "\n" (lib.lists.imap1
          (index: {message ? null, query, tags}: ''
            [Filter.${toString index}]
            query = ${query}
            tags = ${lib.concatStringsSep ";" tags}
            ${lib.optionalString (message != null) "message = ${message}"}
          '')
          rules
        );
    in {
      enable = true;
      extraConfig = ''
        [SpamFilter]

        ${generateFilters tagRules}

        [InboxFilter]
      '';
    };

    accounts.email.maildirBasePath = "${config.users.users.me.home}/mail";

    accounts.email.accounts = {
      fysi = enableDefaults {
        primary = false;
        smtp = {
          host = "smtp.fastmail.com";
          port = 465;
          tls.enable = true;
        };
        imap = {
          host = "imap.fastmail.com";
          port = 993;
          tls.enable = true;
        };
        userName = "kieran@fysi.tech";
        address = "kieran@fysi.tech";
        realName = config.niveum.user.name;
        passwordCommand = pass_ <secrets/mail/fastmail>;
      };
      cock = enableDefaults {
        primary = false;
        smtp = {
          host = "mail.cock.li";
          port = 587;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        imap = {
          host = "mail.cock.li";
          port = 993;
          tls.enable = true;
        };
        userName = "2210@cock.li";
        address = "2210@cock.li";
        realName = "2210";
        passwordCommand = pass_ <secrets/mail/cock>;
      };
      kieran-gmail = enableDefaults {
        primary = false;
        flavor = "gmail.com";
        address = "kieran.meinhardt@gmail.com";
        realName = config.niveum.user.name;
        userName = "kieran.meinhardt";
        passwordCommand = pass_ <secrets/mail/gmail/kieran.meinhardt>;
        folders = {
          drafts = "[Gmail]/Entw&APw-rfe";
          sent = "[Gmail]/Gesendet";
          trash = "[Gmail]/Papierkorb";
        };
      };
      amroplay = enableDefaults {
        primary = false;
        flavor = "gmail.com";
        address = "amroplay@gmail.com";
        realName = config.niveum.user.name;
        userName = "amroplay";
        passwordCommand = pass_ <secrets/mail/gmail/amroplay>;
        folders = {
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/Sent Mail";
          trash = "[Gmail]/Bin";
        };
      };
      posteo = enableDefaults {
        primary = true;
        smtp = {
          host = "posteo.de";
          port = 587;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        imap = {
          host = "posteo.de";
          port = 993;
          tls.enable = true;
        };
        address = "kieran.meinhardt@posteo.net";
        realName = config.niveum.user.name;
        userName = "kieran.meinhardt@posteo.net";
        passwordCommand = pass_ <secrets/mail/posteo>;
      };
      hu-berlin = enableDefaults {
        primary = false;
        address = "meinhark@hu-berlin.de";
        realName = config.niveum.user.name;
        userName = "meinhark";
        passwordCommand = pass_ <secrets/eduroam/password>;
        smtp = {
          host = "mailhost.cms.hu-berlin.de";
          port = 25;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        imap = {
          host = "mailbox.cms.hu-berlin.de";
          port = 993;
          tls.enable = true;
        };
      };
    };
  };
}
