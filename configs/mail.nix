{ config, pkgs, lib, ... }:
let
  generateTaggingScript = filters:
    let
      template = { tags, query, message ? "tagging ${query} -> ${lib.concatStringsSep " " tags}", ... }: ''
        echo '${message}'
        ${pkgs.notmuch}/bin/notmuch tag ${lib.concatStringsSep " " tags} -- "${query}"
      '';
    in lib.concatStringsSep "\n" (map template filters);

  taggingConfig = [
    {
      query = "from:nebenan.de";
      tags = [ "-inbox" "-unread" ];
    }
    {
      query = "subject:fd-noti";
      tags = [ "-inbox -unread" ];
    }
    {
      query = "subject:miaEngiadina AND subject:\"PR run failed\"";
      tags = [ "+deleted" ];
    }
  ];

  pass = id: "${pkgs.pass}/bin/pass ${id}";
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
in {
  environment.systemPackages = [
    pkgs.neomutt

    (pkgs.writers.writeDashBin "mua" ''
      if [ $# -eq 0 ]; then
        ${much}/bin/much-kmein
      else
        ${much}/bin/much-kmein -q "$*"
      fi
    '')

    (pkgs.writers.writeDashBin "mail-fetch" ''
      ${pkgs.isync}/bin/mbsync --all
      ${pkgs.notmuch}/bin/notmuch new
    '')

    (pkgs.writers.writeDashBin "mail-rm-deleted" ''
      ${pkgs.notmuch}/bin/notmuch search --output files --format=text0 tag:deleted | ${pkgs.findutils}/bin/xargs -r0 rm
      ${pkgs.notmuch}/bin/notmuch new
    '')
  ];

  home-manager.users.me = {
    programs.msmtp.enable = true;

    programs.mbsync.enable = true;

    programs.notmuch = {
      enable = true;
      new.tags = [ "unread" "inbox" ];
      search.excludeTags = [ "deleted" "spam" ];
      hooks.postNew = generateTaggingScript taggingConfig;
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
        passwordCommand = pass "mail/kieran@fysi.tech";
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
        passwordCommand = pass "mail/2210@cock.li";
      };
      kieran-gmail = enableDefaults {
        primary = false;
        flavor = "gmail.com";
        address = "kieran.meinhardt@gmail.com";
        realName = config.niveum.user.name;
        userName = "kieran.meinhardt";
        passwordCommand = pass "mail/kieran.meinhardt@gmail.com";
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
        passwordCommand = pass "mail/amroplay@gmail.com";
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
        passwordCommand = pass "shared/posteo/password";
      };
      hu-berlin = enableDefaults {
        primary = false;
        address = "meinhark@hu-berlin.de";
        realName = config.niveum.user.name;
        userName = "meinhark";
        passwordCommand = pass "shared/eduroam/password";
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
