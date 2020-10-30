{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) kieran;

  nixpkgs-much = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "7c2a362b58a1c2ba72d24aa3869da3b1a91d39e1";
    sha256 = "0gl4xndyahasa9dv5mi3x9w8s457wl2xh9lcldizcn1irjvkrzs4";
  }) {};
  much-pkg = nixpkgs-much.haskellPackages.callCabal2nix "much" <niveum/submodules/much> {};
  much = nixpkgs-much.haskell.lib.dontHaddock much-pkg;

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

  # turns out we have to escape $ because, if the password contains a $, it will get interpolated as a variable by the msmtp `passwordeval` which does: `bash -c "COMMAND; echo"`
  pass_ = file: "echo ${lib.escape ["$"] (lib.escapeShellArg (lib.strings.fileContents file))}";

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
    serviceConfig.Type = "oneshot";
    environment.NOTMUCH_CONFIG = config.home-manager.users.me.home.sessionVariables.NOTMUCH_CONFIG;
    script = ''
      export PATH=${lib.makeBinPath [ pkgs.muchsync pkgs.notmuch mail-sync ]}
      mail-sync
      muchsync
    '';
  };

  environment.systemPackages = [
    pkgs.neomutt

    pkgs.notmuch-addrlookup

    mail-sync

    pkgs.muchsync

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
    services.muchsync.remotes =
    let
      muchsyncConfig = host: {
        name = host;
        value = {
          frequency = "*:0/10";
          remote.host = host;
          remote.checkForModifiedFiles = false;
          local.checkForModifiedFiles = false;
          # don't run `notmuch new` locally nor remotely because muchsync is only regularly run after `mail-sync`
          remote.importNew = false;
          local.importNew = false;
        };
      };
    in lib.listToAttrs (map muchsyncConfig [
      "wilde"
      "homeros"
    ]);

    programs.msmtp.enable = true;

    programs.mbsync.enable = true;

    programs.notmuch = {
      enable = true;
      new.tags = [ "new" ];
      search.excludeTags = [ "deleted" "spam" ];
      hooks.postNew = generateTaggingScript tagRules;
      extraConfig.muchsync.and_tags = "inbox;unread";
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
        realName = kieran.name;
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
        realName = kieran.name;
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
        realName = kieran.name;
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
        realName = kieran.name;
        userName = "kieran.meinhardt@posteo.net";
        passwordCommand = pass_ <secrets/mail/posteo>;
      };
      hu-berlin = enableDefaults {
        primary = false;
        address = "meinhark@hu-berlin.de";
        realName = kieran.name;
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
