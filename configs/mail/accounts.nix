{ lib, ... }:
let
  inherit (import <niveum/lib>) kieran;

  enableDefaults = lib.recursiveUpdate {
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
    };
    msmtp.enable = true;
    notmuch.enable = true;
  };

  # turns out we have to escape $ because, if the password contains a $, it will get interpolated as a variable by the msmtp `passwordeval` which does: `bash -c "COMMAND; echo"`
  pass_ = file: "echo ${lib.escape ["$"] (lib.escapeShellArg (lib.strings.fileContents file))}";
in
{
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
  hu-berlin-work = enableDefaults {
    primary = false;
    address = "meinhaki@hu-berlin.de";
    realName = kieran.name;
    userName = "meinhaki";
    passwordCommand = pass_ <secrets/mail/meinhaki>;
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
}
