{
  lib,
  mainMailbox ? "posteo",
  ...
}: {
  uni = {
    user = "meinhark";
    password = lib.strings.fileContents <secrets/eduroam/password>;
    address = "kieran.felix.meinhardt@hu-berlin.de";
    imap = "mailbox.cms.hu-berlin.de";
    smtp = "mailhost.cms.hu-berlin.de";
    smtpSettings = smtp: "smtp://${smtp}";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  uni-old = {
    user = "meinhark";
    password = lib.strings.fileContents <secrets/eduroam/password>;
    address = "meinhark@informatik.hu-berlin.de";
    imap = "mailbox.informatik.hu-berlin.de";
    smtp = "mailhost.informatik.hu-berlin.de";
    smtpSettings = smtp: "smtp://${smtp}";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  work-uni = {
    user = "meinhaki";
    password = lib.strings.fileContents <secrets/mail/meinhaki>;
    address = "kieran.meinhardt@hu-berlin.de";
    imap = "mailbox.cms.hu-berlin.de";
    smtp = "mailhost.cms.hu-berlin.de";
    smtpSettings = smtp: "smtp://${smtp}";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  work-admin = {
    user = "dslalewa";
    password = lib.strings.fileContents <secrets/mail/dslalewa>;
    address = "admin.alew.vglsprwi@hu-berlin.de";
    imap = "mailbox.cms.hu-berlin.de";
    smtp = "mailhost.cms.hu-berlin.de";
    smtpSettings = smtp: "smtp://${smtp}";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  uni-fsi = {
    user = "fsklassp";
    password = lib.strings.fileContents <secrets/mail/fsklassp>;
    address = "fsklassp@hu-berlin.de";
    imap = "mailbox.cms.hu-berlin.de";
    smtp = "mailhost.cms.hu-berlin.de";
    smtpSettings = smtp: "smtp://${smtp}";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  work-fysi = rec {
    user = "kieran@fysi.tech";
    address = user;
    password = lib.strings.fileContents <secrets/mail/fastmail>;
    imap = "imap.fastmail.com";
    smtp = "smtp.fastmail.com";
    smtpSettings = smtp: "smtps://${smtp}:465";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  cock = rec {
    user = "2210@cock.li";
    address = user;
    password = lib.strings.fileContents <secrets/mail/cock>;
    imap = "mail.cock.li";
    smtp = imap;
    smtpSettings = smtp: "smtp://${smtp}:587";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  "${mainMailbox}" = rec {
    user = "kieran.meinhardt@posteo.net";
    address = user;
    password = lib.strings.fileContents <secrets/mail/posteo>;
    imap = "posteo.de";
    smtp = imap;
    smtpSettings = smtp: "smtp://${smtp}";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };

  google-amro = rec {
    user = "amroplay@gmail.com";
    address = user;
    password = lib.strings.fileContents <secrets/mail/gmail/amroplay>;
    imap = "imap.gmail.com";
    smtp = "smtp.gmail.com";
    smtpSettings = smtp: "smtps://${smtp}:465";
    folders = {
      drafts = "[Gmail]/Drafts";
      sent = "[Gmail]/Sent Mail";
      trash = "[Gmail]/Bin";
    };
  };

  google-kieran = rec {
    user = "kieran.meinhardt@gmail.com";
    address = user;
    password = lib.strings.fileContents <secrets/mail/gmail/kieran.meinhardt>;
    imap = "imap.gmail.com";
    smtp = "smtp.gmail.com";
    smtpSettings = smtp: "smtps://${smtp}:465";
    folders = {
      drafts = "[Gmail]/Entwürfe";
      sent = "[Gmail]/Gesendet";
      trash = "[Gmail]/Papierkorb";
    };
  };
}
