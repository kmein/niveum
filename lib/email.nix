{lib, ...}: {
  cock = rec {
    user = "2210@cock.li";
    address = user;
    imap = "mail.cock.li";
    smtp = imap;
    smtpSettings = smtp: "smtp://${smtp}:587";
    folders = {
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };
}
