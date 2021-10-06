{ pkgs, lib, ... }:
let
  accounts.meinhark = {
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

  accounts.meinhaki = {
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

  accounts.fysi = rec {
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

  accounts.cock = rec {
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

  accounts.posteo = rec {
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

  accounts.amroplay = rec {
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

  accounts.kieran-gmail = rec {
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
in
{
  environment.systemPackages = [ pkgs.neomutt ];

  home-manager.users.me.xdg.configFile."neomutt/neomuttrc".text = ''
    set mailcap_path = ${pkgs.writeText "mailcap" ''
      text/plain; $EDITOR %s ;
      text/html; ${pkgs.lynx}/bin/lynx -assume_charset=%{charset} -display_charset=utf-8 -dump %s; nametemplate=%s.html; copiousoutput;
      image/*; ${pkgs.sxiv}/bin/sxiv %s ;
      video/*; ${pkgs.utillinux}/bin/setsid ${pkgs.mpv}/bin/mpv --quiet %s &; copiousoutput
      audio/*; ${pkgs.mpv}/bin/mpv %s ;
      application/pdf; ${pkgs.zathura}/bin/zathura %s ;
      application/pgp-encrypted; ${pkgs.gnupg}/bin/gpg -d '%s'; copiousoutput;
      application/pgp-keys; ${pkgs.gnupg}/bin/gpg --import '%s'; copiousoutput;
    ''}:$mailcap_path

    set sidebar_visible
    set sidebar_format = "%D%?F? [%F]?%* %?N?%N/?%S"
    set sidebar_width = 25                  # Plenty of space
    set sidebar_divider_char = '│'          # Pretty line-drawing character
    set mail_check_stats

    set index_format="%2C %Z %D %-15.15L %s"
    set date_format="%F %R"

    set sort = 'reverse-date'
    set sleep_time = 0		# Pause 0 seconds for informational messages
    set markers = no		# Disables the `+` displayed at line wraps
    set mark_old = no		# Unread mail stay unread until read
    set mime_forward = yes		# attachments are forwarded with mail
    set wait_key = no		# mutt won't ask "press key to continue"
    set fast_reply			# skip to compose when replying
    set forward_format = "Fwd: %s"	# format of subject when forwarding
    set forward_quote		# include message in forwards
    set reverse_name		# reply as whomever it was to
    set include=ask-no			# don't include message in replies
    auto_view text/html		# automatically show html (mailcap uses lynx)
    auto_view application/pgp-encrypted
    alternative_order text/plain text/enriched text/html

    set abort_noattach abort_noattach_regex="\<(attach|attached|attachments?|Anhang|angehängt)\>"
    set attach_save_dir=/tmp
    set fast_reply

    set narrow_tree # narrow threads for more depth

    bind index,pager B sidebar-toggle-visible   # Use 'B' to switch the Sidebar on and off
    bind index,pager \Ck sidebar-prev
    bind index,pager \Cj sidebar-next
    bind index,pager \Co sidebar-open
    bind index,pager \Cp sidebar-prev-new
    bind index,pager \Cn sidebar-next-new

    set query_command = "khard email --parsable %s"
    bind editor <Tab> complete-query
    bind editor ^T    complete

    set mail_check = 90
    set timeout = 15
    # set imap_check_subscribed
    set use_from

    set spoolfile="+INBOX"

    set header_cache="~/.cache/mutt" message_cachedir="~/.cache/mutt"

    source ${pkgs.writeText "accounts.neomuttrc" ''
      set realname = "Kierán Meinhardt"
      account-hook . 'unset imap_user imap_pass smtp_user smtp_pass'
      # set accordingly: postponed trash record
      ${lib.concatStringsSep "\n\n" (lib.mapAttrsToList (name: account: let imapRoot = "imaps://${account.user}@${account.imap}"; in ''
        account-hook ${account.user}@${account.imap} 'set imap_user="${account.user}" imap_pass="${account.password}"'
        account-hook ${account.user}@${account.smtp} 'set smtp_user="${account.user}" smtp_pass="${account.password}"'
        folder-hook  ${account.user}@${account.imap} 'set smtp_url="${account.smtpSettings "${account.user}@${account.smtp}"}" from="${account.address}" record="${imapRoot}/${account.folders.sent}" postponed="${imapRoot}/${account.folders.drafts}" trash="${imapRoot}/${account.folders.trash}"'
        named-mailboxes "${name}" ${imapRoot} ${
          lib.concatStringsSep " "
            (lib.mapAttrsToList
              (folder: imapFolder: ''"${name}.${folder}" "${imapRoot}/${imapFolder}"'')
              account.folders)
        }
      '') accounts)}
    ''}

    source ${pkgs.writeText "colors.neomuttrc" ''
      # Default index colors:
      # color index yellow default '.*'
      color index_author red default '.*'
      color index_number blue default
      color index_subject cyan default '.*'

      # New mail is boldened:
      # color index brightyellow black "~N"
      color index_author brightred black "~N"
      color index_subject brightcyan black "~N"

      # Tagged mail is highlighted:
      color index brightyellow blue "~T"
      color index_author brightred blue "~T"
      color index_subject brightcyan blue "~T"

      # Other colors and aesthetic settings:
      mono bold bold
      mono underline underline
      mono indicator reverse
      mono error bold
      color normal default default
      # color indicator brightblack white
      # color sidebar_highlight red default
      # color sidebar_divider brightblack black
      # color sidebar_flagged red black
      color sidebar_new red black
      # color normal brightyellow default
      color error red default
      color tilde black default
      color message cyan default
      color markers red white
      color attachment white default
      color search brightmagenta default
      # color status brightyellow black
      color hdrdefault brightgreen default
      color quoted green default
      color quoted1 blue default
      color quoted2 cyan default
      color quoted3 yellow default
      color quoted4 red default
      color quoted5 brightred default
      color signature brightgreen default
      color bold black default
      color underline black default
      color normal default default

      # Regex highlighting:
      color header blue default ".*"
      color header brightmagenta default "^(From)"
      color header brightcyan default "^(Subject)"
      color header brightwhite default "^(CC|BCC)"
      color body brightred default "[\-\.+_a-zA-Z0-9]+@[\-\.a-zA-Z0-9]+" # Email addresses
      color body brightblue default "(https?|ftp)://[\-\.,/%~_:?&=\#a-zA-Z0-9]+" # URL
      color body yellow default "^(\t| )*(-|\\*) \.*" # List items as yellow
      color body red default "(BAD signature)|^gpg: BAD signature from.*"
      color body brightgreen default "(Good signature)|^gpg: Good signature .*"
      color body brightyellow default "^gpg: "
      mono body bold "^gpg: Good signature"
      mono body bold "^gpg: BAD signature from.*"
      color body red default "([a-z][a-z0-9+-]*://(((([a-z0-9_.!~*'();:&=+$,-]|%[0-9a-f][0-9a-f])*@)?((([a-z0-9]([a-z0-9-]*[a-z0-9])?)\\.)*([a-z]([a-z0-9-]*[a-z0-9])?)\\.?|[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+)(:[0-9]+)?)|([a-z0-9_.!~*'()$,;:@&=+-]|%[0-9a-f][0-9a-f])+)(/([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*(;([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*)*(/([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*(;([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*)*)*)?(\\?([a-z0-9_.!~*'();/?:@&=+$,-]|%[0-9a-f][0-9a-f])*)?(#([a-z0-9_.!~*'();/?:@&=+$,-]|%[0-9a-f][0-9a-f])*)?|(www|ftp)\\.(([a-z0-9]([a-z0-9-]*[a-z0-9])?)\\.)*([a-z]([a-z0-9-]*[a-z0-9])?)\\.?(:[0-9]+)?(/([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*(;([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*)*(/([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*(;([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*)*)*)?(\\?([-a-z0-9_.!~*'();/?:@&=+$,]|%[0-9a-f][0-9a-f])*)?(#([-a-z0-9_.!~*'();/?:@&=+$,]|%[0-9a-f][0-9a-f])*)?)[^].,:;!)? \t\r\n<>\"]"
    ''}
  '';
}
