{ config, pkgs, lib, ... }:
let
  pass = id: "${pkgs.pass}/bin/pass ${id}";
  enableDefaults = lib.recursiveUpdate {
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
    };
    msmtp.enable = true;
    neomutt.enable = true;
    notmuch.enable = false;
  };
in
{
  environment.systemPackages = [ pkgs.neomutt ];

  home-manager.users.me = let maildir = "${config.users.users.me.home}/mail"; in {
    accounts.email.maildirBasePath = maildir;
    accounts.email.accounts = {
      cock = enableDefaults {
        primary = false;
        smtp = {
          host = "mail.cock.li";
          port = 587;
          tls.enable = true;
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
        primary = true;
        flavor = "gmail.com";
        address = "kieran.meinhardt@gmail.com";
        realName = config.niveum.user.name;
        userName = "kieran.meinhardt";
        passwordCommand = pass "mail/kieran.meinhardt@gmail.com";
        neomutt.extraConfig = ''
          macro pager,index \' "<save-message>+[Gmail]/Alle Nachrichten<enter>" "Archive"
        '';
        folders = {
          drafts = "[Gmail]/Entw&APw-rfe";
          sent = "[Gmail]/Gesendet";
          trash = "[Gmail]/Papierkorb";
        };
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
          tls.enable = true;
        };
        imap = {
          host = "mailbox.cms.hu-berlin.de";
          port = 993;
          tls.enable = true;
        };
        neomutt.extraConfig = ''
          macro pager,index \' "<save-message>Archives<enter>" "Archive"
        '';
      };
    };

    programs.neomutt = {
      enable = true;
      sidebar.enable = true;
      sort = "reverse-date"; # or reverse-threads
      vimKeys = true;
      checkStatsInterval = 60;
      settings = {
        attribution = "\"%n (%a), %d:\"";
        date_format = "\"%Y-%m-%d %H:%M\"";
        # envelope_from = "yes"; # THIS BREAKS SENDING VIA MSMTP (the default setting already contains msmtpq --read-envelope)
        fast_reply = "yes"; # skip to compose when forwarding
        fcc_attach = "no"; # save attachments with the body
        forward_format = "\"→ %s\""; # format of subject when forwarding
        forward_quote = "yes"; # include message in forwards
        include = "yes"; # include message in replies
        # index_format = "\"%2C %Z %?X?A& ? %D %-15.15F %s (%-4.4c)\"";
        index_format= "\"${pkgs.writeDash "mutt-index" ''
          # http://www.mutt.org/doc/manual/#formatstrings
          recipent="$(echo $1 | sed 's/[^,]*<\([^>]*\)[^,]*/ \1/g')"
          #     output to mutt
          #           V
          echo "%4C %Z %?GI?%GI& ? %[%y-%m-%d] %-20.20a %?M?(%3M)& ? %s %> $recipent %?g?%g?%"
          # args to mutt-index dash script
          # V
        ''} %r |\"";
        mail_check = "60"; # to avoid lags using IMAP with some email providers (yahoo for example)
        mail_check_stats = "yes";
        mark_old = "no"; # unread mail stay unread until read
        markers = "no"; # disables the `+` displayed at line wraps
        mime_forward = "yes"; # forward attachments with mail
        # nm_default_uri = "notmuch://${maildir}";
        # nm_record = "yes";
        # nm_record_tags = "-inbox archive";
        virtual_spoolfile = "yes"; # enable virtual folders
        reverse_name = "yes"; # reply as whomever it was to
        rfc2047_parameters = "yes"; # decode RFC2047-encoded attachments
        sleep_time = "0"; # pause 0 seconds for informational messages
        use_from = "yes";
        wait_key = "no"; # mutt won't ask "press key to continue"
        mailcap_path = toString (pkgs.writeText "mailcap" ''
          text/plain; $EDITOR %s ;
          text/html; ${pkgs.lynx}/bin/lynx -assume_charset=%{charset} -display_charset=utf-8 -dump %s; nametemplate=%s.html; copiousoutput;
          image/*; ${pkgs.sxiv}/bin/sxiv %s ;
          video/*; ${pkgs.utillinux}/bin/setsid ${pkgs.mpv}/bin/mpv --quiet %s &; copiousoutput
          application/*; ${pkgs.xdg_utils}/bin/xdg-open %s ;
        '');
      };
      macros = [
        { action = "<change-dir><kill-line>..<enter>"; map = "browser"; key = "h"; } # go to parent directory
        { action = "T~U<enter><tag-prefix><clear-flag>N<untag-pattern>.<enter>" ; map = "index"; key = "\\Cr"; } # mark all messages read
        { action = "<shell-escape>mbsync -a<enter>"; map = "index"; key = "O"; } # run mbsync to sync all mail
        { action = "<limit>all\\n"; map = "index"; key = "A"; } # show all messages (undo limit)
        # { action = "<save-message>=Archive<enter><enter-command>echo 'Saved to Archive'<enter>"; map = "index"; key = ",a"; }
        {
          action = "<enter-command>unset wait_key<enter><shell-escape>read -p 'Enter a search term to find with notmuch: ' x; echo \\$x >~/.cache/mutt_terms<enter><limit>~i \\\"\\`notmuch search --output=messages \$(cat ~/.cache/mutt_terms) | head -n 600 | perl -le '@a=<>;s/\^id:// for@a;$,=\"|\";print@a' | perl -le '@a=<>; chomp@a; s/\\+/\\\\+/ for@a;print@a' \\`\\\"<enter>";
          map = "index";
          key = "\\Cf";
        }
      ];
      binds = [
        { action = "complete-query"; key = "<Tab>"; map = "editor"; }
        { action = "display-message"; key = "l"; map = "index"; }
        { action = "exit"; key = "h"; map = "attach"; }
        { action = "exit"; key = "h"; map = "pager"; }
        { action = "group-reply"; key = "R"; map = "index"; }
        { action = "group-reply"; key = "R"; map = "pager"; }
        { action = "limit"; key = "L"; map = "index"; }
        { action = "next-entry"; key = "j"; map = "index"; }
        { action = "next-line"; key = "\\005"; map = "pager"; } # mouse wheel
        { action = "next-undeleted"; key = "\\005"; map = "index"; } # mouse wheel
        { action = "noop"; key = "<space>"; map = "editor"; }
        { action = "noop"; key = "\\Cf"; map = "index"; }
        { action = "noop"; key = "h"; map = "index"; }
        { action = "noop"; key = "i"; map = "index"; }
        { action = "noop"; key = "i"; map = "pager"; }
        { action = "previous-entry"; key = "k"; map = "index"; }
        { action = "previous-line"; key = "\\031"; map = "pager"; } # mouse wheel
        { action = "previous-undeleted"; key = "\\031"; map = "index"; } # mouse wheel
        { action = "select-entry"; key = "l"; map = "browser"; }
        { action = "sidebar-next"; key = "\\Cj"; map = "index"; }
        { action = "sidebar-next"; key = "\\Cj"; map = "pager"; }
        { action = "sidebar-next-new"; key = "\\Cn"; map = "index"; }
        { action = "sidebar-next-new"; key = "\\Cn"; map = "pager"; }
        { action = "sidebar-open"; key = "\\Co"; map = "index"; }
        { action = "sidebar-open"; key = "\\Co"; map = "pager"; }
        { action = "sidebar-prev"; key = "\\Ck"; map = "index"; }
        { action = "sidebar-prev"; key = "\\Ck"; map = "pager"; }
        { action = "sidebar-prev-new"; key = "\\Cp"; map = "index"; }
        { action = "sidebar-prev-new"; key = "\\Cp"; map = "pager"; }
        { action = "sidebar-toggle-visible"; key = "B"; map = "index"; }
        { action = "sidebar-toggle-visible"; key = "B"; map = "pager"; }
        { action = "sync-mailbox"; key = "S"; map = "index"; }
        { action = "sync-mailbox"; key = "S"; map = "pager"; }
        { action = "tag-entry"; key = "<space>"; map = "index"; }
        { action = "undelete-message"; key = "U"; map = "index"; }
        { action = "view-attachments"; key = "l"; map = "pager"; }
        { action = "view-mailcap"; key = "<return>"; map = "attach"; }
        { action = "view-mailcap"; key = "l"; map = "attach"; }
        { action = "view-raw-message"; key = "H"; map = "index"; }
        { action = "view-raw-message"; key = "H"; map = "pager"; }
      ];
      extraConfig = ''
        auto_view text/html # automatically show html
        alternative_order text/plain text/enriched text/html

        # virtual-mailboxes "INBOX" "notmuch://?query=tag:inbox"
        # virtual-mailboxes "Unread" "notmuch://?query=tag:unread"
        # virtual-mailboxes "TODO" "notmuch://?query=tag:TODO"
        # virtual-mailboxes "Starred" "notmuch://?query=tag:*"
        # virtual-mailboxes "Archive" "notmuch://?query=tag:archive"
        # virtual-mailboxes "Sent" "notmuch://?query=tag:sent"
        # virtual-mailboxes "Junk" "notmuch://?query=tag:junk"
        # virtual-mailboxes "All" "notmuch://?query=*"

        # Default index colors:
        # color index yellow default '.*'
        color index_author red default '.*'
        color index_number blue default
        color index_subject cyan default '.*'

        # New mail is boldened:
        color index brightyellow black "~N"
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
        color indicator brightblack white
        color sidebar_highlight red default
        color sidebar_divider brightblack black
        color sidebar_flagged red black
        color sidebar_new green black
        color normal brightyellow default
        color error red default
        color tilde black default
        color message cyan default
        color markers red white
        color attachment white default
        color search brightmagenta default
        color status brightyellow black
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
        color body green default "\`[^\`]*\`" # Green text between ` and `
        color body brightblue default "^# \.*" # Headings as bold blue
        color body brightcyan default "^## \.*" # Subheadings as bold cyan
        color body brightgreen default "^### \.*" # Subsubheadings as bold green
        color body yellow default "^(\t| )*(-|\\*) \.*" # List items as yellow
        color body brightcyan default "[;:][-o][)/(|]" # emoticons
        color body brightcyan default "[;:][)(|]" # emoticons
        color body brightcyan default "[ ][*][^*]*[*][ ]?" # more emoticon?
        color body brightcyan default "[ ]?[*][^*]*[*][ ]" # more emoticon?
        color body red default "(BAD signature)"
        color body cyan default "(Good signature)"
        color body brightblack default "^gpg: Good signature .*"
        color body brightyellow default "^gpg: "
        color body brightyellow red "^gpg: BAD signature from.*"
        mono body bold "^gpg: Good signature"
        mono body bold "^gpg: BAD signature from.*"
        color body red default "([a-z][a-z0-9+-]*://(((([a-z0-9_.!~*'();:&=+$,-]|%[0-9a-f][0-9a-f])*@)?((([a-z0-9]([a-z0-9-]*[a-z0-9])?)\\.)*([a-z]([a-z0-9-]*[a-z0-9])?)\\.?|[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+)(:[0-9]+)?)|([a-z0-9_.!~*'()$,;:@&=+-]|%[0-9a-f][0-9a-f])+)(/([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*(;([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*)*(/([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*(;([a-z0-9_.!~*'():@&=+$,-]|%[0-9a-f][0-9a-f])*)*)*)?(\\?([a-z0-9_.!~*'();/?:@&=+$,-]|%[0-9a-f][0-9a-f])*)?(#([a-z0-9_.!~*'();/?:@&=+$,-]|%[0-9a-f][0-9a-f])*)?|(www|ftp)\\.(([a-z0-9]([a-z0-9-]*[a-z0-9])?)\\.)*([a-z]([a-z0-9-]*[a-z0-9])?)\\.?(:[0-9]+)?(/([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*(;([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*)*(/([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*(;([-a-z0-9_.!~*'():@&=+$,]|%[0-9a-f][0-9a-f])*)*)*)?(\\?([-a-z0-9_.!~*'();/?:@&=+$,]|%[0-9a-f][0-9a-f])*)?(#([-a-z0-9_.!~*'();/?:@&=+$,]|%[0-9a-f][0-9a-f])*)?)[^].,:;!)? \t\r\n<>\"]"
      '';
    };

    programs.msmtp.enable = true;
    programs.mbsync.enable = true;
    programs.notmuch.enable = false;
  };
}
