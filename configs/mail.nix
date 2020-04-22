{ config, pkgs, lib, ... }:
let
  pass = id: "${pkgs.pass}/bin/pass ${id}";
in
{
  environment.systemPackages = [ pkgs.neomutt ];

  home-manager.users.me = {
    accounts.email.maildirBasePath = "${config.users.users.me.home}/mail";
    accounts.email.accounts = {
      kieran-gmail = {
        primary = true;
        flavor = "gmail.com";
        address = "kieran.meinhardt@gmail.com";
        realName = config.niveum.user.name;
        userName = "kieran.meinhardt";
        passwordCommand = pass "mail/kieran.meinhardt@gmail.com";
        neomutt.enable = true;
        mbsync = {
          enable = true;
          create = "both";
        };
        msmtp.enable = true;
      };
      hu-berlin = {
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
        neomutt.enable = true;
        mbsync = {
          enable = true;
          create = "both";
        };
        msmtp.enable = true;

        # notmuch.enable = true;
      };
    };

    programs.neomutt = {
      enable = true;
      sidebar.enable = true;
      sort = "threads"; # or reverse-date
      vimKeys = true;
      checkStatsInterval = 60;
      settings = {
        date_format = "\"%Y-%m-%d %H:%M\"";
        mime_forward = "yes"; # forward attachments with mail
        fast_reply = "yes"; # skip to compose when forwarding
        forward_format = "\"Fwd: %s\""; # format of subject when forwarding
        forward_quote = "yes"; # include message in forwards
        reverse_name = "yes"; # reply as whomever it was to
        include = "no"; # dont include message in replies
        mailcap_path = toString (pkgs.writeText "mailcap" ''
          text/html; ${pkgs.elinks}/bin/elinks -dump; copiousoutput;
        '');
      };
      # binds = [ {map = null; key = null; action = null; }];
      extraConfig = ''
        # Default index colors:
        color index yellow default '.*'
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

    /*
    programs.notmuch.enable = true;

    home.file = {
      ".muttrc".text = ''
        # read html mail
        auto_view text/html
        set mailcap_path = ${pkgs.writeText "mailcap" "text/html; ${pkgs.elinks}/bin/elinks -dump ; copiousoutput;"}

        set nm_default_uri="notmuch://$HOME/Maildir"
        set nm_record = yes
        set nm_record_tags = "-inbox me archive"
        set virtual_spoolfile=yes

        set sendmail=${pkgs.writers.writeDash "msmtp" ''
          ${pkgs.coreutils}/bin/tee >(${pkgs.notmuch}/bin/notmuch insert --create-folder +sent) \
            | ${pkgs.msmtp}/bin/msmtpq "$@"
        ''}

        # This file contains all of mutt-wizard's default settings.
        # mutt-wizard will have this file sourced from your muttrc.
        # In the interest of seamless updating, do not edit this file.
        # If you want to override any settings, set those in your muttrc.
        set date_format="%y-%m-%d %I:%M%p"
        set index_format="%2C %Z %?X?A& ? %D %-15.15F %s (%-4.4c)"
        set sort = 'reverse-date'
        set rfc2047_parameters = yes
        set sleep_time = 0      # Pause 0 seconds for informational messages
        set markers = no        # Disables the `+` displayed at line wraps
        set mark_old = no       # Unread mail stay unread until read
        set mime_forward = yes      # attachments are forwarded with mail
        set wait_key = no       # mutt won't ask "press key to continue"
        set fast_reply          # skip to compose when replying
        set fcc_attach          # save attachments with the body
        set forward_format = "Fwd: %s"  # format of subject when forwarding
        set forward_quote       # include message in forwards
        set reverse_name        # reply as whomever it was to
        set include         # include message in replies
        set mail_check=60 # to avoid lags using IMAP with some email providers (yahoo for example)
        auto_view text/html     # automatically show html (mailcap uses w3m)
        auto_view application/pgp-encrypted
        alternative_order text/plain text/enriched text/html
        bind index,pager i noop
        bind index,pager g noop
        bind index \Cf noop

        # General rebindings
        bind index j next-entry
        bind index k previous-entry
        bind attach <return> view-mailcap
        bind attach l view-mailcap
        bind editor <space> noop
        bind index G last-entry
        bind index gg first-entry
        bind pager,attach h exit
        bind pager j next-line
        bind pager k previous-line
        bind pager l view-attachments
        bind index D delete-message
        bind index U undelete-message
        bind index L limit
        bind index h noop
        bind index l display-message
        bind index <space> tag-entry
        #bind browser h goto-parent
        macro browser h '<change-dir><kill-line>..<enter>' "Go to parent folder"
        bind index,pager H view-raw-message
        bind browser l select-entry
        bind pager,browser gg top-page
        bind pager,browser G bottom-page
        bind index,pager,browser d half-down
        bind index,pager,browser u half-up
        bind index,pager S sync-mailbox
        bind index,pager R group-reply
        bind index \031 previous-undeleted  # Mouse wheel
        bind index \005 next-undeleted      # Mouse wheel
        bind pager \031 previous-line       # Mouse wheel
        bind pager \005 next-line       # Mouse wheel
        bind editor <Tab> complete-query

        macro index \Cr "T~U<enter><tag-prefix><clear-flag>N<untag-pattern>.<enter>" "mark all messages as read"
        macro index O "<shell-escape>mbsync -a<enter>" "run mbsync to sync all mail"
        macro index \Cf "<enter-command>unset wait_key<enter><shell-escape>read -p 'Enter a search term to find with notmuch: ' x; echo \$x >~/.cache/mutt_terms<enter><limit>~i \"\`notmuch search --output=messages \$(cat ~/.cache/mutt_terms) | head -n 600 | perl -le '@a=<>;s/\^id:// for@a;$,=\"|\";print@a' | perl -le '@a=<>; chomp@a; s/\\+/\\\\+/ for@a;print@a' \`\"<enter>" "show only messages matching a notmuch pattern"
        macro index A "<limit>all\n" "show all messages (undo limit)"

        # Sidebar mappings
        set sidebar_visible = yes
        set sidebar_width = 20
        set sidebar_short_path = yes
        set sidebar_next_new_wrap = yes
        set mail_check_stats
        set sidebar_format = '%B%?F? [%F]?%* %?N?%N/? %?S?%S?'
        bind index,pager \Ck sidebar-prev
        bind index,pager \Cj sidebar-next
        bind index,pager \Co sidebar-open
        bind index,pager \Cp sidebar-prev-new
        bind index,pager \Cn sidebar-next-new
        bind index,pager B sidebar-toggle-visible

        # Default index colors:
        color index yellow default '.*'
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
    */
  };
}
