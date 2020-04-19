{ pkgs, lib, config, options, ... }:
let
  inherit (lib.strings) makeBinPath;
in
{
  imports = [
    <niveum/modules/constants.nix>
    <home-manager/nixos>
    ./alacritty.nix
    ./bash.nix
    ./bluetooth.nix
    ./ccc.nix
    ./chromium.nix
    ./cloud.nix
    ./compton.nix
    ./default.nix
    ./direnv.nix
    ./distrobump.nix
    ./docker.nix
    ./dunst.nix
    ./flix.nix
    ./fonts.nix
    ./fzf.nix
    ./git.nix
    ./hledger.nix
    ./htop.nix
    ./hu-berlin.nix
    ./i3.nix
    ./kdeconnect.nix
    ./keybase.nix
    ./keyboard.nix
    # ./mail.nix
    ./mpv.nix
    ./nano.nix
    ./neovim.nix
    ./newsboat.nix
    ./nixpkgs-unstable.nix
    ./packages
    ./printing.nix
    ./wallpaper.nix
    ./redshift.nix
    ./retiolum.nix
    ./rofi.nix
    ./ssh.nix
    ./sudo.nix
    ./sxiv.nix
    ./themes/mac-os.nix
    ./theming.nix
    ./tmux.nix
    ./todo-txt.nix
    ./traadfri.nix
    ./unclutter.nix
    # ./urxvt.nix
    ./version.nix
    ./vscode.nix
    ./xautolock.nix
    # ./xresources.nix
    ./zsh.nix
    {
      niveum.user = {
        github = "kmein";
        email = "kieran.meinhardt@gmail.com";
        name = "Kierán Meinhardt";
      };

      niveum.applications = rec {
        fileManager = "$TERMINAL -e ${pkgs.ranger}/bin/ranger";
      };

      niveum.theme = {
        gtk = { name = "Arc-Dark"; package = pkgs.arc-theme; };
        icon = { name = "Arc"; package = pkgs.arc-icon-theme; };
        cursor = { name = "capitaine-cursors-white"; package = pkgs.capitaine-cursors; };
      };
    }
    {
      nix.nixPath = [
        "/var/src"
        "nixpkgs-overlays=${toString ../overlays}"
      ];
    }
    {
      services.dbus.packages = [ pkgs.gnome3.dconf ];
    }
    {
      environment.systemPackages = [
        (pkgs.writers.writeDashBin "x-www-browser" ''
          for browser in $BROWSER firefox chromium google-chrome google-chrome-stable opera vivaldi qupzilla iceweasel konqueror firefox-aurora google-chrome-beta opera-beta vivaldi-beta google-chrome-dev opera-developer vivaldi-snapshot luakit midori epiphany lynx w3m dillo elinks vimb; do
            if command -v $browser > /dev/null 2>&1; then
              exec $browser "$@"
            fi
          done
          exit 1
        '')
      ];
    }
    {
      nixpkgs = {
        config = {
          allowUnfree = true;
          packageOverrides = pkgs: {
            nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
              inherit pkgs;
            };
          };
        };
        overlays = [
          # (import <stockholm/submodules/nix-writers>)
          (import <niveum/overlays/toml.nix>)
          (import <niveum/overlays/scripts.nix>)
          (import <niveum/overlays/rust.nix>)
          (self: super: {
            writeDashBin = super.writers.writeDashBin;
            writeDash = super.writers.writeDash;

            ix = super.callPackage <niveum/packages/ix.nix> {};

            iolanguage = super.callPackage <niveum/packages/iolanguage.nix> {};
            gfs-fonts = super.callPackage <niveum/packages/gfs-fonts.nix> {
              scardanelli = config.networking.hostName == "scardanelli";
            };
          })
        ];
      };
    }
    {
      boot.cleanTmpDir = true;
      boot.loader.timeout = 1;
      boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
    }
    {
      time.timeZone = "Europe/Berlin";
      location = {
        latitude = 52.517;
        longitude = 13.3872;
      };
    }
    {
      home-manager.users.me = {
        programs.zathura = {
          enable = true;
          options = {
            selection-clipboard = "clipboard";
            # first-page-column = "1:1"; # makes side-by-side mode start on the left side
          };
        };
      };
    }
    {
      users.mutableUsers = false;

      users.defaultUserShell = pkgs.zsh;

      users.users.me = {
        name = "kfm";
        description = config.niveum.user.name;
        hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
        isNormalUser = true;
      };
    }
    {
      sound.enable = true;

      hardware.pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull; # for bluetooth sound output
      };

      users.users.me.extraGroups = [ "audio" ];

      environment.systemPackages = [ pkgs.pavucontrol pkgs.pamixer ];
    }
    {
      environment.interactiveShellInit = "export PATH=$PATH:$HOME/projects/niveum";
      environment.shellAliases =
      let
        wcd = pkgs.writers.writeDash "wcd" ''
          cd "$(readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname)/.."
        '';
        where = pkgs.writers.writeDash "where" ''
          readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname
        '';
        take = pkgs.writers.writeDash "take" ''
          mkdir "$1" && cd "$1"
        '';
      in {
        cat = "${pkgs.bat}/bin/bat --style=plain";
        chromium-incognito = "chromium --user-data-dir=$(mktemp -d /tmp/chr.XXXXXX) --no-first-run --incognito";
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard -in";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -out";
        cp = "cp -i";
        dig = "dig +short";
        ip = "${pkgs.iproute}/bin/ip -c";
        l = "${pkgs.exa}/bin/exa -s type -a";
        la = "${pkgs.exa}/bin/exa -s type -la";
        ll = "${pkgs.exa}/bin/exa -s type -l";
        ls = "${pkgs.exa}/bin/exa -s type";
        mv = "mv -i";
        nixi = "nix repl '<nixpkgs>'";
        ns = "nix-shell --run zsh";
        s = "${pkgs.systemd}/bin/systemctl";
        us = "${pkgs.systemd}/bin/systemctl --user";
        o = "${pkgs.xdg_utils}/bin/xdg-open";
        rm = "rm -i";
        take = "source ${take}";
        tmux = "${pkgs.tmux}/bin/tmux -2";
        yt = "${pkgs.youtube-dl}/bin/youtube-dl --add-metadata -ic"; # Download video link
        yta = "${pkgs.youtube-dl}/bin/youtube-dl --add-metadata -xic"; # Download with audio
        tree = "${pkgs.exa}/bin/exa --tree";
        wcd = "source ${wcd}";
        weechat = "${pkgs.openssh}/bin/ssh kmein@prism.r -t tmux attach";
        where = "source ${where}";
      };
    }
    {
      networking.wireless = {
        enable = true;
        userControlled.enable = true;
        networks = {
          "Aether" = {
            pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
            priority = 10;
          };
          "Asoziales Netzwerk" = {
            pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6";
            priority = 10;
          };
          "Libertarian WiFi" = {
            pskRaw = "e9beaae6ffa55d10e80b8a2e7d997411d676a3cc6f1f29d0b080391f04555050";
            priority = 9;
          };
          "EasyBox-927376".pskRaw = "dbd490ab69b39bd67cfa06daf70fc3ef3ee90f482972a668ed758f90f5577c22";
          "FlixBus Wi-Fi" = {};
          "FlixBus" = {};
          "FlixTrain" = {};
          "BVG Wi-Fi" = {};
          "Ni/Schukajlow".pskRaw = "ffc47f6829da59c48aea878a32252223303f5c47a3859edc90971ffc63346781";
          "WIFIonICE" = {};
          "WLAN-914742".psk = "67647139648174545446";
          "KDG-4ECF7".psk = "Gdbwh7afw2Bx";
          "WLAN-XVMU6T".pskRaw = "46ea807283255a3d7029233bd79c18837df582666c007c86a8d591f65fae17cc";
          "c-base-public" = {};
          "discord".psk = "baraustrinken";
          "GoOnline".psk = "airbnbguest";
          "security-by-obscurity".psk = "44629828256481964386";
          "Mayflower".psk = "Fr31EsLan";
        };
      };

      environment.systemPackages = [ pkgs.wpa_supplicant_gui ];
    }
    {
      networking.hosts = {
        "192.168.178.1" = [ "fritz.box" ];
        "192.168.178.21" = [ "scardanelli" ];
        "192.168.178.22" = [ "homeros" ];
        "192.168.178.24" = [ "catullus" ];
      };
    }
    {
      i18n.defaultLocale = "en_GB.UTF-8";
    }
    {
      services.illum.enable = true;
    }
    {
      services.xserver = {
        enable = true;
        displayManager.lightdm = {
          enable = true;
          autoLogin = {
            enable = true;
            user = config.users.users.me.name;
          };
          greeters.gtk = {
            enable = true;
            indicators = [ "~spacer" "~host" "~spacer" "~session" "~power" ];
          };
        };
        desktopManager.default = "none";
      };
    }
    {
      security.wrappers = {
        pmount.source = "${pkgs.pmount}/bin/pmount";
        pumount.source = "${pkgs.pmount}/bin/pumount";
      };
    }
    {
      programs.command-not-found.enable = true;
    }
    {
      systemd.services.restart-vpn = {
        description = "Restart VPNs after suspend";
        wantedBy = [ "suspend.target" ];
        after = [ "suspend.target" ];
        serviceConfig.Type = "oneshot";
        script = ''
          set -efu

          export PATH=${makeBinPath [ pkgs.procps ]}

          pkill -HUP --exact openvpn
          pkill -ALRM --exact tincd
        '';
      };
    }
    {
      environment.systemPackages = [ pkgs.neomutt ];
      home-manager.users.me.xdg.configFile."neomutt/neomuttrc".text = ''
        set mailcap_path = ${pkgs.writeText "mailcap" ''
          text/plain; $EDITOR %s ;
          text/html; ${pkgs.lynx} -assume_charset=%{charset} -display_charset=utf-8 -dump %s; nametemplate=%s.html; copiousoutput;
          image/*; ${pkgs.sxiv}/bin/sxiv %s ;
          video/*; ${pkgs.utillinux}/bin/setsid ${pkgs.mpv}/bin/mpv --quiet %s &; copiousoutput
          application/*; ${pkgs.xdg_utils}/bin/xdg-open %s ;
        ''}

        set sort = reverse-threads
        set sleep_time = 0		# Pause 0 seconds for informational messages
        set markers = no		# Disables the `+` displayed at line wraps
        set mark_old = no		# Unread mail stay unread until read
        set mime_forward = yes		# attachments are forwarded with mail
        set wait_key = no		# mutt won't ask "press key to continue"
        set fast_reply			# skip to compose when replying
        set fcc_attach			# save attachments with the body
        set forward_format = "Fwd: %s"	# format of subject when forwarding
        set forward_quote		# include message in forwards
        set reverse_name		# reply as whomever it was to
        set include = no		# dont include message in replies
        set mail_check=60 # to avoid lags using IMAP with some email providers (yahoo for example)
        auto_view text/html		# automatically show html (mailcap uses w3m)
        alternative_order text/plain text/enriched text/html

        source ${pkgs.neomutt}/share/doc/neomutt/vim-keys/vim-keys.rc
        source ${pkgs.writeText "vim-keys.rc" ''
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
          bind index,pager H view-raw-message
          bind browser l select-entry
          bind pager,browser gg top-page
          bind pager,browser G bottom-page
          # bind index,pager,browser d half-down
          # bind index,pager,browser u half-up
          bind index,pager S sync-mailbox
          bind index,pager R group-reply
          bind index \031 previous-undeleted	# Mouse wheel
          bind index \005 next-undeleted		# Mouse wheel
          bind pager \031 previous-line		# Mouse wheel
          bind pager \005 next-line		# Mouse wheel
          bind editor <Tab> complete-query

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
        ''}

        source ${pkgs.writeText "colours.rc" ''
          # Default index colors:
          # color index yellow default '.*'
          color index_author red default '.*'
          color index_number blue default
          color index_subject cyan default '.*'

          # New mail is boldened
          color index brightyellow black "~N"
          color index_author brightred black "~N"
          color index_subject brightcyan black "~N"

          # Tagged mail is highlighted
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
        ''}

        set index_format="%2C %Z %?X?A& ? %D %-15.15F %s (%-4.4c)"
        set smtp_authenticators = 'gssapi:login'

        source ${
          let emailUser = "kieran.meinhardt";
          in pkgs.writeText "gmail.rc" ''
            set from="${emailUser}@gmail.com"
            set realname="${config.niveum.user.name}"

            set imap_login="${emailUser}@gmail.com"
            set imap_pass="`${pkgs.pass}/bin/pass mail/${emailUser}@gmail.com`"

            set smtp_url="smtps://${emailUser}@gmail.com@smtp.gmail.com:465/"
            set smtp_pass="$imap_pass"

            set folder=imaps://${emailUser}@imap.gmail.com/
            set spoolfile="+INBOX"
            set record="+[Gmail]/Gesendet"
            set postponed="+[Gmail]/Entwürfe"
            set mbox="+[Gmail]/Archiv"

            mailboxes =INBOX, =[Gmail]/Gesendet, =[Gmail]/Entwürfe, =[Gmail]/Archiv
        ''}
      '';
    }
  ];
}
