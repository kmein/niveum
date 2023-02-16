{
  lib,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) kieran;
  relayPassword = lib.fileContents <system-secrets/weechat/relay>;
  weechatHome = "/var/lib/weechat";
in {
  systemd.services.weechat = let
    tmux = pkgs.writers.writeDash "tmux" ''
      exec ${pkgs.tmux}/bin/tmux -f ${
        pkgs.writeText "tmux.conf" ''
          set-option -g prefix `
          unbind-key C-b
          bind ` send-prefix

          set-option -g status off
          set-option -g default-terminal screen-256color

          #use session instead of windows
          bind-key c new-session
          bind-key p switch-client -p
          bind-key n switch-client -n
          bind-key C-s switch-client -l
        ''
      } "$@"
    '';
    weechat = pkgs.weechat-declarative.override {
      config = {
        scripts = [
          pkgs.weechatScripts.weechat-autosort
          pkgs.weechatScripts.colorize_nicks
          pkgs.weechatScripts.weechat-matrix
          (pkgs.callPackage <niveum/packages/weechatScripts/hotlist2extern.nix> {})
        ];
        settings = let
          nick = "kmein";
        in {
          weechat = {
            look.mouse = true;
            look.prefix_align_max = 15;
            color.chat_nick_colors = lib.lists.subtractLists (lib.range 52 69 ++ lib.range 231 248) (lib.range 31 254);
          };
          irc = {
            look = {
              server_buffer = "independent";
              color_nicks_in_nicklist = true;
            };
            server_default = {
              nicks = nick;
              msg_part = "tschö mit ö";
              msg_quit = "ciao kakao";
              msg_kick = "warum machst du diese?";
              realname = lib.head (lib.strings.split " " kieran.name);
            };
            server = {
              hackint = {
                autoconnect = true;
                addresses = "irc.hackint.org/6697";
                ipv6 = true;
                ssl = true;
                autojoin = ["#eloop" "#krebs" "#hsmr" "#hsmr-moin" "#nixos" "#the_playlist" "#flipdot-berlin" "#hackint"];
                sasl_mechanism = "plain";
                sasl_username = nick;
                sasl_password = lib.strings.fileContents <system-secrets/irc/hackint>;
              };
              libera = {
                autoconnect = true;
                addresses = "irc.libera.chat/6697";
                ssl = true;
                autojoin = ["#flipdot" "#haskell" "#nixos" "#fysi" "#binaergewitter" "#vim" "#newsboat"];
                sasl_mechanism = "plain";
                sasl_username = nick;
                sasl_password = lib.strings.fileContents <system-secrets/irc/libera>;
              };
              oftc = {
                autoconnect = true;
                addresses = "irc.oftc.net/6697";
                ssl = true;
                ipv6 = true;
                command = lib.concatStringsSep "\\;" [
                  "/msg nickserv identify ${lib.strings.fileContents <system-secrets/irc/oftc>}"
                  "/msg nickserv set cloak on"
                ];
                autojoin = ["#home-manager"];
              };
              retiolum = {
                autoconnect = true;
                addresses = "irc.r";
                autojoin = ["#xxx" "#brockman" "#flix"];
                command = lib.concatStringsSep "\\;" [
                  "/oper admin aidsballs"
                  "/msg nickserv always-on true"
                  "/msg nickserv autoreplay-missed on"
                  "/msg nickserv auto-away"
                ];
                sasl_mechanism = "plain";
                sasl_username = nick;
                sasl_password = lib.strings.fileContents <system-secrets/irc/retiolum>;
              };
              news = {
                autoconnect = true;
                addresses = "news.r";
                autojoin = ["#cook" "#drachengame" "#oepnv" "#kmeinung" "#memes"];
                command = "/oper aids balls";
                sasl_mechanism = "plain";
                sasl_username = nick;
                sasl_password = nick + nick;
              };
            };
          };
          logger.level.irc.news = 0;
          plugins.var.perl.hotlist2extern = {
            external_command_hotlist = "echo %X > ${weechatHome}/hotlist.txt";
            external_command_hotlist_empty = "echo -n %X > ${weechatHome}/hotlist.txt";
            lowest_priority = "2";
            use_title = "off";
            delimiter = ",";
          };
          matrix.server.nibbana = {
            address = "nibbana.jp";
            username = nick;
            password = lib.strings.fileContents <system-secrets/matrix/nibbana>;
            autoconnect = true;
          };
          alias.cmd.mod = "/quote omode $channel +o $nick";
          relay = {
            port.weechat = 9000;
            network.password = relayPassword;
          };
          filters = {
            zerocovid = {
              buffer = "irc.news.*";
              tags = "*";
              regex = "[kc]orona|💉|🤒|😷|[kc]ovid|virus|lockdown|va[kc][sc]in|vaxx|mutante|mutation|impf|pandemi|κορ[ωο]ν[αο]ϊό|корона|expert|infe[ck]t|infizi|in[cz]iden[cz]|sars-cov|drosten|virolog|lauterbach|delta|omi[ck]ron|epidemi|booster|r-wert";
            };
            smart = {
              buffer = "*";
              tags = "irc_smart_filter";
              regex = "*";
            };
            playlist_topic = {
              buffer = "irc.*.#the_playlist";
              tags = "irc_topic";
              regex = "*";
            };
            brockman_notice = {
              buffer = "irc.news.*";
              tags = "irc_notice";
              regex = "*";
            };
            bots = {
              buffer = "irc.retiolum.*";
              tags = ["nick_gitlab"];
              regex = "*";
            };
            people = {
              buffer = "irc.*.*";
              tags = map (name: "nick_${name}") ["mod_p[matrix-fli"];
              regex = "*";
            };
          };
        };
        extraCommands = ''
          /save
          /matrix connect nibbana
          /connect -all
        '';
      };
    };
  in {
    description = "Weechat bouncer";
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    restartIfChanged = true;
    path = [pkgs.alacritty.terminfo];
    environment.WEECHAT_HOME = weechatHome;
    preStart = "${pkgs.coreutils}/bin/rm $WEECHAT_HOME/*.conf";
    script = "${tmux} -2 new-session -d -s IM ${weechat}/bin/weechat";
    preStop = "${tmux} kill-session -t IM";
    serviceConfig = {
      User = "weechat";
      Group = "weechat";
      RemainAfterExit = true;
      Type = "oneshot";
    };
  };

  users.groups.weechat = {};
  users.extraUsers.weechat = {
    useDefaultShell = true;
    openssh.authorizedKeys.keys =
      kieran.sshKeys pkgs
      ++ [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC+KVDmYYH7mA8v81e9O3swXm3ZVYY9t4HP65ud61uXy weechat_android@heym"
      ];
    createHome = true;
    group = "weechat";
    home = "/var/lib/weechat";
    isSystemUser = true;
    packages = [pkgs.tmux];
  };

  niveum.passport.services = [
    {
      title = "weechat bouncer";
      description = "keeps me logged in on IRC.";
    }
  ];
}
