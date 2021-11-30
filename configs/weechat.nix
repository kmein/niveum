{ lib, pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran;
  relayPassword = lib.fileContents <system-secrets/weechat/relay>;
in {
  systemd.services.weechat =
  let
    tmux = pkgs.writers.writeDash "tmux" ''
      exec ${pkgs.tmux}/bin/tmux -f ${pkgs.writeText "tmux.conf" ''
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
      ''} "$@"
    '';
    weechat = pkgs.weechat.override {
      configure = { ... }: {
        scripts = [ pkgs.weechatScripts.weechat-autosort pkgs.weechatScripts.colorize_nicks ];
        init = let
          coolColors = lib.lists.subtractLists (lib.range 52 69 ++ lib.range 231 248) (lib.range 31 254);
          nick = "kmein";
        in ''
          /mouse enable
          /set irc.server_default.nicks "${nick}"
          /set irc.server_default.msg_part "tschö mit ö"
          /set irc.server_default.msg_quit "ciao kakao"
          /set irc.server_default.msg_kick "warum machst du diese?"
          /set irc.server_default.realname "${kieran.name}"

          /set irc.look.color_nicks_in_nicklist "on"
          /set weechat.color.chat_nick_colors "${lib.concatMapStringsSep "," toString coolColors}"

          /server add hackint irc.hackint.org/6697 -ipv6 -ssl
          /server add libera irc.libera.chat/6697 -ssl
          /server add oftc irc.oftc.net/6697 -ssl -ipv6
          /server add retiolum irc.r
          /server add news news.r

          /alias add mod /quote omode $channel +o $nick

          /relay add weechat 9000
          /set relay.network.password ${relayPassword}

          /set irc.server.oftc.command /msg nickserv IDENTIFY ${lib.strings.fileContents <system-secrets/irc/oftc>};/msg nickserv SET CLOAK ON
          /set irc.server.oftc.autojoin "#osm,#osm-de"

          /set irc.server.hackint.autojoin "#krebs,#nixos,#the_playlist"
          /set irc.server.hackint.sasl_mechanism plain
          /set irc.server.hackint.sasl_username ${nick}
          /set irc.server.hackint.sasl_password ${lib.strings.fileContents <system-secrets/irc/hackint>}

          /set irc.server.libera.autojoin "#flipdot,#haskell,#nixos,#fysi,#binaergewitter"
          /set irc.server.libera.sasl_mechanism plain
          /set irc.server.libera.sasl_username ${nick}
          /set irc.server.libera.sasl_password ${lib.strings.fileContents <system-secrets/irc/libera>}

          /set irc.server.retiolum.autojoin "#xxx,#brockman,#flix,#autowifi"
          /set irc.server.retiolum.command "/oper aids balls"
          /set irc.server.news.autojoin "#cook,#drachengame,#oepnv,#kmeinung,#memes"
          /set irc.server.news.command "/oper aids balls"
          /set logger.level.irc.news 0

          /filter addreplace zerocovid * * [kc]orona|💉|🤒|😷|[kc]ovid|virus|lockdown|va[kc][sc]in|mutante|mutation|impf|pandemi|κορ[ωο]ν[αο]ϊό|корона|expert|infe[ck]t|infizi|in[cz]iden[cz]|sars-cov|drosten|virolog|lauterbach|delta|omi[ck]ron
          /filter addreplace joinquit * irc_join,irc_part,irc_quit,irc_nick *
          /filter addreplace playlist_topic irc.*.#the_playlist irc_topic *
          /filter addreplace brockman_notice irc.news.* irc_notice *

          /set irc.look.server_buffer independent

          /connect libera
          /connect oftc
          /connect hackint
          /connect retiolum
          /connect news
        '';
      };
    };
  in {
    description = "Weechat bouncer";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    restartIfChanged = true;
    path = [ pkgs.alacritty.terminfo ];
    environment.WEECHAT_HOME = "/var/lib/weechat";
    script = "${tmux} -2 new-session -d -s IM ${weechat}/bin/weechat";
    preStop = "${tmux} kill-session -t IM";
    serviceConfig = {
      User = "weechat";
      RemainAfterExit = true;
      Type = "oneshot";
    };
  };

  users.groups.weechat = {};
  users.extraUsers.weechat = {
    useDefaultShell = true;
    openssh.authorizedKeys.keys = kieran.sshKeys pkgs ++ [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC+KVDmYYH7mA8v81e9O3swXm3ZVYY9t4HP65ud61uXy weechat_android@heym"
    ];
    createHome = true;
    group = "weechat";
    home = "/var/lib/weechat";
    isSystemUser = true;
    packages = [ pkgs.tmux ];
  };
}
