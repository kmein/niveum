{ pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran nixpkgs-unstable;
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
        scripts = [ pkgs.weechatScripts.weechat-autosort nixpkgs-unstable.weechatScripts.colorize_nicks ];
        init = ''
          /set irc.server_default.nicks "kmein,kfm,kieran"
          /set irc.server_default.msg_part "tschö mit ö"
          /set irc.server_default.msg_quit "ciao kakao"
          /set irc.server_default.msg_kick "warum machst du diese?"
          /set irc.server_default.realname "${kieran.name}"

          /server add hackint irc.hackint.org/6697 -ipv6 -ssl -autoconnect
          /server add freenode chat.freenode.org
          /server add irc.r irc.r
          /server add news.r news.r

          /alias add mod /quote omode $channel +o $nick

          /set irc.server.freenode.autojoin "#krebs,#flipdot,##myengadin"
          /set irc.server.irc.r.autojoin "#xxx,#brockman"
          /set irc.server.news.r.autojoin "#drachengame,#memes,#all,#berlin"
          /set logger.level.irc.news.r.#all 0

          /filter addreplace corona irc.news.r.* * [kc]orona|[kc]ovid|virus|lockdown|va[kc][sc]in|mutante|mutation|impf|pandemi

          /set irc.look.server_buffer independent

          /connect freenode
          /connect irc.r
          /connect news.r
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
    openssh.authorizedKeys.keys = kieran.sshKeys pkgs;
    createHome = true;
    group = "weechat";
    home = "/var/lib/weechat";
    isSystemUser = true;
    packages = [ pkgs.tmux ];
  };
}
