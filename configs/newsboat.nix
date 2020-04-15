{ pkgs, ... }:
let
  newsboat-home = "~/cloud/syncthing/common/newsboat";
  linkhandler-bin = "${pkgs.scripts.linkhandler}/bin/linkhandler";
in
{
  environment.shellAliases.newsboat = "${pkgs.newsboat}/bin/newsboat -u ${newsboat-home}/urls";

  home-manager.users.me = {
    programs.newsboat = {
      enable = true;
      extraConfig = ''
        auto-reload yes

        external-url-viewer "${pkgs.urlscan}/bin/urlscan -dc -r '${linkhandler-bin} {}'"

        bind-key j down
        bind-key k up
        bind-key j next articlelist
        bind-key k prev articlelist
        bind-key J next-feed articlelist
        bind-key K prev-feed articlelist
        bind-key G end
        bind-key g home
        bind-key d pagedown
        bind-key u pageup
        bind-key l open
        bind-key h quit
        bind-key a toggle-article-read
        bind-key n next-unread
        bind-key N prev-unread
        bind-key D pb-download
        bind-key U show-urls
        bind-key x pb-delete

        save-path ${newsboat-home}/saved/

        color listnormal cyan default
        color listfocus black yellow standout bold
        color listnormal_unread blue default
        color listfocus_unread yellow default bold
        color info red black bold

        browser ${linkhandler-bin}/bin/linkhandler
        macro , open-in-browser
        macro v set browser "${pkgs.utillinux}/bin/setsid ${pkgs.coreutils}/bin/nohup ${pkgs.mpv}/bin/mpv"; open-in-browser ; set browser ${linkhandler-bin}
        macro w set browser "${pkgs.w3m}/bin/w3m"; open-in-browser ; set browser ${linkhandler-bin}
      '';
    };
  };
}
