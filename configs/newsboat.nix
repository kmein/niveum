{ pkgs, ... }:
{
  environment.shellAliases.newsboat = "${pkgs.newsboat}/bin/newsboat -u ~/cloud/syncthing/common/urls";

  home-manager.users.me = {
    programs.newsboat = {
      enable = true;
      extraConfig = ''
        auto-reload yes

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

        color listnormal cyan default
        color listfocus black yellow standout bold
        color listnormal_unread blue default
        color listfocus_unread yellow default bold
        color info red black bold
        color article cyan default
      '';
    };
  };
}
