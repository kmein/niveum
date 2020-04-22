{ pkgs, config, lib, ... }:
let
  newsboat-home = "${config.users.users.me.home}/cloud/syncthing/common/newsboat";
  scripts = import <niveum/packages/scripts> { inherit pkgs lib; };
  linkhandler-bin = "${scripts.linkhandler}/bin/linkhandler";

  newsboat-config = pkgs.writeText "config" ''
    auto-reload yes

    external-url-viewer "${pkgs.urlscan}/bin/urlscan -dc -r '${linkhandler-bin} {}'"
    browser ${linkhandler-bin}

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
  '';
in
{
  nixpkgs.config.packageOverrides = pkgs: {
    newsboat = pkgs.writers.writeDashBin "newsboat" ''
      ${pkgs.newsboat}/bin/newsboat -C ${newsboat-config} -c ${newsboat-home}/cache.db -u ${newsboat-home}/urls "$@"
    '';
  };

  environment.systemPackages = [ pkgs.newsboat ];
}
