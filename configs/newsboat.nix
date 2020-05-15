{ pkgs, config, lib, ... }:
let
  newsboat-home = "${config.users.users.me.home}/cloud/syncthing/common/newsboat";
  scripts = import <niveum/packages/scripts> { inherit pkgs lib; };
  linkhandler-bin = "${scripts.linkhandler}/bin/linkhandler";

  newsboat-config = pkgs.writeText "config" ''
    auto-reload no
    prepopulate-query-feeds yes

    # dont keep a search history
    history-limit 0

    datetime-format %F

    text-width 85

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

    highlight article "^Title:.*" yellow black bold
    highlight article "^Author:.*" yellow black
    highlight article "^Flags:.*" red black
    highlight article "\\[[0-9][0-9]*\\]" color66 default bold
    highlight article "\\[image [0-9][0-9]*\\]" color109 default bold
    highlight article "\\[embedded flash: [0-9][0-9]*\\]" color66 default bold

    color background white black
    color listnormal white black
    color listnormal_unread white black bold
    color listfocus blue black
    color listfocus_unread blue black bold
    color info red black bold
    color article white black
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
