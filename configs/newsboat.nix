{
  pkgs,
  config,
  lib,
  ...
}: let
  scripts = import <niveum/packages/scripts> {inherit pkgs lib;};

  ytdl-format = "'bestvideo[height<=?720][fps<=?30][vcodec!=?vp9]+bestaudio/best'";

  youtube-download = "${pkgs.ts}/bin/ts ${pkgs.yt-dlp}/bin/yt-dlp -f ${ytdl-format} --add-metadata";

  newsboat-home = "${config.users.users.me.home}/cloud/Seafile/Documents/newsboat";
  linkhandler-bin = "${scripts.linkhandler}/bin/linkhandler";

  newsboat-config = pkgs.writeText "config" ''
    auto-reload no
    reload-threads 8
    prepopulate-query-feeds yes

    # dont keep a search history
    history-limit 0

    datetime-format %F

    text-width 85

    external-url-viewer "${pkgs.urlscan}/bin/urlscan -dc -r '${linkhandler-bin} {}'"
    browser ${linkhandler-bin}
    macro , open-in-browser
    macro c set browser "${pkgs.xsel}/bin/xsel -b <<<" ; open-in-browser ; set browser ${linkhandler-bin}
    macro v set browser "${pkgs.utillinux}/bin/setsid -f ${pkgs.mpv}/bin/mpv" ; open-in-browser ; set browser ${linkhandler-bin}
    macro y set browser "${youtube-download}" ; open-in-browser ; set browser ${linkhandler-bin}

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

    highlight all "---.*---" yellow default
    # highlight feedlist ".*(0/0))" default default
    highlight article "^Title:.*" yellow default bold
    highlight article "^Author:.*" yellow default
    highlight article "^Flags:.*" red default
    highlight article "\\[[0-9][0-9]*\\]" color66 default bold
    highlight article "\\[image [0-9][0-9]*\\]" color109 default bold
    highlight article "\\[embedded flash: [0-9][0-9]*\\]" color66 default bold

    color listfocus blue default
    color listfocus_unread blue default bold
    color info red default bold
  '';

  newsboat-sql = "${pkgs.sqlite}/bin/sqlite3 ${newsboat-home}/cache.db";
in {
  nixpkgs.config.packageOverrides = pkgs: {
    newsboat = pkgs.writers.writeDashBin "newsboat" ''
      ${pkgs.newsboat}/bin/newsboat -C ${newsboat-config} -c ${newsboat-home}/cache.db -u ${newsboat-home}/urls "$@"
    '';
  };

  environment.systemPackages = [
    pkgs.newsboat
    (pkgs.writers.writeDashBin "newsboat-unread-count" ''
      if [ -f ${newsboat-home}/cache.db.lock ]; then
        ${pkgs.jq}/bin/jq -n '{state: "Info", text: "↻", icon: "rss"}'
      else

        ${pkgs.jq}/bin/jq -n \
          --argjson unread "$(${newsboat-sql} "SELECT COUNT(DISTINCT id) FROM rss_item WHERE unread=1")" \
          --argjson watchLater "$(${newsboat-sql} "SELECT COUNT(DISTINCT id) FROM rss_item WHERE flags='e' AND deleted=0")" \
          '{
            state: (if $unread > 0 then "Good" else "Idle" end),
            text: (if $unread > 0 then "\($unread)" else "[\($watchLater)]" end),
            icon: "rss"
          }'
      fi
    '')
    (pkgs.writers.writeDashBin "mpv-watch-later" ''
      ${newsboat-sql} "SELECT url FROM rss_item WHERE flags='e' AND deleted=0 ORDER BY pubDate DESC" \
        | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv
    '')
  ];
}
