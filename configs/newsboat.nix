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
  linkhandler = pkgs.writers.writeDash "linkhandler" ''
    # Feed script a url or file location.
    # If an image, it will view in sxiv,
    # if a video or gif, it will view in mpv
    # if a music file or pdf, it will download,
    # otherwise it opens link in browser.

    # If no url given. Opens browser. For using script as $BROWSER.
    [ -z "$1" ] && { "$BROWSER"; exit; }

    case "$1" in
        *mkv|*webm|*mp4|*youtube.com/watch*|*youtube.com/playlist*|*youtu.be*|*bitchute.com*|*videos.lukesmith.xyz*|*odysee.com*)
            setsid -f ${pkgs.mpv}/bin/mpv -quiet "$1" >/dev/null 2>&1 ;;
        *png|*jpg|*jpe|*jpeg|*gif)
            curl -sL "$1" > "/tmp/$(echo "$1" | sed "s/.*\///")" && sxiv -a "/tmp/$(echo "$1" | sed "s/.*\///")"  >/dev/null 2>&1 & ;;
        *mp3|*flac|*opus|*mp3?source*)
            setsid -f tsp curl -LO "$1" >/dev/null 2>&1 ;;
        *)
            if [ -f "$1" ]; then "$TERMINAL" -e "$EDITOR" "$1"
        else setsid -f "$BROWSER" "$1" >/dev/null 2>&1; fi ;;
    esac
  '';

  newsboat-config = pkgs.writeText "config" ''
    auto-reload no
    reload-threads 8
    prepopulate-query-feeds yes

    # dont keep a search history
    history-limit 0

    datetime-format %F

    text-width 85

    external-url-viewer "${pkgs.urlscan}/bin/urlscan -dc -r '${linkhandler} {}'"
    browser ${linkhandler}
    macro , open-in-browser
    macro c set browser "${pkgs.xsel}/bin/xsel -b <<<" ; open-in-browser ; set browser ${linkhandler}
    macro v set browser "${pkgs.util-linux}/bin/setsid -f ${pkgs.mpv}/bin/mpv" ; open-in-browser ; set browser ${linkhandler}
    macro y set browser "${youtube-download}" ; open-in-browser ; set browser ${linkhandler}

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

    urls-source "ttrss"
    ttrss-url "https://feed.kmein.de"
    ttrss-login "k"
    ttrss-flag-star "s"
    ttrss-password "${lib.strings.fileContents <secrets/tt-rss/password>}"
    ttrss-mode "multi"
  '';

  newsboat-sql = "${pkgs.sqlite}/bin/sqlite3 ${newsboat-home}/cache.db";
in {
  nixpkgs.config.packageOverrides = pkgs: {
    newsboat = pkgs.writers.writeDashBin "newsboat" ''
      ${pkgs.newsboat}/bin/newsboat -C ${newsboat-config} -u ${pkgs.writeText "newsboat-urls" ''
        https://feed.kmein.de/public.php?op=rss&id=-1&is_cat=0&q=&key=${lib.strings.fileContents <secrets/tt-rss/private-rss.key>} "foo"
        "query:🕒 Read Later:flags # \"e\""
        "query:📥 Unread:unread = \"yes\""
      ''} "$@"
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
