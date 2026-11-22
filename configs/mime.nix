{ lib, ... }:
{
  home-manager.users.me = {
    xdg.mimeApps = {
      enable = true;
      associations = {
        added = {
          "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
        };
        removed = {
        };
      };
      # what nsxiv's desktop file claims and its imlib2 has a loader for;
      # anything left out falls to whichever app claims it, often the browser
      defaultApplications =
        lib.genAttrs [
          "image/avif"
          "image/bmp"
          "image/gif"
          "image/heic"
          "image/heif"
          "image/jpeg"
          "image/jpg"
          "image/jxl"
          "image/png"
          "image/svg+xml"
          "image/tiff"
          "image/webp"
          "image/x-bmp"
          "image/x-portable-anymap"
          "image/x-portable-bitmap"
          "image/x-portable-graymap"
          "image/x-tga"
          "image/x-xpixmap"
        ] (_: "nsxiv.desktop")
        # every audio type an installed app claims, so audacity and shotcut take none
        // lib.genAttrs [
          "audio/3gpp"
          "audio/3gpp2"
          "audio/AMR"
          "audio/AMR-WB"
          "audio/aac"
          "audio/ac3"
          "audio/aiff"
          "audio/amr"
          "audio/amr-wb"
          "audio/basic"
          "audio/dv"
          "audio/eac3"
          "audio/flac"
          "audio/m3u"
          "audio/m4a"
          "audio/midi"
          "audio/mp1"
          "audio/mp2"
          "audio/mp3"
          "audio/mp4"
          "audio/mpeg"
          "audio/mpeg2"
          "audio/mpeg3"
          "audio/mpegurl"
          "audio/mpg"
          "audio/musepack"
          "audio/ogg"
          "audio/opus"
          "audio/rn-mpeg"
          "audio/scpls"
          "audio/vnd.dolby.heaac.1"
          "audio/vnd.dolby.heaac.2"
          "audio/vnd.dolby.mlp"
          "audio/vnd.dts"
          "audio/vnd.dts.hd"
          "audio/vnd.rn-realaudio"
          "audio/vnd.wave"
          "audio/vorbis"
          "audio/wav"
          "audio/webm"
          "audio/x-aac"
          "audio/x-adpcm"
          "audio/x-aiff"
          "audio/x-ape"
          "audio/x-flac"
          "audio/x-gsm"
          "audio/x-it"
          "audio/x-m4a"
          "audio/x-matroska"
          "audio/x-mod"
          "audio/x-mp1"
          "audio/x-mp2"
          "audio/x-mp3"
          "audio/x-mpeg"
          "audio/x-mpegurl"
          "audio/x-mpg"
          "audio/x-ms-asf"
          "audio/x-ms-asx"
          "audio/x-ms-wax"
          "audio/x-ms-wma"
          "audio/x-musepack"
          "audio/x-pls"
          "audio/x-pn-aiff"
          "audio/x-pn-au"
          "audio/x-pn-realaudio"
          "audio/x-pn-realaudio-plugin"
          "audio/x-pn-wav"
          "audio/x-pn-windows-acm"
          "audio/x-pn-windows-pcm"
          "audio/x-real-audio"
          "audio/x-realaudio"
          "audio/x-s3m"
          "audio/x-scpls"
          "audio/x-shorten"
          "audio/x-speex"
          "audio/x-tta"
          "audio/x-vorbis"
          "audio/x-vorbis+ogg"
          "audio/x-wav"
          "audio/x-wavpack"
          "audio/x-xm"
        ] (_: "mpv.desktop")
        // {
          "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";
          "application/pdf" = "mupdf.desktop";
          "application/vnd.oasis.opendocument.text" = "writer.desktop";
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";
          # lf claims it too, but its desktop file drops the path
          "inode/directory" = "pcmanfm.desktop";
          "image/vnd.djvu+multipage" = "org.pwmt.zathura.desktop";
          "text/html" = "firefox.desktop";
          "text/markdown" = "nvim.desktop";
          "text/plain" = "nvim.desktop";
          "x-scheme-handler/about" = "firefox.desktop";
          "x-scheme-handler/http" = "firefox.desktop";
          "x-scheme-handler/https" = "firefox.desktop";
          "x-scheme-handler/mailto" = "firefox.desktop";
          "x-scheme-handler/unknown" = "firefox.desktop";
          "x-scheme-handler/webcal" = "firefox.desktop";
          "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
        };
    };
  };
}
