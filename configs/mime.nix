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
        // {
          "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";
          "application/pdf" = "org.pwmt.zathura.desktop";
          "application/vnd.oasis.opendocument.text" = "writer.desktop";
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";
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
