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
      defaultApplications = {
        "application/epub+zip" = "org.pwmt.zathura.desktop";
        "application/pdf" = "org.pwmt.zathura.desktop";
        "application/vnd.oasis.opendocument.text" = "writer.desktop";
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";
        "image/jpeg" = "nsxiv.desktop";
        "image/png" = "nsxiv.desktop";
        "image/vnd.djvu+multipage" = "org.pwmt.zathura.desktop";
        "inode/directory" = "nemo.desktop";
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
