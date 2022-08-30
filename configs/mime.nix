{
  home-manager.users.me = {
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "application/epub+zip" = "org.pwmt.zathura.desktop";
        "application/pdf" = "org.pwmt.zathura.desktop";
        "application/vnd.oasis.opendocument.text" = "writer.desktop";
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";
        "image/jpeg" = "nsxiv.desktop";
        "image/png" = "nsxiv.desktop";
        "image/vnd.djvu+multipage" = "org.pwmt.zathura.desktop";
        "text/html" = "brave-browser.desktop";
        "text/markdown" = "nvim.desktop";
        "text/plain" = "nvim.desktop";
        "x-scheme-handler/about" = "brave-browser.desktop";
        "x-scheme-handler/http" = "brave-browser.desktop";
        "x-scheme-handler/https" = "brave-browser.desktop";
        "x-scheme-handler/mailto" = "brave-browser.desktop";
        "x-scheme-handler/unknown" = "brave-browser.desktop";
        "x-scheme-handler/webcal" = "brave-browser.desktop";
        "inode/directory" = "pcmanfm.desktop";
      };
    };
  };
}
