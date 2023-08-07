{pkgs, ...}: {
  programs.fzf = {
    fuzzyCompletion = true;
    keybindings = true;
  };

  home-manager.users.me = {
    programs.fzf = rec {
      enable = true;
      defaultCommand = "${pkgs.fd}/bin/fd --type f --strip-cwd-prefix --follow --no-ignore-vcs --exclude .git";
      defaultOptions = ["--height=40%"];
      changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
      changeDirWidgetOptions = [
        "--preview='${pkgs.tree}/bin/tree -L 1 {}'"
        "--bind=space:toggle-preview"
        "--preview-window=hidden"
      ];
      fileWidgetCommand = defaultCommand;
      fileWidgetOptions = ["--preview='head -$LINES {}'"];
    };
  };
}
