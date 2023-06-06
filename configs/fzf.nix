{
  pkgs,
  lib,
  ...
}: {
  environment = {
    systemPackages = [pkgs.fzf];
    variables = rec {
      FZF_DEFAULT_COMMAND = "${pkgs.fd}/bin/fd --type f --strip-cwd-prefix --follow --no-ignore-vcs --exclude .git";
      FZF_DEFAULT_OPTS =
        lib.escapeShellArgs ["--height=40%"];
      FZF_ALT_C_COMMAND = "${pkgs.fd}/bin/fd --type d";
      FZF_ALT_C_OPTS = lib.escapeShellArgs [
        "--preview='${pkgs.tree}/bin/tree -L 1 \"{}\"'"
        "--bind=space:toggle-preview"
        "--preview-window=hidden"
      ];
      FZF_CTRL_T_COMMAND = FZF_DEFAULT_COMMAND;
      FZF_CTRL_T_OPTS =
        lib.escapeShellArgs ["--preview='head -$LINES {}'"];
    };
  };

  programs.fzf = {
    fuzzyCompletion = true;
    keybindings = true;
  };
}
