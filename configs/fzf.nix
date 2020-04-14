{ pkgs, lib, ... }:
{
  environment = {
    systemPackages = [ pkgs.fzf ];
    variables = {
      FZF_DEFAULT_OPTS = lib.escapeShellArgs [
        "--height=40%"
        "--layout=reverse"
      ];
      FZF_ALT_C_COMMAND = "${pkgs.fd}/bin/fd --type d";
      FZF_ALT_C_OPTS = lib.escapeShellArgs [
        "--preview '${pkgs.tree}/bin/tree -L 1 {}'"
        "--bind='space:toggle-preview'"
        "--preview-window=hidden"
      ];
      FZF_CTRL_T_COMMAND = "${pkgs.fd}/bin/fd --type f";
      FZF_CTRL_T_OPTS = lib.escapeShellArgs [
        "--preview '${pkgs.bat}/bin/bat {}'"
      ];
    };
  };

  programs.zsh.interactiveShellInit = ''
    if [[ $options[zle] = on ]]; then
      . ${pkgs.fzf}/share/fzf/completion.zsh
      . ${pkgs.fzf}/share/fzf/key-bindings.zsh
    fi
  '';

  programs.bash.interactiveShellInit = ''
    if [[ :$SHELLOPTS: =~ :(vi|emacs): ]]; then
      . ${pkgs.fzf}/share/fzf/completion.bash
      . ${pkgs.fzf}/share/fzf/key-bindings.bash
    fi
  '';
}
