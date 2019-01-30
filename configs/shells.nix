{ pkgs, ... }:
{
  environment.shellAliases =
    let rlwrap = cmd: "${pkgs.rlwrap}/bin/rlwrap ${cmd}";
    in {
      o = "${pkgs.xdg_utils}/bin/xdg-open";
      ns = "nix-shell --command zsh";
      ":r" = ''echo "You stupid!"'';
      clipboard = "${pkgs.xclip}/bin/xclip -se c";
      ip = "${pkgs.iproute}/bin/ip -c";
      ocaml = rlwrap "${pkgs.ocaml}/bin/ocaml";
      tmux = "${pkgs.tmux}/bin/tmux -2";
      nixi = ''nix repl "<nixpkgs>"'';
    };

  environment.interactiveShellInit = "export PATH=$PATH:$HOME/.local/bin:$HOME/.cargo/bin";

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    syntaxHighlighting.highlighters = [ "main" "brackets" "pattern" "cursor" "root" "line" ];
    interactiveShellInit = ''
      setopt INTERACTIVE_COMMENTS
      setopt MULTIOS
      setopt CORRECT
      setopt AUTO_NAME_DIRS
      setopt PUSHD_MINUS PUSHD_TO_HOME AUTO_PUSHD
      export KEYTIMEOUT=1
      bindkey -v
      bindkey '^w' backward-kill-word
      bindkey '^r' history-incremental-search-backward
    '';
    promptInit = ''
      PROMPT=$'%{\e[1m%}%~%{\e[0m%}'
      PROMPT="$PROMPT \$([[ \$? == 0 ]] && echo \"%{$fg_bold[green]%}\" || echo \"%{$fg_bold[red]%}\")\$(test $IN_NIX_SHELL && echo λ || echo %#)%{$reset_color%} "
      RPROMPT='$(git_prompt_info) $(date +%H:%M)'
      ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[cyan]%}"
      ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
      ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*"
    '';
    ohMyZsh.enable = true;
    ohMyZsh.plugins = [ "common-aliases" ];
  };

  programs.bash = {
    promptInit = ''PS1="$(tput bold)\w \$([[ \$? == 0 ]] && echo \"\[\033[1;32m\]\" || echo \"\[\033[1;31m\]\")\$$(tput sgr0) "'';
    interactiveShellInit = ''
      set -o vi
    '';
    enableCompletion = true;
  };

}
