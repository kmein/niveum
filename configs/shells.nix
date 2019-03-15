{ pkgs, lib, ... }:
let
  zsh-plugins-file =
    pkgs.writeText "zsh_plugins.txt" (lib.concatStringsSep "\n" [
      "sharat87/zsh-vim-mode"
      "Tarrasch/zsh-mcd"
      "mafredri/zsh-async"
      "zsh-users/zsh-completions"
      "caarlos0/ports kind:path"
      "Tarrasch/zsh-functional"
      "zsh-users/zsh-history-substring-search"
    ]);
  zsh-plugins =
    let package = {pkgs}:
        pkgs.stdenv.mkDerivation rec {
          name = "zsh-plugins";
          phases = [ "configurePhase" "installPhase" ];
          buildInputs = with pkgs; [ antibody git ];
          configurePhase = ''
            export GIT_SSL_CAINFO=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          '';
          installPhase = ''
            mkdir -p $out/.cache
            XDG_CACHE_HOME=$out/.cache antibody bundle < ${zsh-plugins-file} > $out/zsh_plugins.sh
          '';
        };
    in pkgs.callPackage package {};
in {
  environment.shellAliases = {
    ns = "nix-shell --run zsh";
    niveum-switch = "sudo -i nixos-rebuild --fast switch";
    niveum-upgrade = "sudo -i nix-channel --update && sudo -i nixos-rebuild switch";
    ":r" = ''echo "You stupid!"'';
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
      export KEYTIMEOUT=1
      bindkey -v
      bindkey '^w' backward-kill-word
      bindkey '^r' history-incremental-search-backward
    '';
      # source ${zsh-plugins}/zsh_plugins.sh
    promptInit = ''
      PROMPT=$'%{\e[1m%}%~%{\e[0m%}'
      PROMPT="$PROMPT \$([[ \$? == 0 ]] && echo \"%{$fg_bold[green]%}\" || echo \"%{$fg_bold[red]%}\")\$(test $IN_NIX_SHELL && echo λ || echo %#)%{$reset_color%} "
      RPROMPT='$(git_prompt_info)'
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
