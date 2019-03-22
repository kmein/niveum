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
    nixi = ''nix repl "<nixpkgs>"'';
    grep = "grep --color=auto";
    rm = "rm -i";
    cp = "cp -i";
    mv = "mv -i";
  };

  environment.interactiveShellInit = "export PATH=$PATH:$HOME/.local/bin:$HOME/.cargo/bin";

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    syntaxHighlighting.highlighters = [ "main" "brackets" "pattern" "line" ];
    interactiveShellInit = ''
      setopt INTERACTIVE_COMMENTS CORRECT
      setopt MULTIOS
      setopt AUTO_NAME_DIRS
      setopt AUTOCD CDABLE_VARS
      setopt HIST_IGNORE_ALL_DUPS
      setopt VI
      unsetopt NOMATCH

      export KEYTIMEOUT=1

      hash -d nixos=/etc/nixos

      autoload -U zmv run-help

      take() {
        mkdir $1
        cd $1
      }
    '';
    promptInit = ''
      autoload -Uz vcs_info
      zstyle ':vcs_info:*' enable git
      zstyle ':vcs_info:*' check-for-changes true
      zstyle ':vcs_info:*' stagedstr '%F{green}+%f'
      zstyle ':vcs_info:*' unstagedstr '%F{red}~%f'
      zstyle ':vcs_info:*' use-prompt-escapes true
      zstyle ':vcs_info:*' formats "%c%u%F{cyan}%b%f"
      zstyle ':vcs_info:*' actionformats "(%a) %c%u%F{cyan}%b%f"

      precmd () {
        vcs_info
        RPROMPT="$vcs_info_msg_0_"
        if [[ -n $IN_NIX_SHELL ]]; then
          PROMPT='%B%~%b %(?.%F{green}.%F{red})λ%f '
        else
          PROMPT='%B%~%b %(?.%F{green}.%F{red})%#%f '
        fi
      }

      zle-keymap-select zle-line-init () {
        case $KEYMAP in
          vicmd) print -n '\e]12;green\a';;
          viins|main) print -n '\e]12;gray\a';;
        esac
      }

      zle -N zle-line-init
      zle -N zle-keymap-select
    '';
  };

  programs.bash = {
    promptInit = ''PS1="$(tput bold)\w \$([[ \$? == 0 ]] && echo \"\[\033[1;32m\]\" || echo \"\[\033[1;31m\]\")\$$(tput sgr0) "'';
    interactiveShellInit = ''
      set -o vi
    '';
    enableCompletion = true;
  };

}
