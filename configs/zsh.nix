{ config, ... }:
{
  home-manager.users.me.home.file.".zshrc".text = "# nothing to see here";

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
      setopt AUTO_MENU
      setopt COMPLETE_IN_WORD
      setopt ALWAYS_TO_END
      unsetopt NOMATCH
      unsetopt MENU_COMPLETE

      zstyle ':completion:*:*:*:*:*' menu select
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
      zstyle ':completion:*' special-dirs true
      zstyle ':completion:*' list-colors \'\'
      zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
      zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
      zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

      export KEYTIMEOUT=1

      hash -d nixos=/etc/nixos niveum=${config.users.users.me.home}/prog/git/niveum

      autoload -U zmv run-help

      take() {
        mkdir $1
        cd $1
      }

      niveum-deploy() {
        for system in "$@"; do
          eval $(nix-build --no-out-link ~niveum/deploy.nix -A "$system") &
        done
        wait
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
}
