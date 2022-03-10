{
  config,
  pkgs,
  ...
}: {
  home-manager.users.me.home.file.".zshrc".text = ''
    # nothing to see here
  '';

  programs.zsh = let
    zsh-completions = pkgs.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-completions";
      rev = "cf565254e26bb7ce03f51889e9a29953b955b1fb";
      sha256 = "1yf4rz99acdsiy0y1v3bm65xvs2m0sl92ysz0rnnrlbd5amn283l";
    };
    zsh-history-substring-search = pkgs.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-history-substring-search";
      rev = "aae3388491c2312c4efb2e86bcb999927bb2900e";
      sha256 = "0lgmq1xcccnz5cf7vl0r0qj351hwclx9p80cl0qczxry4r2g5qaz";
    };
  in {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    syntaxHighlighting.highlighters = ["main" "brackets" "pattern" "line"];
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

      hash -d nixos=/etc/nixos niveum=${config.users.users.me.home}/projects/niveum

      autoload -U zmv run-help

      fpath=(${zsh-completions}/src $fpath)
      source ${zsh-history-substring-search}/zsh-history-substring-search.zsh

      bindkey '^[[A' history-substring-search-up
      bindkey '^[[B' history-substring-search-down
      bindkey -M vicmd 'k' history-substring-search-up
      bindkey -M vicmd 'j' history-substring-search-down
    '';
    promptInit = with config.niveum; ''
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
          PROMPT='%B%~%b %(?.%F{${promptColours.success}}.%F{${promptColours.failure}})λ%f '
        else
          PROMPT='%B%~%b %(?.%F{${promptColours.success}}.%F{${promptColours.failure}})%#%f '
        fi
        print -Pn "\e]2;%n@%M:%~\a" # title bar prompt
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
