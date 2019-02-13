{ pkgs, ... }:
{
  environment.shellAliases = {
    ns = "nix-shell --command fish";
    ":r" = ''echo "You stupid!"'';
    nixi = ''nix repl "<nixpkgs>"'';
  };

  environment.interactiveShellInit = "export PATH=$PATH:$HOME/.local/bin:$HOME/.cargo/bin";

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      function take
        mkdir $argv
        and cd $argv[-1]
      end
    '';
    promptInit = ''
      function fish_right_prompt --description 'Write out the right prompt'
        set -g __fish_git_prompt_show_informative_status 1
        set -g __fish_git_prompt_hide_untrackedfiles 1

        set -g __fish_git_prompt_color_branch magenta
        set -g __fish_git_prompt_showupstream "informative"
        set -g __fish_git_prompt_char_upstream_ahead "↑"
        set -g __fish_git_prompt_char_upstream_behind "↓"
        set -g __fish_git_prompt_char_upstream_prefix ""

        set -g __fish_git_prompt_char_stagedstate "●"
        set -g __fish_git_prompt_char_dirtystate "✚"
        set -g __fish_git_prompt_char_untrackedfiles "…"
        set -g __fish_git_prompt_char_conflictedstate "✖"
        set -g __fish_git_prompt_char_cleanstate "✔"

        set -g __fish_git_prompt_color_dirtystate blue
        set -g __fish_git_prompt_color_stagedstate yellow
        set -g __fish_git_prompt_color_invalidstate red
        set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
        set -g __fish_git_prompt_color_cleanstate green
        echo -n -s (__fish_git_prompt)
      end

      function fish_prompt --description 'Write out the prompt'
        set color_cwd blue
        set -l suffix
        set -l color_suffix
        switch "$USER"
          case root toor
            set suffix '#'
          case '*'
            set suffix '>'
            if test $IN_NIX_SHELL
              set suffix 'λ'
            end
        end
        if test $status -eq 0
          set color_suffix green
        else
            set color_suffix red
        end
        echo -n -s (set_color --bold $color_cwd) (prompt_pwd) (set_color normal) " " (set_color --bold $color_suffix) "$suffix " (set_color normal)
      end
    '';
  };

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
