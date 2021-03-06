{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.tmuxp
    pkgs.reptyr # move programs over to a tmux session
  ];

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    clock24 = true;
    terminal = "screen-256color";
    extraConfig = ''
      set -g status-interval 2
      set -g status-left-length 32
      set -g status-right-length 150

      set -g status-bg default

      setw -g window-status-format "#[fg=colour12,bg=colour233] #I #[fg=white,bg=colour237] #W "
      setw -g window-status-current-format "#[fg=colour12,bg=colour233] * #[fg=white,bg=colour237,bold] #W "

      set -g status-left ""
      set -g status-right "#[fg=colour255,bg=colour237,bold] %Y-%m-%d #[default]#[fg=colour12,bg=colour233] %H:%M "
      set -g status-justify left

      set -g status-position bottom

      set -g mouse on

      unbind *
      bind * list-clients
    '';
  };
}
