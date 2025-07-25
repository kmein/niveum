{pkgs, ...}: {
  environment.systemPackages = [
    pkgs.tmuxp
    pkgs.reptyr # move programs over to a tmux session
  ];

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    clock24 = true;
    terminal = "screen-256color";
    baseIndex = 1;
    aggressiveResize = true;
    escapeTime = 50;
    historyLimit = 7000;
    shortcut = "b";
    extraConfig = ''
      set -g mouse on

      unbind *
      bind * list-clients

      # naVIgate
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      # Use C-h and C-l to cycle through panes
      bind -r C-h select-window -t :-
      bind -r C-l select-window -t :+

      setw -g monitor-activity on
      set -g visual-activity on

      set -g status-interval 2
      set -g status-left-length 32
      set -g status-right-length 150

      set -g status-position bottom
    '';
  };
}
