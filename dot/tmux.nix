''
set -g status-interval 2
set -g status-left-length 32
set -g status-right-length 150

# Appearance
# set -g status-utf8 on
set -g status-bg default
# set -g status-fg white
setw -g window-status-format "#[fg=colour12,bg=colour233] #I #[fg=white,bg=colour237] #W "
setw -g window-status-current-format "#[fg=colour12,bg=colour233] * #[fg=white,bg=colour237,bold] #W "
# setw -g window-status-current-bg colour0
# setw -g window-status-current-fg colour11
# Status Bar
set -g status-left ""
# set -g status-right "#[fg=colour196,bg=colour233] LOAD #[fg=white,bg=colour237] #(awk '{print $1, $2, $3}' < /proc/loadavg) #[default] #[fg=colour196,bg=colour233] CPU #[fg=white,bg=colour237] #(ps -A -o pcpu | tail -n+2 | paste -sd+ | bc) #[default] #[fg=colour196,bg=colour233] MEM #[fg=white,bg=colour237] #(ps -A -o pmem | tail -n+2 | paste -sd+ | bc) #[default] #[fg=white,bg=colour237,bold] %Y-%m-%d #[default]#[fg=colour196,bg=colour233] %H:%M "
set -g status-right "#[fg=colour12,bg=colour233] LOAD #[fg=colour255,bg=colour237] #(cut -d' ' -f 1-3 < /proc/loadavg) #[default] #[fg=colour12,bg=colour233] MEM #[fg=colour255,bg=colour237] #(free -h --si | awk 'NR==2{print $3}') #[default] #[fg=colour255,bg=colour237,bold] %Y-%m-%d #[default]#[fg=colour12,bg=colour233] %H:%M "
set -g status-justify left

# setw -g window-status-current-fg black
# setw -g window-status-fg black
# setw -g window-status-current-bg white
# setw -g window-status-bg white
# setw -g window-status-current-attr bold

set -g pane-active-border-fg colour237
set -g pane-border-fg colour237
set -g pane-active-border-bg default
set -g pane-border-bg default

set -g status-position bottom

set -g mouse on

# displays *
unbind *
bind * list-clients
''
