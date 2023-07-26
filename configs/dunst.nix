{
  config,
  pkgs,
  ...
}: let
  inherit (import ../lib) defaultApplications theme;
in {
  home-manager.users.me.services.dunst = {
    enable = true;
    iconTheme = (theme pkgs).icon;
    settings = {
      global = {
        transparency = 10;
        geometry = "200x5-30+20";
        follow = "mouse";
        indicate_hidden = true;
        notification_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        sort = true;
        markup = "full";
        format = "%a\\n<b>%s</b>\\n%b";
        alignment = "left";
        show_age_threshold = 60;
        bounce_freq = 0;
        word_wrap = true;
        ellipsize = "middle";
        ignore_newline = false;
        stack_duplicates = true;
        hide_duplicate_count = false;
        max_icon_size = 32;
        sticky_history = true;
        history_length = 20;
        dmenu = "${pkgs.rofi}/bin/rofi -display-run dunst -show run";
        browser = (defaultApplications pkgs).browser;
        verbosity = "mesg";
        corner_radius = 0;
        mouse_left_click = "do_action";
        mouse_right_click = "close_current";
        mouse_middle_click = "close_all";
      };
      urgency_low = {
        timeout = 5;
      };
      urgency_normal = {
        timeout = 10;
      };
      urgency_critical = {
        timeout = 0;
      };
    };
  };
}
