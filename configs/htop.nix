{
  home-manager.users.me = {
    programs.htop = {
      enable = true;
      settings = {
        color_scheme = 0;
        account_guest_in_cpu_meter = true;
        cpu_count_from_zero = false;
        delay = 15;
        detailed_cpu_time = false;
        header_margin = true;
        hide_kernel_threads = true;
        hide_threads = true;
        hide_userland_threads = true;
        highlight_base_name = true;
        highlight_megabytes = true;
        highlight_threads = true;
        shadow_other_users = true;
        show_program_path = false;
        show_thread_names = false;
        sort_descending = true;
        sort_key = "PERCENT_CPU";
        tree_view = true;
        update_process_names = false;
        right_meters = ["Uptime" "Tasks" "LoadAverage" "Battery"];
        left_meters = ["LeftCPUs2" "RightCPUs2" "Memory" "Swap"];
      };
    };
  };
}
