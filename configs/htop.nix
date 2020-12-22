{
  home-manager.users.me = {
    programs.htop = {
      enable = true;
      colorScheme = 0;
      accountGuestInCpuMeter = true;
      cpuCountFromZero = false;
      delay = 15;
      detailedCpuTime = false;
      headerMargin = true;
      hideKernelThreads = true;
      hideThreads = true;
      hideUserlandThreads = true;
      highlightBaseName = true;
      highlightMegabytes = true;
      highlightThreads = true;
      shadowOtherUsers = true;
      showProgramPath = false;
      showThreadNames = false;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
      treeView = true;
      updateProcessNames = false;
      meters = {
        left = [
          {
            kind = "LeftCPUs2";
            mode = 1;
          }
          {
            kind = "RightCPUs2";
            mode = 1;
          }
          {
            kind = "Memory";
            mode = 1;
          }
          {
            kind = "Swap";
            mode = 1;
          }
        ];
        right = [ "Uptime" "Tasks" "LoadAverage" "Battery" ];
      };
    };
  };
}
