{
  documentation.doc.enable = false;
  documentation.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;
  documentation.man.cache.enable = false;
  fonts.fontconfig.enable = false;
  nix.gc.automatic = true;
  # without this, gc keeps every system generation and their closures forever
  nix.gc.options = "--delete-older-than 14d";
  nix.optimise.automatic = true;
  services.journald.extraConfig = "SystemMaxUse=500M";
  # https://webzine.snowflake.ovh/issue-001.html -- garbage collect when less then 1 GB is available, make 3 GB fee
  nix.extraOptions = ''
    min-free = 1073741824
    max-free = 3221225472
  '';
}
