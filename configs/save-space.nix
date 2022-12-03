{
  documentation.doc.enable = false;
  documentation.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;
  documentation.man.generateCaches = false;
  fonts.fontconfig.enable = false;
  nix.gc.automatic = true;
  nix.optimise.automatic = true;
  services.journald.extraConfig = "SystemMaxUse=500M";
  # https://webzine.snowflake.ovh/issue-001.html -- garbage collect when less then 1 GB is available, make 3 GB fee
  nix.extraOptions = ''
    min-free = 1073741824
    max-free = 3221225472
  '';
}
