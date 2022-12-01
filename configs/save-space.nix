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
}
