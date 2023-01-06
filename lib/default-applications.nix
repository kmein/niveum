pkgs: rec {
  terminal = "alacritty";
  browser = "${pkgs.firefox}/bin/firefox";
  fileManager = "${terminal} -e ${pkgs.ranger}/bin/ranger";
}
