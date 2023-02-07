pkgs: rec {
  terminal = "alacritty";
  browser = "${pkgs.firefox}/bin/firefox";
  fileManager = "${pkgs.cinnamon.nemo}/bin/nemo";
}
