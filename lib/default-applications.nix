pkgs: rec {
  terminal = "${pkgs.alacritty}/bin/alacritty";
  browser = "${pkgs.brave}/bin/brave";
  fileManager = "${terminal} -e ${pkgs.ranger}/bin/ranger";
}
