pkgs: rec {
  terminal = "alacritty";
  browser = "${pkgs.brave}/bin/brave";
  fileManager = "${terminal} -e ${pkgs.ranger}/bin/ranger";
}
