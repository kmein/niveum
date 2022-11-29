{
  config,
  pkgs,
  ...
}: {
  services.clipmenu.enable = true;

  # synchronize all the clipboards
  systemd.user.services.autocutsel = {
    enable = true;
    wantedBy = ["graphical-session.target"];
    after = ["graphical-session.target"];
    serviceConfig = {
      Type = "forking";
      ExecStart = pkgs.writers.writeDash "autocutsel" ''
        ${pkgs.autocutsel}/bin/autocutsel -fork -selection PRIMARY
        ${pkgs.autocutsel}/bin/autocutsel -fork -selection CLIPBOARD
      '';
    };
  };
}
