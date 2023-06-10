{
  config,
  pkgs,
  lib,
  ...
}: {
  users.extraUsers.kiosk = {
    isNormalUser = true;
    password = "";
    extraGroups = ["audio"];
  };
  # TODO https://github.com/cage-kiosk/cage/issues/138
  services.cage = {
    enable = true;
    user = config.users.extraUsers.kiosk.name;
    extraArguments = ["-s"]; # allow vt switching
    program = let
      startUrls = ["https://youtube.com" "http://radio.kmein.r"];
    in
      pkgs.writers.writeDash "kiosk-browser" ''
        while true; do
          ${pkgs.brave}/bin/brave \
            --no-first-run --no-message-box --noerrdialogs \
            --default-browser --no-default-browser-check \
            --start-maximized ${lib.escapeShellArgs startUrls}
          sleep 0.5
        done
      '';
  };
  systemd.services.cage-tty1.environment.XKB_DEFAULT_LAYOUT = "de";
  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
    ];
  };
}
