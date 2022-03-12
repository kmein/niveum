{pkgs, ...}: {
  systemd.services.moinbot = {
    startAt = "7:00";
    script = ''
      echo moin | ${pkgs.ircaids}/bin/ircsink \
        --nick moinbot \
        --server irc.hackint.org \
        --port 6697 \
        --secure \
        --target '#hsmr' >/dev/null 2>&1
    '';
    serviceConfig.DynamicUser = true;
  };

  systemd.timers.moinbot.timerConfig.RandomizedDelaySec = "14h";
}