{
  pkgs,
  config,
  ...
}: {
  systemd.services.moinbot = {
    startAt = "7:00";
    script = ''
      greeting=$(echo "moin
      oi
      noim
      MOIN
      OI
      moi" | shuf -n1)
      echo "$greeting" | ${config.nur.repos.mic92.ircsink}/bin/ircsink \
        --nick "$greeting""bot" \
        --server irc.hackint.org \
        --port 6697 \
        --secure \
        --target '#hsmr-moin' >/dev/null 2>&1
    '';
    serviceConfig.DynamicUser = true;
  };

  niveum.passport.services = [
    {
      title = "moinbot";
      description = "greets #hsmr-moin:hackint.org daily.";
    }
  ];

  systemd.timers.moinbot.timerConfig.RandomizedDelaySec = "14h";
}
