{
  pkgs,
  ...
}:
{
  systemd.services.moinbot = {
    startAt = "7:00";
    script = ''
      greeting=$(echo "moin
      MOIN
      moin: gib" | shuf -n1)
      echo "$greeting" | ${pkgs.nur.repos.mic92.ircsink}/bin/ircsink \
        --nick "$greeting""bot" \
        --server irc.hackint.org \
        --port 6697 \
        --secure \
        --target '#hsmr' >/dev/null 2>&1
    '';
    serviceConfig = pkgs.lib.niveum.hardening // {
      DynamicUser = true;
    };
  };

  systemd.timers.moinbot.timerConfig.RandomizedDelaySec = "14h";
}
