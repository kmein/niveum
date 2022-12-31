{
  systemd.services.imaginary-illuminations = {
    enable = true;
    wants = ["network-online.target"];
    serviceConfig = {
      user = "kfm";
      WorkingDirectory = "/home/kfm/cloud/Seafile/Documents/Media/imaginary-illuminations";
    };
    startAt = "7:00";
    script = ''
      ./post.ts
    '';
    serviceConfig.DynamicUser = true;
  };

  systemd.timers.moinbot.timerConfig.RandomizedDelaySec = "14h";
}
