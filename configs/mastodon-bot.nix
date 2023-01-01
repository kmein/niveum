{pkgs, ...}: {
  systemd.services.imaginary-illuminations = {
    enable = true;
    wants = ["network-online.target"];
    serviceConfig = {
      User = "kfm";
      Group = "users";
      WorkingDirectory = "/home/kfm/cloud/Seafile/Documents/Media/imaginary-illuminations";
    };
    startAt = "7:00";
    script = ''
      ${pkgs.deno}/bin/deno run -A post.ts
    '';
  };

  systemd.timers.imaginary-illuminations.timerConfig.RandomizedDelaySec = "14h";
}
