{ config, pkgs, ... }:
{
  # cli tools systemd ships in lib/systemd only, with no client in bin
  environment.systemPackages = [
    (pkgs.runCommand "systemd-tools" { } ''
      mkdir -p $out/bin
      for tool in \
        systemd-report \
        systemd-pcrlock \
        systemd-measure \
        systemd-sbsign \
        systemd-keyutil \
        systemd-bless-boot \
        systemd-socket-proxyd \
        systemd-networkd-wait-online
      do
        ln -s ${config.systemd.package}/lib/systemd/$tool $out/bin/$tool
        test -x $out/bin/$tool
      done
    '')
  ];
}
