{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.networking) hostName;

  # sent before authentication, so to anyone who connects; the art lets a client
  # without the pinned key compare it with its VisualHostKey output
  banner = pkgs.runCommand "ssh-banner" { nativeBuildInputs = [ pkgs.openssh ]; } ''
    {
      cat <<'EOF'
    Private machine. If you are not its owner, please disconnect now.

    EOF
      ssh-keygen -lv -f ${pkgs.writeText "host-key.pub" "${pkgs.lib.niveum.machines.${hostName}.hostKey} ${hostName}"}
    } > $out
  '';

  # only what needs doing; silent when all is well
  todo = pkgs.writers.writeDash "motd-todo" ''
    ${lib.getExe' pkgs.systemd "systemctl"} --failed --no-legend --plain | while read -r unit _; do
      echo "failed: $unit"
    done
    running=$(${lib.getExe' pkgs.coreutils "uname"} -r)
    installed=$(${lib.getExe' pkgs.coreutils "ls"} /run/current-system/kernel-modules/lib/modules)
    for file in kernel initrd kernel-modules; do
      if [ "$(${lib.getExe' pkgs.coreutils "readlink"} -f /run/booted-system/$file)" != "$(${lib.getExe' pkgs.coreutils "readlink"} -f /run/current-system/$file)" ]; then
        if [ "$running" = "$installed" ]; then
          echo "reboot: kernel $running, but the booted $file is not the current one"
        else
          echo "reboot: kernel $running is running, $installed is installed"
        fi
        break
      fi
    done
    ${lib.getExe' pkgs.coreutils "df"} --local --output=pcent,target -x tmpfs -x devtmpfs -x efivarfs | while read -r used target; do
      if [ "''${used%\%}" -ge 90 ] 2>/dev/null; then
        echo "disk: $target is $used full"
      fi
    done
  '';
in
{
  programs.rust-motd = {
    enable = true;
    settings.banner = {
      color = "yellow";
      command = todo;
    };
  };

  services.openssh = {
    enable = true;
    ports = [ pkgs.lib.niveum.machines.${config.networking.hostName}.sshPort ];
    settings = {
      PasswordAuthentication = false;
      Banner = "${banner}";
      X11Forwarding = true;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = pkgs.lib.niveum.kieran.sshKeys;
}
