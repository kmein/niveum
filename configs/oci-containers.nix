{ pkgs, ... }:
{
  virtualisation.oci-containers.backend = "podman";

  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [ "--all" ];
    };
  };

  # pull newer images on Monday; hosts restart their containers on Tuesday
  systemd.services.update-containers = {
    startAt = "Mon 02:00";
    script = ''
      images=$(${pkgs.podman}/bin/podman ps -a --format="{{.Image}}" | sort -u)

      for image in $images; do
        ${pkgs.podman}/bin/podman pull "$image"
      done
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      RestartSec = "1h";
    };
  };
}
