{ ... }:
{
  virtualisation.oci-containers.backend = "podman";

  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [ "--all" ];
    };
  };
}
