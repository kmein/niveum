{
  networking.hostName = "khall";
  networking.networkmanager.enable = true;
  system.stateVersion = "25.11";

  age.secrets = {
    restic.file = ../../secrets/restic.age;
  };
}
