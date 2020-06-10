{ config, ... }: {
  services.keybase.enable = true;

  services.kbfs = {
    enable = true;
    mountPoint = "%h/cloud/keybase";
  };
}
