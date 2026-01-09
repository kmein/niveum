{
  nixpkgs.config = {
    allowUnfree = true;
  };
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    modesetting.enable = true; # needed for PRIME
    open = false; # MX150 (Pascal) needs proprietary firmware
    nvidiaSettings = true;
    prime = {
      offload = {
        enable = true;
        enableOffloadCmd = true;
      };
      # from lspci
      intelBusId = "PCI:0:2:0"; # Intel UHD Graphics 620
      nvidiaBusId = "PCI:1:0:0"; # NVIDIA GeForce MX150
    };
  };

  nix.settings.system-features = [ "cuda" ];

  programs.nix-required-mounts = {
    enable = true;
    presets.nvidia-gpu.enable = true;
    # Workaround for <https://github.com/NixOS/nix/issues/9272>, copied from
    # <https://github.com/nix-community/infra/pull/1807>.
    extraWrapperArgs = [
      "--run shift"
      "--add-flag '${
        builtins.unsafeDiscardOutputDependency
          (derivation {
            name = "needs-cuda";
            builder = "_";
            system = "_";
            requiredSystemFeatures = [ "cuda" ];
          }).drvPath
      }'"
    ];
  };
}
