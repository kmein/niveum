{ config, pkgs, ... }:
{
  nixpkgs.config = {
    allowUnfree = true;
    # mx150; nixpkgs defaults to 7.5+, so cache.nixos-cuda.org has nothing for it
    cudaCapabilities = [ "6.1" ];
  };
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.graphics.enable = true;

  services.llama-cpp = {
    enable = true;
    package = pkgs.llama-cpp.override { cudaSupport = true; };
    model = pkgs.fetchurl {
      url = "https://huggingface.co/Qwen/Qwen2.5-Coder-1.5B-Instruct-GGUF/resolve/f86cb2c1fa58255f8052cc32aeede1b7482d4361/qwen2.5-coder-1.5b-instruct-q4_k_m.gguf";
      sha256 = "cc324af070c2ecbfd324a30884d2f951a7ff756aba85cb811a6ec436933bb046";
    };
    # weights + 8k context fit the mx150's 2 GB
    extraFlags = [
      "-ngl"
      "99"
      "-c"
      "8192"
    ];
  };

  # niphas' minuet expects llama-server on its default port and a qwen2.5-coder model
  niphas.editor.minuet = "llama-cpp";

  environment.systemPackages = [ config.services.llama-cpp.package ];

  hardware.nvidia = {
    modesetting.enable = true; # needed for PRIME
    open = false; # MX150 (Pascal) needs proprietary firmware
    # 590+ dropped pascal
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
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
