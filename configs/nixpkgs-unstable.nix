{ config, ... }: {
  nixpkgs.config.packageOverrides = pkgs: {
    unstable = import <nixpkgs-unstable> { config = config.nixpkgs.config; };
  };
}
