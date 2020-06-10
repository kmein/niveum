{ lib, config, pkgs, ... }: {
  imports = [
    (import <stockholm/makefu/3modules/bump-distrowatch.nix> {
      inherit lib config;
      pkgs = pkgs // { writeDash = pkgs.writers.writeDash; };
    })
  ];

  makefu.distrobump.enable = false;
}
