{pkgs, ...}: {
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (import <nix-writers/pkgs>)
      (import <stockholm/krebs/5pkgs>)
    ];
  };
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = "experimental-features = nix-command flakes";
    settings = {
      substituters = [
        "http://cache.prism.r"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "cache.prism-1:+S+6Lo/n27XEtvdlQKuJIcb1yO5NUqUCE2lolmTgNJU="
        "cache.prism-2:YwmCm3/s/D+SxrPKN/ETjlpw/219pNUbpnluatp6FKI="
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
      ];
    };
  };
}
