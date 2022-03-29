{pkgs, ...}: {
  services.tor.enable = true;
  environment.systemPackages = [pkgs.tor];
}
