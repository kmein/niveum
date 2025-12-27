{ pkgs, ... }:
{
  services.tor.enable = true;
  services.tor.client.enable = true;
  environment.systemPackages = [
    pkgs.tor
    pkgs.torsocks
  ];
}
