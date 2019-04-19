{ pkgs, ... }:
{
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  networking.hosts."192.168.178.27" = [ "officejet" ];
}
