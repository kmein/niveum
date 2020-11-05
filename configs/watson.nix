{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.watson ];
}
