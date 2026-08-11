{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.radicle-node ];
}
