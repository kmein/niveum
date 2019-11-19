{ pkgs, ... }:
{
  imports = [
    {
      environment.systemPackages = with pkgs; [
        htmlTidy
        nodePackages_10_x.csslint
        nodePackages_10_x.jsonlint
        nodePackages_10_x.prettier
        nodePackages_10_x.typescript
        nodePackages_10_x.yarn
        nodejs-10_x
      ];
    }
  ];

  environment.systemPackages = with pkgs; [
    tokei # count lines of code
    gnumake
    gcc
    binutils # strip, ld, ...
    rustup
    shellcheck
  ];
}
