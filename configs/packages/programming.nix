{ pkgs, ... }:
let
  nightly = pkgs.rustChannelOf {
    date = "2019-12-27";
    channel = "nightly";
  };
in {
  imports = [
    {
      environment.systemPackages = with pkgs; [
        htmlTidy
        nodePackages.csslint
        nodePackages.jsonlint
        nodePackages.prettier
        nodePackages.typescript
        nodePackages.yarn
        nodejs
        nodePackages.javascript-typescript-langserver
      ];
    }
  ];

  environment.systemPackages = with pkgs; [
    tokei # count lines of code
    gnumake
    gcc
    binutils # strip, ld, ...
    # rustup
    nightly.rust
    shellcheck
  ];
}
