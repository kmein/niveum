{ pkgs, ... }:
let
  secrets = import <dot/secrets.nix>;
in {
  imports = [ <modules/todoist.nix> ];

  niveum.todoist = {
    enable = true;
    token = secrets.todoist.token;
  };

  programs.chromium.extensions = [
    "jldhpllghnbhlbpcmnajkpdmadaolakh" # Todoist
  ];
}
