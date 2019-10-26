{ pkgs, ... }:
let
  secrets = import <niveum/dot/secrets.nix>;
in {
  imports = [ <niveum/modules/todoist.nix> ];

  niveum.todoist = {
    enable = true;
    token = secrets.todoist.token;
  };

  programs.chromium.extensions = [
    "jldhpllghnbhlbpcmnajkpdmadaolakh" # Todoist
  ];
}
