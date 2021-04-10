{ config, pkgs, lib, ... }:
let
  nixpkgs-kmein = builtins.fetchGit {
    url = "https://github.com/kmein/nixpkgs";
    ref = "refs/heads/feature/moodle-dl-module";
    rev = "ea2e0387ee946e575f7851ec21debc9179d82ad0";
  };

  moodle-dl-package = (import nixpkgs-kmein {}).moodle-dl.overrideAttrs (old: old // {
    patches = [ <niveum/packages/moodle-dl/telegram-format.patch> ];
  });
in
{
  imports = [ "${nixpkgs-kmein}/nixos/modules/services/networking/moodle-dl.nix" ];

  services.moodle-dl = {
    enable = true;
    startAt = "hourly";
    package = moodle-dl-package;
    notifyOnly = true;
    settings = {
      telegram = {
        token = lib.strings.fileContents <system-secrets/telegram/moodle-dl.token>;
        chat_id = "311425510";
        send_error_msg = false;
      };
      token = lib.strings.fileContents <system-secrets/moodle-dl/faye.token>;
      moodle_domain = "moodle.hu-berlin.de";
      moodle_path = "/";
    };
  };
}
