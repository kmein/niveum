{ config, pkgs, lib, ... }:
let
  nixpkgs-kmein = builtins.fetchGit {
    url = "https://github.com/kmein/nixpkgs";
    ref = "refs/heads/feature/moodle-dl-module";
    rev = "98ec084fe4dd9608769d592318d211dff3d32d86";
  };

  moodle-dl-package = (import nixpkgs-kmein {}).moodle-dl.overrideAttrs (old: old // {
    patches = old.patches ++ [ <niveum/packages/moodle-dl/telegram-format.patch> ];
  });
in
{
  imports = [ "${nixpkgs-kmein}/nixos/modules/services/networking/moodle-dl.nix" ];

  services.moodle-dl = {
    enable = true;
    startAt = "hourly";
    package = moodle-dl-package;
    config = {
      telegram = {
        token = lib.strings.fileContents <system-secrets/telegram/moodle-dl.token>;
        chat_id = "18980945";
        send_error_msg = true;
      };
      token = lib.strings.fileContents <system-secrets/moodle.token>;
      moodle_domain = "moodle.hu-berlin.de";
      moodle_path = "/";
      download_course_ids = [
        99881 # Dialektologie
        100183 # Onomastik
        100353 # Sanskrit I
        100692 # Sanskrit Tutorium
        99832 # Germanisch
        99823 # Gotisch
        99813 # Altalbanisch
        98681 # Geistliche Lyrik von Luther bis Lehnert
        99667 # Antike Mythologie
        # 52365 # FSR KlassPhil
      ];
      download_submissions = true;
      download_descriptions = true;
      download_links_in_descriptions = false;
      download_databases = false;
      download_forums = false;
      download_linked_files = false;
      download_also_with_cookie = false;
    };
  };

  services.syncthing = let moodle-dl-directory = config.services.moodle-dl.directory; in {
    enable = true;
    user = "moodle"; # config.users.extraUsers.moodle.name;
    openDefaultPorts = true;
    configDir = "${moodle-dl-directory}/.config/syncthing";
    dataDir = "${moodle-dl-directory}/.config/syncthing";
    declarative = rec {
      cert = toString <system-secrets/syncthing/cert.pem>;
      key = toString <system-secrets/syncthing/key.pem>;
      devices = {
        inherit ((import <niveum/lib>).syncthing.devices) wilde manakish toum;
      };
      folders.${moodle-dl-directory} = {
        devices = [ "toum" "wilde" "manakish" ];
        id = "moodle-dl";
      };
    };
  };
}
