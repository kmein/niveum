{ config, pkgs, lib, ... }:
let
  moodle-dl-package = pkgs.moodle-dl.overrideAttrs (old: old // {
    patches = [ <niveum/packages/moodle-dl/telegram-format.patch> ];
  });
in
{
  imports = [ <niveum/modules/moodle-dl.nix> ];

  services.moodle-dl = {
    enable = true;
    startAt = "hourly";
    package = moodle-dl-package;
    settings = {
      telegram = {
        token = lib.strings.fileContents <system-secrets/telegram/moodle-dl.token>;
        chat_id = "18980945";
        send_error_msg = false;
      };
      token = lib.strings.fileContents <system-secrets/moodle.token>;
      moodle_domain = "moodle.hu-berlin.de";
      moodle_path = "/";
      download_course_ids = [
        # WS 2020
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

        # SS 2021
        104850 # Metrik
        103274 # Marc Aurel
        102909 # Sanskrit II
        104937 # Altirisch
        104925 # Gradierung und Komparation
        105264 # Was andere Sprachen anders machen
        104991 # Warum klingt Orkisch böse
        105074 # Litauisch
        103685 # Griechische Sprache und Übersetzung I
        105455 # Elegia greca
        105335 # Homerische Epen
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
    user = "moodle-dl"; # config.users.extraUsers.moodle.name;
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
