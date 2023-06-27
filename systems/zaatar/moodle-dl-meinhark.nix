{
  config,
  pkgs,
  lib,
  ...
}: let
  moodle-dl-package = pkgs.moodle-dl.overrideAttrs (old:
    old
    // {
      # patches = [../../packages/moodle-dl/telegram-format.patch]; TODO?
    });
in {
  age.secrets = {
    /*
    moodle-dl-tokens = {
      file = ../../secrets/zaatar-moodle-dl-tokens.json.age;
      owner = "moodle-dl";
      group = "moodle-dl";
      mode = "400";
    };
    */
    moodle-dl-basicAuth = {
      file = ../../secrets/zaatar-moodle-dl-basicAuth.age;
      owner = "nginx";
      group = "nginx";
      mode = "400";
    };
  };

  services.moodle-dl = {
    enable = false;
    startAt = "hourly";
    package = moodle-dl-package;
    tokensFile = config.age.secrets.moodle-dl-tokens.path;
    settings = {
      telegram = {
        chat_id = "18980945";
        send_error_msg = false;
      };
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

        # WS 2021
        108122 # Griechisch
        107986 # Altostslavisch
        107792 # Elegie in Rom
        107369 # Tusculanae Disputationes
        108586 # Griechische Religion
        107988 # Balkanindogermanisch
        108312 # Altnordisch
        107281 # NLP
        108736 # Grammatiktheorie
        109438 # Warum klingt Orkisch böse
        108025 # Poetik der Nordgermanen
        107280 # Statistik
        108350 # Attische Redner
        107461 # Argonautika
        108283 # Digital Classicist
        109211 # Altlitauisch
        109185 # Etymologie

        # SS 2022
        112606 # Avestisch
        111761 # Griechische Wissenschaftsliteratur
        111515 # H. Furens
        110914 # Apostelgeschichte
        112225 # Gr. Paläographie
        113275 # ALEW
        112783 # Akzent und Silbenstruktur
        113493 # Papyrologie

        # WS 2022
        115414 # Nonnos
        116108 # Dialektologie

        # SS 2023
        117967 # Archaische Lyrik
        119658 # Dyskolos
        118963 # Antike Biographie
        92668 # Taa
        120671 # Jiddisch
        120720 # Sorbisch
        118076 # X-Tutorial
        120631 # Predigten
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

  fileSystems."/export/moodle" = {
    device = config.services.moodle-dl.directory;
    options = ["bind"];
  };

  networking.firewall.allowedTCPPorts = [2049];

  services.nginx.enable = true;

  services.nginx.virtualHosts."moodle.kmein.r" = {
    basicAuthFile = config.age.secrets.moodle-dl-basicAuth.path;
    locations."/" = {
      root = config.services.moodle-dl.directory;
      extraConfig = ''
        autoindex on;
        charset UTF-8;
      '';
    };
  };

  services.nfs.server = {
    enable = true;
    exports = let
      machines = with (import ../../lib).retiolumAddresses; [kabsa manakish];
    in ''
      /export        ${lib.concatMapStringsSep " " (machine: "${machine.ipv4}(fsid=0)") machines}
      /export/moodle ${lib.concatMapStringsSep " " (machine: "${machine.ipv4}(insecure,rw)") machines}
    '';
  };
}
