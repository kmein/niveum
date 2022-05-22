{
  config,
  pkgs,
  lib,
  ...
}: let
  moodle-dl-package = pkgs.moodle-dl.overrideAttrs (old:
    old
    // {
      patches = [<niveum/packages/moodle-dl/telegram-format.patch>];
    });
in {
  imports = [<niveum/modules/moodle-dl.nix>];

  niveum.passport.services = [
    {
      title = "MoodleDL";
      description = "notifies about changes on Moodle.";
    }
  ];

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
