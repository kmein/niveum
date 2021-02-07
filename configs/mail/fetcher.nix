{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) kieran;

  tagRules = [
    {
      query = "to:miaengiadina-pwa@noreply.github.com AND subject:\"PR run failed\"";
      tags = [ "-new" "+deleted" ];
    }
    {
      query = lib.concatStringsSep " OR " [
        "from:noreply-local-guides@google.com"
        "from:google-maps-noreply@google.com"
        "subject:fd-noti"
        "from:nebenan.de"
        "to:miaengiadina-pwa@noreply.github.com"
      ];
      tags = [ "-new" ];
    }
    {
      query = "tag:new";
      tags = [ "-new" "+inbox" ];
    }
  ];

  generateTaggingScript = filters:
    let
      template = { tags, query, message ? "tagging ${query} -> ${lib.concatStringsSep " " tags}", ... }: ''
        echo '${message}'
        ${pkgs.notmuch}/bin/notmuch tag ${lib.concatStringsSep " " tags} -- "${query}"
      '';
    in lib.concatStringsSep "\n" (map template filters);
in
{
  users.users.email = {
    isNormalUser = true;
    description = "fetching mails since 2021";
    openssh.authorizedKeys.keys = kieran.sshKeys pkgs;
    packages = [ pkgs.muchsync ];
  };

  environment.variables.NOTMUCH_CONFIG = config.home-manager.users.email.home.sessionVariables.NOTMUCH_CONFIG;

  systemd.services.mail-sync = {
    enable = true;
    wants = [ "network-online.target" ];
    startAt = "*:0/15";
    serviceConfig.User = config.users.users.email.name;
    serviceConfig.Type = "oneshot";
    environment.NOTMUCH_CONFIG = config.home-manager.users.email.home.sessionVariables.NOTMUCH_CONFIG;
    script = ''
      ${pkgs.isync}/bin/mbsync --all
      ${pkgs.notmuch}/bin/notmuch new
    '';
  };

  home-manager.users.email = {
    programs.mbsync.enable = true;

    accounts.email.accounts = import ./accounts.nix { inherit lib; };

    programs.notmuch = {
      enable = true;
      new.tags = [ "new" ];
      hooks.postNew = generateTaggingScript tagRules;
    };
  };
}
