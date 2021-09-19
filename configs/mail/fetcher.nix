{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) kieran sshPort;

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
  imports = [ <stockholm/krebs/3modules/secret.nix> ];

  krebs.secret.files.email-ssh = {
    path = "${config.users.users.email.home}/.ssh/id_ed25519";
    owner.name = config.users.users.email.name;
    source-path = toString <system-secrets> + "/email/ssh.key";
  };

  users.users.email = {
    isNormalUser = true;
    description = "fetching mails since 2021";
  };

  systemd.services.mail-sync =
  let
    hosts = [ "manakish.r" "kabsa.r" ];
  in {
    enable = true;
    wants = [ "network-online.target" config.krebs.secret.files.email-ssh.service ];
    startAt = "*:0/3";
    serviceConfig.User = config.users.users.email.name;
    serviceConfig.Type = "oneshot";
    environment.NOTMUCH_CONFIG = config.home-manager.users.email.home.sessionVariables.NOTMUCH_CONFIG;
    path = [ pkgs.notmuch pkgs.openssh ];
    script = ''
      ${pkgs.isync}/bin/mbsync --all || true

      ${lib.concatMapStringsSep "\n" (host: ''
        echo === syncing ${host}
        ${pkgs.muchsync}/bin/muchsync -s 'ssh -CTaxq -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o ConnectTimeout=4 -p ${toString sshPort}' kfm@${host} || :
      '') hosts}
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
