{ config, pkgs, lib, ... }:
let inherit (import <lib> { inherit lib; }) strip;
in
{
  environment.systemPackages = [
    pkgs.notmuch
    pkgs.offlineimap
    pkgs.msmtp
    pkgs.alot
  ];

  home-manager.users.me = {
    accounts.email.maildirBasePath = "${config.users.users.me.home}/mail";
    accounts.email.accounts.hu-berlin = {
      address = "meinhark@hu-berlin.de";
      userName = "meinhark";
      realName = config.niveum.user.name;
      imap = {
        host = "mailbox.cms.hu-berlin.de";
        port = 993;
        tls.enable = true;
      };
      smtp = {
        host = "mailhost.cms.hu-berlin.de";
        port = 25;
        tls.enable = true;
      };
      msmtp.enable = true;
      notmuch.enable = true;
      passwordCommand = "echo '${strip (builtins.readFile <shared-secrets/eduroam/password>)}'";
      offlineimap = {
        enable = true;
        postSyncHookCommand = "notmuch new";
      };
      primary = true;
    };

    programs.offlineimap = {
      enable = true;
    };

    programs.alot.enable = true;
  };
}
