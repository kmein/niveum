{ config, pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.notmuch
    pkgs.offlineimap
    pkgs.msmtp
    pkgs.neomutt
  ];

  home-manager.users.me = {
    accounts.email.maildirBasePath = "${config.users.users.me.home}/mail";
    accounts.email.accounts.hu-berlin = {
      address = "meinhark@hu-berlin.de";
      userName = "meinhark";
      realName = config.niveum.user.fullName;
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
      offlineimap = {
        enable = true;
        postSyncHookCommand = "notmuch new";
      };
      primary = true;
    };

    programs.offlineimap = {
      enable = true;
    };
  };
}
