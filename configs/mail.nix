{ config, pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.notmuch
    pkgs.offlineimap
    pkgs.msmtp
    pkgs.neomutt
  ];
  home-manager.users.me = {
    programs.astroid = {
      enable = true;
      externalEditor = "urxvt -embed %3 -e nvim %1";
      pollScript = "offlineimap";
    };

    accounts.email.maildirBasePath = "${config.users.users.me.home}/mail";

    accounts.email.accounts.amroplay = {
      astroid.enable = true;
      address = "amroplay@gmail.com";
      userName = "amroplay";
      flavor = "gmail.com";
      realName = config.niveum.user.name;
      msmtp.enable = true;
      notmuch.enable = true;
      offlineimap = {
        enable = true;
        postSyncHookCommand = "notmuch new";
      };
    };

    accounts.email.accounts.hu-berlin = {
      astroid.enable = true;
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
      offlineimap = {
        enable = true;
        postSyncHookCommand = "notmuch new";
      };
      primary = true;
    };

    programs.offlineimap.enable = true;
  };
}
