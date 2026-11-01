{
  config,
  lib,
  pkgs,
  ...
}:
let
  username = "meinhak99";
in
{
  home-manager.users.me = {
    programs.ssh = {
      settings = {
        fu-berlin = {
          user = username;
          hostname = "login.zedat.fu-berlin.de";
        };
      };
    };
  };

  niveum.mail.users.me.accounts = {
    letos = {
      address = "letos.sprachlit@hu-berlin.de";
      userName = "slfletos";
      passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-letos.path}";
      imap.host = "mailbox.cms.hu-berlin.de";
      smtp = {
        host = "mailhost.cms.hu-berlin.de";
        port = 25;
        starttls = true;
      };
    };
    fu = {
      address = "kieran.meinhardt@fu-berlin.de";
      aliases = [ "${username}@fu-berlin.de" ];
      userName = username;
      passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-meinhak99.path}";
      imap.host = "mail.zedat.fu-berlin.de";
      smtp.host = "mail.zedat.fu-berlin.de";
      folders = {
        drafts = "Entwürfe";
        sent = "Gesendet";
      };
      aerc.pgp-key-id = pkgs.lib.niveum.kieran.pgpFingerprint;
    };
  };

  age.secrets = {
    email-password-meinhak99 = {
      file = ../secrets/email-password-meinhak99.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    email-password-letos = {
      file = ../secrets/email-password-letos.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "hu-vpn-split" ''
      ${pkgs.openfortivpn}/bin/openfortivpn \
        --password="$(cat "${config.age.secrets.email-password-letos.path}")" \
        --config=${pkgs.writeText "hu-berlin-split.config" ''
          host = forti-ssl.vpn.hu-berlin.de
          port = 443
          username = slfletos@split_tunnel
        ''}
    '')
    (pkgs.writers.writeDashBin "hu-vpn-full" ''
      ${pkgs.openfortivpn}/bin/openfortivpn \
        --password="$(cat "${config.age.secrets.email-password-letos.path}")" \
        --config=${pkgs.writeText "hu-berlin-full.config" ''
          host = forti-ssl.vpn.hu-berlin.de
          port = 443
          username = slfletos@tunnel_all
        ''}
    '')
    (pkgs.writers.writeDashBin "fu-vpn" ''
      if [ "$(${pkgs.wirelesstools}/bin/iwgetid --raw)" = eduroam ]; then
        echo "on eduroam, no vpn needed" >&2
        exit 0
      fi
      # saml login runs in the user's browser; only the tunnel needs root
      eval "$(${lib.getExe pkgs.openconnect} --useragent=AnyConnect --authenticate \
        --external-browser=${lib.getExe' pkgs.xdg-utils "xdg-open"} vpn.fu-berlin.de)"
      [ -n "$COOKIE" ] || exit 1
      echo "$COOKIE" | sudo ${lib.getExe pkgs.openconnect} --useragent=AnyConnect --cookie-on-stdin \
        --servercert "$FINGERPRINT" --resolve "$RESOLVE" "$CONNECT_URL"
    '')
  ];
}
