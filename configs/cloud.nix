{ config, lib, pkgs, ... }:
{
  imports = [
    <modules/seafile.nix>
    <modules/google-drive.nix>
    <modules/dropbox.nix>
    <stockholm/krebs/3modules/syncthing.nix>
  ];

  niveum.dropbox.enable = true;

  niveum.seafile.enable = true;

  niveum.google-drive = rec {
    enable = true;
    directory = "${user.home}/cloud/gdrive";
    user = config.users.users.me;
  };

  services.syncthing = rec {
    enable = true;
    user = "kfm";
    openDefaultPorts = true;
    configDir = "/home/kfm/.config/syncthing";
    dataDir = "/home/kfm/.config/syncthing";
  };

  krebs.syncthing = rec {
    enable = true;
    id = config.networking.hostName;
    peers = {
      homeros.id = "TGVJKSM-5P7YP4E-OCYDB6S-LXQ3PNM-RP6BNBS-2UNYKKX-YJCMWAF-NGWQFA2";
      scardanelli.id = "XEQUNNZ-FQ67ASA-4DWBKAO-RQD2PTK-B6J74TT-RQPBVDE-SRNOSMF-UUAUMAK";
      rilke.id = "NYNNHXP-7JMSTXG-SVNOPWD-RWXCCCL-CBOVBEI-X4QPLF4-NJA5G2P-RSGYRQQ";
    };
    folders."${config.users.users.me.home}/cloud/syncthing/common".peers = [ "homeros" "scardanelli" ];
    folders."${config.users.users.me.home}/cloud/syncthing/library".peers = lib.attrNames peers;
    folders."${config.users.users.me.home}/cloud/syncthing/mundoiu".peers = lib.attrNames peers;
  };
}
