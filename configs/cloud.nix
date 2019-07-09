{ config, lib, pkgs, ... }:
{
  imports = [
    <modules/seafile.nix>
    <modules/google-drive.nix>
    <modules/dropbox.nix>
    <stockholm/krebs/3modules/syncthing.nix>
  ];

  niveum.dropbox = {
    enable = true;
    user = config.users.users.me;
  };

  niveum.seafile = {
    enable = true;
    user = config.users.users.me;
  };

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
    key = toString <secrets/syncthing/key.pem>;
    cert = toString <secrets/syncthing/cert.pem>;
    peers = {
      homeros.id = "HSOL72W-MMN346W-C3WCWCH-OTRKJYG-MY2WWV6-P7JUHN7-5WYYYRV-ZMH4KAA";
      scardanelli.id = "7CZYHEX-3CSFDQU-PEEMYHG-6XGQ2ZD-KGVUWH5-GFRB2XK-FP57ERX-7APZUQU";
      rilke.id = "NYNNHXP-7JMSTXG-SVNOPWD-RWXCCCL-CBOVBEI-X4QPLF4-NJA5G2P-RSGYRQQ";
      wilde.id = "R6DEBD7-G5RYDKN-VFA3HPO-WX4DNVI-373F7OQ-AW5MZTT-3L4BDVW-Y6ROEAF";
    };
    folders."${config.users.users.me.home}/cloud/syncthing/common".peers = [ "homeros" "scardanelli" "wilde" ];
    folders."${config.users.users.me.home}/cloud/syncthing/library".peers = lib.attrNames peers;
    folders."${config.users.users.me.home}/cloud/syncthing/mundoiu".peers = lib.attrNames peers;
  };
}
