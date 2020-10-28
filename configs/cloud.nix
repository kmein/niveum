{ config, lib, pkgs, ... }: {
  imports = [
    <niveum/modules/seafile.nix>
    <niveum/modules/dropbox.nix>
  ];

  niveum = {
    dropbox.enable = true;
    seafile.enable = true;
  };

  system.activationScripts.home-symlinks = ''
    ln -sfn ${config.users.users.me.home}/cloud/syncthing/common/mahlzeit ${config.users.users.me.home}/mahlzeit
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Wiki ${config.users.users.me.home}/notes
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Uni ${config.users.users.me.home}/uni
  '';

  home-manager.users.me = {
    services.nextcloud-client.enable = true;
  };

  services.syncthing = rec {
    enable = true;
    user = "kfm";
    openDefaultPorts = true;
    configDir = "/home/kfm/.config/syncthing";
    dataDir = "/home/kfm/.config/syncthing";
    declarative = rec {
      cert = toString <system-secrets/syncthing/cert.pem>;
      key = toString <system-secrets/syncthing/key.pem>;
      devices = {
        homeros.id =
          "HSOL72W-MMN346W-C3WCWCH-OTRKJYG-MY2WWV6-P7JUHN7-5WYYYRV-ZMH4KAA";
        rilke.id =
          "NYNNHXP-7JMSTXG-SVNOPWD-RWXCCCL-CBOVBEI-X4QPLF4-NJA5G2P-RSGYRQQ";
        wilde.id =
          "R6DEBD7-G5RYDKN-VFA3HPO-WX4DNVI-373F7OQ-AW5MZTT-3L4BDVW-Y6ROEAF";
        heym.id =
          "HLQSG3D-WSKLA6S-MEYQ3EU-GDBGABE-PY53RQ6-SWQAP2I-Z5MVBVX-MYPJXAM";
      };
      folders =
        let syncthing-dir = "${config.users.users.me.home}/cloud/syncthing";
        in {
          "${syncthing-dir}/common".devices =
            [ "homeros" "wilde" ];
          "${syncthing-dir}/library".devices = lib.attrNames devices;
          "${syncthing-dir}/mundoiu".devices = lib.attrNames devices;
          "${syncthing-dir}/music".devices = lib.attrNames devices;
        };
    };
  };
}
