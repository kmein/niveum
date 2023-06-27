{
  config,
  pkgs,
  inputs,
  ...
}: let
  inherit (import ../lib) localAddresses;
  living-room-id = 131090;
in {
  environment.systemPackages = [
    (pkgs.writers.writeDashBin "traadfri-party" ''
      while true; do
        for color in $(traadfri colours | shuf); do
          echo "$color"
          traadfri group "''${2:-${toString living-room-id}}" --on --colour="$color"
          sleep "''${1:-2}"
        done
      done
    '')
  ];

  age.secrets.traadfri-key = {
    file = inputs.secrets + "/traadfri-key.age";
    owner = config.users.users.me.name;
    group = config.users.users.me.group;
    mode = "400";
  };

  niveum.traadfri = {
    enable = true;
    user = "kmein";
    host = localAddresses.tradfri;
    keyFile = config.age.secrets.traadfri-key.path;
    rooms = {
      corridor = 131080;
      kitchen = 131081;
      bedroom = 131082;
      living-room = living-room-id;
      bedside = 131087;
      chain = 131089;
    };
  };
}
