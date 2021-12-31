{ inputs, system, name }:
let
  inherit (inputs) nixpkgs;
  pkgs = nixpkgs.legacyPackages.${system};
  ensureFiles = paths: pkgs.runCommand "directory" {} ''
    set -efu
    mkdir $out
    cd $out
    ${nixpkgs.lib.concatMapStringsSep "\n" (path: ''
      mkdir -p "$(dirname ${nixpkgs.lib.escapeShellArg path})"
      echo foo > ${nixpkgs.lib.escapeShellArg path}
    '') paths}
  '';
  nixPath = nixpkgs.lib.concatStringsSep ":" [
    "nixos-config=${toString ./.}/systems/${name}/configuration.nix"
    "niveum=${toString ./.}"
    "nixpkgs=${inputs.nixpkgs}"
    "nixpkgs-unstable=${inputs.nixpkgs-unstable}"
    "stockholm=${inputs.stockholm}"
    "home-manager=${inputs.home-manager}"
    "nix-writers=${inputs.nix-writers}"
    "retiolum=${inputs.retiolum}"
    "menstruation-backend=${inputs.menstruation-backend}"
    "menstruation-telegram=${inputs.menstruation-telegram}"
    "system-secrets=${systemSecrets}"
    "secrets=${sharedSecrets}"
  ];
  # cd ~/.password-store/shared && find * -type f | sed 's/.gpg$//'
  sharedSecrets = ensureFiles [
    "di.fm/key"
    "eduroam/identity"
    "eduroam/password"
    "fritznas.smb"
    "mail/cock"
    "mail/fastmail"
    "mail/gmail/amroplay"
    "mail/gmail/kieran.meinhardt"
    "mail/meinhaki"
    "mail/meinhaki.cert"
    "mail/posteo"
    "nextcloud-fysi/password"
    "nextcloud/password"
    "openweathermap.key"
    "posteo/password"
    "spotify/password"
    "spotify/username"
    "traadfri.key"
    "wifi/Aether.psk"
  ];
  systemSecrets = let basic = [ "retiolum.ed25519" "retiolum.key" "syncthing/cert.pem" "syncthing/key.pem"]; in {
    zaatar = ensureFiles ([ "moodle.token" "telegram/moodle-dl.token" ] ++ basic);
    kabsa = ensureFiles basic;
    manakish = ensureFiles basic;
    makanek = ensureFiles ([
      "irc/retiolum"
      "irc/hackint"
      "irc/libera"
      "irc/oftc"
      "matrix/nibbana"
      "maxmind/license.key"
      "moodle-dl/faye.token"
      "nextcloud/admin"
      "nextcloud/database"
      "telegram/nachtischsatan.token"
      "telegram/reverse.token"
      "telegram/odyssey.token"
      "telegram/betacode.token"
      "telegram/moodle-dl.token"
      "telegram/proverb.token"
      "telegram/menstruation.token"
      "telegram/cool_village.token"
      "telegram/kmein.token"
      "telegram/prometheus.token"
      "weechat/relay"
    ] ++ basic);
  }.${name};
in toString (pkgs.writers.writeDash "build" "NIX_PATH=${nixPath} nix-build '<nixpkgs/nixos>' -A system --dry-run")
