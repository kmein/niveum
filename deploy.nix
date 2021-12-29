let
  inherit (import ./lib/default.nix) sshPort;

  krops = builtins.fetchGit { url = "https://cgit.krebsco.de/krops/"; };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" { };

  source = name: lib.evalSource [{
    niveum.file = toString ./.;
    system-secrets.pass = {
      dir = toString ~/.password-store;
      name = "systems/${name}";
    };
    secrets.pass = {
      dir = toString ~/.password-store;
      name = "shared";
    };
  }];

  command = targetPath: ''
    nix-shell -p git --run '
      nixos-rebuild switch -v --show-trace --flake ${targetPath}/niveum || \
        nixos-rebuild switch -v --flake ${targetPath}/niveum
    '
  '';

  createHost = name: target: pkgs.krops.writeCommand "deploy-${name}" {
    source = source name;
    inherit command target;
  };
in rec {
  zaatar = createHost "zaatar" "root@zaatar.r:${toString sshPort}";
  kabsa = createHost "kabsa" "root@kabsa.r:${toString sshPort}";
  makanek = createHost "kabsa" "root@makanek.r:${toString sshPort}";
  manakish = createHost "kabsa" "root@manakish.r:${toString sshPort}";
  all = pkgs.writeScript "deploy-all"
    (lib.concatStringsSep "\n" [ zaatar kabsa makanek manakish ]);
}
