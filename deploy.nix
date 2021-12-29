{ pkgs, secrets, writeCommand, lib, nixosRebuildCommand ? "switch" }:
let
  sshPort = 22022;

  # command that ensures we use flake.nix during switch
  command = targetPath:
  let
    commandLine = "TMPDIR=/tmp nixos-rebuild ${nixosRebuildCommand} --flake ${targetPath} -L --keep-going";
  in
  ''
    echo '${commandLine}'
    nix-shell \
     -E "with import <nixpkgs> {}; mkShell { buildInputs = [ git (nixos { nix.package = nixFlakes; }).nixos-rebuild ]; }" \
     --run '${commandLine}'
  '';

  source = name: {
    niveum.file = toString ./.;
    system-secrets.pass = {
      dir = secrets;
      name = "systems/${name}";
    };
    secrets.pass = {
      dir = secrets;
      name = "shared";
    };
  };

  deploy = {name, host}: writeCommand "/bin/system" {
    source = lib.evalSource [ (source name) ];
    force = true;
    target = lib.mkTarget "root@${host}:${toString sshPort}/var/krops/niveum";
    inherit command;
  };
in {
  zaatar = deploy { name = "zaatar"; host = "zaatar.r"; };
  kabsa = deploy { name = "kabsa"; host = "kabsa.r"; };
  manakish = deploy { name = "manakish"; host = "manakish.r"; };
  makanek = deploy { name = "makanek"; host = "makanek.r"; };
}
