{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs) lib;

  dependencies = let nixosVersion = "20.03";
  in {
    nixpkgs = {
      ref = "refs/heads/nixos-${nixosVersion}";
      url = "https://github.com/NixOS/nixpkgs-channels.git";
      path = ".versions/nixpkgs.json";
    };
    nixpkgs-unstable = {
      ref = "refs/heads/nixos-unstable";
      url = "https://github.com/NixOS/nixpkgs-channels.git";
      path = ".versions/nixpkgs-unstable.json";
    };
    home-manager = {
      ref = "refs/heads/release-${nixosVersion}";
      url = "https://github.com/rycee/home-manager.git";
      path = ".versions/home-manager.json";
    };
    krops = {
      ref = "refs/tags/v1.21.0";
      url = "https://cgit.krebsco.de/krops";
      path = ".versions/krops.nix";
    };
    stockholm = {
      ref = "refs/heads/master";
      url = "https://cgit.krebsco.de/stockholm";
      path = ".versions/stockholm.json";
    };
  };

  updateScript = name: dependency: ''
    ${pkgs.nix-prefetch-git}/bin/nix-prefetch-git --url "${dependency.url}" --rev "${dependency.ref}" > "${toString ./.}/${dependency.path}"'';

  updateScripts =
    lib.mapAttrsToList
      (name: dependency: pkgs.writers.writeDashBin "niveum-update-${name}" (updateScript name dependency))
      dependencies;

  updateAllScript = pkgs.writers.writeDashBin "niveum-update" (lib.concatStringsSep " &\n" (lib.mapAttrsToList updateScript dependencies));
in pkgs.mkShell {

  buildInputs = updateScripts ++ [ updateAllScript ] ++ [
    (let
      deployCommand = pkgs.writers.writeDash "niveum-deploy-one" ''
        eval "$(${pkgs.nix}/bin/nix-build --no-out-link "${toString ./.}/deploy.nix" -A "$1")"
      '';
    in pkgs.writers.writeDashBin "niveum-deploy" ''
      ${pkgs.parallel}/bin/parallel --line-buffer --tagstring '{}' -q ${deployCommand} '{1}' ::: "$@"
    '')

    (pkgs.writers.writeDashBin "niveum-status" ''
      cd "${toString ./.}"

      version_file=/etc/niveum/version

      machine_status () {
        hostname="$1"

        if commit_id="$(${pkgs.openssh}/bin/ssh "$hostname" cat $version_file 2>/dev/null)"; then
          machine_status="$(${pkgs.git}/bin/git log -1 --oneline "$commit_id")"
        else
          machine_status=offline
        fi

        printf "\033[1m%11s\033[0m %s\n" "$hostname" "$machine_status"
      }

      for system in systems/*; do
        hostname="$(basename "$system")"
        machine_status "$hostname"
      done
    '')
  ];

  shellHook = "export HISTFILE=${toString ./.history}";
}
