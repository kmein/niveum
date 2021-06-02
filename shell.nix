{ pkgs ? import <nixpkgs> { }
, release ? "20.09"
}:
let
  inherit (pkgs) lib;

  dependencies = {
    nixpkgs = {
      ref = "refs/heads/nixos-${release}";
      url = "https://github.com/NixOS/nixpkgs.git";
      path = toString .versions/nixpkgs.json;
    };
    nixpkgs-unstable = {
      ref = "refs/heads/master";
      url = "https://github.com/NixOS/nixpkgs.git";
      path = toString .versions/nixpkgs-unstable.json;
    };
    home-manager = {
      ref = "refs/heads/release-${release}";
      url = "https://github.com/nix-community/home-manager.git";
      path = toString .versions/home-manager.json;
    };
    krops = {
      ref = "refs/tags/v1.25.0";
      url = "https://cgit.krebsco.de/krops";
      path = toString .versions/krops.json;
    };
    stockholm = {
      ref = "refs/heads/master";
      url = "https://github.com/kmein/stockholm";
      path = toString .versions/stockholm.json;
    };
    retiolum = {
      ref = "refs/heads/master";
      url = "https://github.com/krebs/retiolum";
      path = toString .versions/retiolum.json;
    };
    nur = {
      ref = "refs/heads/master";
      url = "https://github.com/nix-community/NUR";
      path = toString .versions/nur.json;
    };
  };
in pkgs.mkShell {
  buildInputs = [
    (let
      updateCommand = pkgs.writers.writeDash "niveum-update-one" ''
        [ $# -eq 1 ] || {
          echo "Please provide a dependency to update." >&2
          exit 1
        }
        case "$1" in
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: dependency: ''
            ${name})
              ${pkgs.nix-prefetch-git}/bin/nix-prefetch-git --url "${dependency.url}" --rev "${dependency.ref}" > "${dependency.path}";;
          '') dependencies)}
        esac
      '';
    in pkgs.writers.writeDashBin "niveum-update" ''
      if [ $# -gt 0 ]; then
        dependencies="$@"
      else
        dependencies="${lib.concatStringsSep " " (lib.attrNames dependencies)}"
      fi
      ${pkgs.parallel}/bin/parallel --line-buffer --tagstring '{}' -q ${updateCommand} '{1}' ::: $dependencies
    '')

    (let
      deployCommand = pkgs.writers.writeDash "niveum-deploy-one" ''
        eval "$(${pkgs.nix}/bin/nix-build --no-out-link "${toString ./.}/deploy.nix" -A "$1")"
      '';
    in pkgs.writers.writeDashBin "niveum-deploy" ''
      if [ -z "$(${pkgs.git}/bin/git status --porcelain)" ]; then
        ${pkgs.parallel}/bin/parallel --line-buffer --tagstring '{}' -q ${deployCommand} '{1}' ::: "$@"
      else
        echo Working directory is dirty. Not deploying.
        exit 1
      fi
    '')

    (let
      statusCommand = pkgs.writers.writeDash "niveum-status-one" ''
        [ $# -eq 1 ] || {
          echo "Please provide a niveum system hostname." >&2
          exit 1
        }

        hostname="$1"
        version_file=/etc/niveum/version

        if commit_id="$(${pkgs.coreutils}/bin/timeout 2s ${pkgs.openssh}/bin/ssh "$hostname" cat $version_file 2>/dev/null)"; then
          ${pkgs.git}/bin/git log -1 --oneline "$commit_id"
        else
          echo offline
        fi
      '';
    in pkgs.writers.writeDashBin "niveum-status" ''
      if [ $# -gt 0 ]; then
        systems="$@"
      else
        systems="$(ls ${toString ./.}/systems)"
      fi
      ${pkgs.parallel}/bin/parallel --line-buffer --tagstring '{}' -q ${statusCommand} '{1}' ::: $systems
    '')
  ];
}
