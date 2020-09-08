{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs) lib;

  dependencies = let nixosVersion = "20.03"; in {
    nixpkgs = {
      ref = "refs/heads/nixos-${nixosVersion}";
      url = "https://github.com/NixOS/nixpkgs-channels.git";
      path = toString .versions/nixpkgs.json;
    };
    nixpkgs-unstable = {
      ref = "refs/heads/nixos-unstable";
      url = "https://github.com/NixOS/nixpkgs-channels.git";
      path = toString .versions/nixpkgs-unstable.json;
    };
    home-manager = {
      ref = "refs/heads/release-${nixosVersion}";
      url = "https://github.com/rycee/home-manager.git";
      path = toString .versions/home-manager.json;
    };
    krops = {
      ref = "refs/tags/v1.21.0";
      url = "https://cgit.krebsco.de/krops";
      path = toString .versions/krops.nix;
    };
    stockholm = {
      ref = "refs/heads/master";
      url = "https://cgit.krebsco.de/stockholm";
      path = toString .versions/stockholm.json;
    };
  };

  updateCommand = name: dependency: ''
    ${pkgs.nix-prefetch-git}/bin/nix-prefetch-git --url "${dependency.url}" --rev "${dependency.ref}" > "${dependency.path}"'';

  updateScripts =
    lib.mapAttrsToList
      (name: dependency: pkgs.writers.writeDashBin "niveum-update-${name}" (updateCommand name dependency))
      dependencies;

in pkgs.mkShell {

  buildInputs = updateScripts ++ [

    (pkgs.writers.writeDashBin "niveum-update"
      (lib.concatStringsSep " &\n" (lib.mapAttrsToList updateCommand dependencies)))

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

      for system in systems/*; do
        hostname="$(${pkgs.coreutils}/bin/basename "$system")"

        if commit_id="$(${pkgs.openssh}/bin/ssh "$hostname" cat $version_file 2>/dev/null)"; then
          machine_status="$(${pkgs.git}/bin/git log -1 --oneline "$commit_id")"
        else
          machine_status=offline
        fi

        printf "\033[1m%11s\033[0m %s\n" "$hostname" "$machine_status"
      done
    '')

  ];

  shellHook = "export HISTFILE=${toString ./.history}";
}
