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
}
