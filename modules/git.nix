{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.niveum.git;

  repositoryService = name: repository: nameValuePair "git-repository-${name}" {
    enable = repository.enable;
    startAt = "*/3:0";
    wants = [ (if (repository.autoFetch != {}) then "network-online.target" else "multi-user.target") ];
    after = [ (if (repository.autoFetch != {}) then "network-online.target" else "multi-user.target") ];
    serviceConfig = {
      Type = "oneshot";
      RandomizedDelaySec = "300";
    };
    script =
    let
      branchSetup = flip map repository.branches (branch: ''
        ${pkgs.git}/bin/git branch ${branch}
      '');
      remoteSetup = concatStringsSep "\n" (flip mapAttrsToList repository.remotes (name: url: ''
        ${pkgs.git}/bin/git remote add ${name} ${url}
      ''));
      fetch = concatStringsSep "\n" (flip map repository.autoFetch ({ branch, remote }: ''
        ${pkgs.git}/bin/git fetch ${remote} ${branch}
      ''));
      repositorySetup = ''
        if [ ! -d ${repository.location}/.git ]; then
          ${pkgs.git}/bin/git clone ${repository.remotes.origin} ${repository.location}
        fi
      '';
    in ''
      ${repositorySetup}
      cd ${repository.location}

      ${branchSetup}
      ${remoteSetup}
      ${fetch}
    '';
  };
in {
  options.niveum.git = {
    enable = mkEnableOption "declarative repository management";
    repositories = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          enable = mkEnableOption "git repository";
          location = mkOption { type = types.path; };
          remotes = mkOption { type = types.attrsOf types.path; default = {}; };
          branches = mkOption { type = types.listOf types.str; default = []; };
          autoFetch = mkOption {
            type = types.listOf (types.submodule {
              options = {
                branch = mkOption { type = types.str; };
                remote = mkOption { type = types.str; };
              };
            });
            default = {};
          };
        };
      });
      default = {};
    };
  };

  config = mkIf cfg.enable {
    systemd.services = attrsets.mapAttrs' repositoryService cfg.repositories;
  };
}
