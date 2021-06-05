{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) email-sshKey;
  much =
    let
      nixpkgs-much = import (pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "7c2a362b58a1c2ba72d24aa3869da3b1a91d39e1";
        sha256 = "0gl4xndyahasa9dv5mi3x9w8s457wl2xh9lcldizcn1irjvkrzs4";
      }) {
        overlays = [
          (import <stockholm/krebs/5pkgs/haskell>)
        ];
      };
      much-pkg = pkgs.haskellPackages.callCabal2nix "much" <niveum/submodules/much> {};
    in nixpkgs-much.haskell.lib.dontHaddock much-pkg;
in {
  environment.variables.NOTMUCH_CONFIG = config.home-manager.users.me.home.sessionVariables.NOTMUCH_CONFIG;

  users.users.me.openssh.authorizedKeys.keys = [ email-sshKey ];

  environment.systemPackages = [
    pkgs.notmuch-addrlookup

    pkgs.muchsync

    (pkgs.writers.writeDashBin "mua" ''
      if [ $# -eq 0 ]; then
        ${much}/bin/much-kmein
      else
        ${much}/bin/much-kmein -q "$*"
      fi
    '')

    (pkgs.writers.writeDashBin "mail-clean" ''
      ${pkgs.notmuch}/bin/notmuch search --output files --format=text0 tag:deleted | ${pkgs.findutils}/bin/xargs -r0 rm
      ${pkgs.notmuch}/bin/notmuch new
    '')
  ];

  home-manager.users.me = {
    programs.notmuch = {
      enable = true;
      search.excludeTags = [ "deleted" "spam" ];
    };

    programs.msmtp.enable = true;

    accounts.email.accounts = import ./accounts.nix { inherit lib; };
  };
}
