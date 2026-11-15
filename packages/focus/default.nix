{ lib, rustPlatform }:
# the program behind modules/focus.nix: applies the schedule (nft table, state file)
# and serves the systemd-resolved hook that blocks the listed domains
rustPlatform.buildRustPackage {
  pname = "focus";
  version = "0.1.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
    ];
  };
  cargoLock.lockFile = ./Cargo.lock;
  meta = {
    description = "Scheduled site and internet blocking through systemd-resolved hooks and nftables";
    mainProgram = "focus";
    # systemd-resolved and nftables, so Linux only
    platforms = lib.platforms.linux;
  };
}
