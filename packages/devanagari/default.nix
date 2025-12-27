{ yarn2nix-moretea, lib }:
yarn2nix-moretea.mkYarnPackage {
  name = "devanagari";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./devanagari.js
      ./package.json
    ];
  };
  packageJson = ./package.json;
  yarnLock = ./yarn.lock;
}
