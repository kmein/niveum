{ yarn2nix-moretea }:
yarn2nix-moretea.mkYarnPackage {
  name = "devanagari";
  src = ./.;
  packageJson = ./package.json;
  yarnLock = ./yarn.lock;
}
