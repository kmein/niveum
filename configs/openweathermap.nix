{
  pkgs,
  lib,
  ...
}:
let
  openweathermap-repo = pkgs.fetchFromGitHub {
    owner = "ip1981";
    repo = "openweathermap";
    rev = "9cfef7b14ac5af7109449b54b1cb352b4c76167a";
    sha256 = "0sm43wicvw2fy7nq65s8vch6jjb5bszqr4ilnhibayamj4jcpw53";
  };
  openweathermap = pkgs.haskellPackages.callCabal2nix "openweathermap" openweathermap-repo { };
  openweathermap-key = lib.strings.fileContents <secrets/openweathermap.key>;
in
{
  nixpkgs.config.packageOverrides = pkgs: {
    weather = pkgs.writers.writeDashBin "weather" ''
      ${openweathermap}/bin/openweathermap --api-key ${openweathermap-key} "$@"
    '';
  };

  environment.systemPackages = [ pkgs.weather ];
}
