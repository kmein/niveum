{
  localAddresses = {
    toum = "192.168.178.24";
    scardanelli = "192.168.178.21";
    homeros = "192.168.178.22";
    wilde = "192.168.178.32";
    android = "192.168.178.35";

    tradfri = "192.168.178.28";
    officejet = "192.168.178.27";
    fritzbox = "192.168.178.1";
  };

  nixpkgs-unstable = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "4512dac960f3833cf24cdbd742b63cb447bbdd9a";
  };
}
