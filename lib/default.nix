{
  nixpkgs-unstable = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "4512dac960f3833cf24cdbd742b63cb447bbdd9a";
  };

  sshPort = 22022;

  colours = import ./colours/mac-os.nix;

  theme = pkgs: {
    gtk = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.gnome-themes-extra;
    };
    icon = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    cursor = {
      name = "capitaine-cursors-white";
      package = pkgs.capitaine-cursors;
    };
  };

  defaultApplications = import ./default-applications.nix;

  localAddresses = import ./local-network.nix;

  kieran = {
    github = "kmein";
    email = "kmein@posteo.de";
    name = "Kierán Meinhardt";
    sshKeys = pkgs: pkgs.lib.strings.splitString "\n" (pkgs.lib.strings.fileContents (pkgs.fetchurl {
      url = "https://github.com/kmein.keys";
      sha256 = "09c6ny0rmpid1m0pc1wsmb3wyy9g721lf4kv55i4lrp42b3i2d5b";
    }));
  };

  ignorePaths = [
    "*~"
    ".stack-work/"
    "__pycache__/"
    ".mypy_cache/"
    "*.py[co]"
    "*.o"
    "*.hi"
    "*.aux"
    "*.bbl"
    "*.bcf"
    "*.blg"
    "*.fdb_latexmk"
    "*.fls"
    "*.out"
    "*.run.xml"
    "*.toc"
    "*.bbl"
    "*.class"
    "*.dyn_hi"
    "*.dyn_o"
    "dist/"
    ".envrc"
    ".direnv/"
    "dist-newstyle/"
    ".history"
  ];
}
