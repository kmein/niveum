rec {
  tmpfilesConfig = {
    type,
    path,
    mode ? "-",
    user ? "-",
    group ? "-",
    age ? "-",
    argument ? "-",
  }: "${type} '${path}' ${mode} ${user} ${group} ${age} ${argument}";

  restic = rec {
    port = 3571;
    host = "zaatar.r";
    repository = "rest:http://${host}:${toString port}/";
  };

  firewall = lib: {
    accept = {
      source,
      protocol,
      dport,
    }: "nixos-fw -s ${lib.escapeShellArg source} -p ${lib.escapeShellArg protocol} --dport ${lib.escapeShellArg (toString dport)} -j nixos-fw-accept";
    addRules = lib.concatMapStringsSep "\n" (rule: "iptables -A ${rule}");
    removeRules = lib.concatMapStringsSep "\n" (rule: "iptables -D ${rule} || true");
  };

  serveHtml = file: pkgs: ''
    default_type "text/html";
    root ${
      pkgs.linkFarm "fahrplan" [
        {
          name = "index.html";
          path = file;
        }
      ]
    };
    index index.html;
  '';

  sshPort = 22022;

  colours = import ./colours/ibm-3270.nix;

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

  retiolumAddresses = import ./retiolum-network.nix;

  localAddresses = import ./local-network.nix;

  email-sshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINKz33wHtPuIfgXEb0+hybxFGV9ZuPsDTLUZo/+hlcdA";

  kieran = {
    github = "kmein";
    email = "kmein@posteo.de";
    name = "Kierán Meinhardt";
    sshKeys = pkgs:
      pkgs.lib.strings.splitString "\n" (pkgs.lib.strings.fileContents (pkgs.fetchurl {
        url = "https://github.com/kmein.keys";
        sha256 = "09c6ny0rmpid1m0pc1wsmb3wyy9g721lf4kv55i4lrp42b3i2d5b";
      }));
  };

  syncthing.devices = {
    kabsa.id = "R6DEBD7-G5RYDKN-VFA3HPO-WX4DNVI-373F7OQ-AW5MZTT-3L4BDVW-Y6ROEAF";
    heym.id = "HLQSG3D-WSKLA6S-MEYQ3EU-GDBGABE-PY53RQ6-SWQAP2I-Z5MVBVX-MYPJXAM";
    manakish.id = "AJVBWR2-VFFAGZF-7ZF5JAX-T63GMOG-NZ446WK-MC5E6WK-6X6Q2HE-QQA2JQ3";
    toum.id = "CBJQXFF-FMFGWFU-2J6FMPR-SRDTSGX-7NHOYOH-CQCABKF-KQJMRJC-SDE24Q4";
    zaatar.id = "CGHO6LK-ZJBAXBD-UWI7AH3-BXYARE6-EUIM7PE-O2FUCOM-VCCRNCM-IG34WQ7";
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
