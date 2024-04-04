{
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
      pkgs.linkFarm "www" [
        {
          name = "index.html";
          path = file;
        }
      ]
    };
    index index.html;
  '';

  sshPort = 22022;

  theme = pkgs: {
    gtk = {
      name = "Adwaita-dark";
      package = pkgs.gnome.gnome-themes-extra;
    };
    icon = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
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
    sshKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDyTnGhFq0Q+vghNhrqNrAyY+CsN7nNz8bPfiwIwNpjk" # kabsa
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOiQEc8rTr7C7xVLYV7tQ99BDDBLrJsy5hslxtCEatkB" # manakish
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIByreBjBEMJKjgpKLd5XZHIUUwIhNafVqN6OUOQpJa3y" # fatteh
    ];
  };

  syncthing.devices = {
    kabsa.id = "R6DEBD7-G5RYDKN-VFA3HPO-WX4DNVI-373F7OQ-AW5MZTT-3L4BDVW-Y6ROEAF";
    kibbeh.id = "HLQSG3D-WSKLA6S-MEYQ3EU-GDBGABE-PY53RQ6-SWQAP2I-Z5MVBVX-MYPJXAM";
    manakish.id = "AJVBWR2-VFFAGZF-7ZF5JAX-T63GMOG-NZ446WK-MC5E6WK-6X6Q2HE-QQA2JQ3";
    fatteh.id = "GSOGYT3-2GBHZXT-MNCTDIY-3BJIR4V-OHVOOMJ-ICVLKXR-U4C7RFB-HJOK3AC";
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
