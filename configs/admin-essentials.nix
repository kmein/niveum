{
  pkgs,
  niveumPackages,
  ...
}: {
  environment.systemPackages = [
    pkgs.htop
    pkgs.w3m
    pkgs.wget
    # ARCHIVE TOOLS
    pkgs.unzip
    pkgs.unrar
    pkgs.p7zip
    pkgs.zip
    # MONITORS
    pkgs.iotop # I/O load monitor
    pkgs.iftop # interface bandwidth monitor
    pkgs.lsof # list open files
    pkgs.psmisc # for killall, pstree
    # SHELL
    pkgs.fd # better find
    pkgs.tree
    pkgs.parallel # for parallel, since moreutils shadows task spooler
    pkgs.ripgrep # better grep
    pkgs.rlwrap
    pkgs.progress # display progress bars for pipes
    pkgs.file # determine file type
    pkgs.gdu # ncurses disk usage (ncdu is broken)
    pkgs.rmlint # remove duplicate files
    pkgs.jq # json toolkit
    pkgs.jless # less(1) for json
    pkgs.fq # toolkit for yaml, xml and binaries
    pkgs.bc # calculator
    pkgs.pari # gp -- better calculator
    pkgs.ts
    niveumPackages.vimv
    niveumPackages.vg
    niveumPackages.fkill
    niveumPackages.cyberlocker-tools
    niveumPackages.untilport
    niveumPackages.kpaste
    # HARDWARE
    pkgs.usbutils # for lsusb
    pkgs.pciutils # for lspci
    pkgs.lshw # for lshw
  ];

  environment.shellAliases = let
    take = pkgs.writers.writeDash "take" ''
      mkdir "$1" && cd "$1"
    '';
    cdt = pkgs.writers.writeDash "cdt" ''
      cd "$(mktemp -d)"
      pwd
    '';
    wcd = pkgs.writers.writeDash "wcd" ''
      cd "$(readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname)/.."
    '';
    where = pkgs.writers.writeDash "where" ''
      readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname
    '';
  in {
    "ß" = "${pkgs.util-linux}/bin/setsid";
    nixi = "nix repl '<nixpkgs>'";
    take = "source ${take}";

    wcd = "source ${wcd}";
    where = "source ${where}";
    # temporary files and directories
    cdt = "source ${cdt}";
    vit = "$EDITOR $(mktemp)";
    # file safety
    mv = "mv --interactive";
    rm = "rm --interactive";
    cp = "cp --interactive";
    # colours
    cat = "${pkgs.bat}/bin/bat --theme=ansi --style=plain";
    l = "ls --color=auto --time-style=long-iso --almost-all";
    ls = "ls --color=auto --time-style=long-iso";
    ll = "ls --color=auto --time-style=long-iso -l";
    la = "ls --color=auto --time-style=long-iso --almost-all -l";
    ip = "${pkgs.iproute2}/bin/ip -c";
    # systemd
    s = "${pkgs.systemd}/bin/systemctl";
    us = "${pkgs.systemd}/bin/systemctl --user";
  };
}
