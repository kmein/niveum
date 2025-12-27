{
  pkgs,
  lib,
  ...
}:
let
  darwin = lib.strings.hasSuffix "-darwin" pkgs.stdenv.hostPlatform.system;
in
{
  environment.systemPackages = [
    pkgs.htop
    pkgs.w3m
    pkgs.wget
    # ARCHIVE TOOLS
    pkgs.unzip
    pkgs.unrar
    pkgs.p7zip
    pkgs.sshuttle
    pkgs.zip
    # MONITORS
    pkgs.iftop # interface bandwidth monitor
    pkgs.lsof # list open files
    # SHELL
    pkgs.sqlite
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
    pkgs.vimv
    pkgs.vg
    pkgs.fkill
    pkgs.cyberlocker-tools
    pkgs.untilport
    pkgs.kpaste
    # HARDWARE
    pkgs.pciutils # for lspci
  ]
  ++ lib.optionals (!darwin) [
    pkgs.usbutils # for lsusb
    pkgs.lshw # for lshw
    pkgs.iotop # I/O load monitor
    pkgs.psmisc # for killall, pstree
  ];

  security.wrappers = {
    pmount = {
      setuid = true;
      owner = "root";
      group = "root";
      source = "${pkgs.pmount}/bin/pmount";
    };
    pumount = {
      setuid = true;
      owner = "root";
      group = "root";
      source = "${pkgs.pmount}/bin/pumount";
    };
  };

  environment.interactiveShellInit = ''
    # Use XDG_RUNTIME_DIR for temporary files if available
    if [ -d "$XDG_RUNTIME_DIR" ]; then
      export TMPDIR="$XDG_RUNTIME_DIR"
    fi
  '';

  environment.shellAliases =
    let
      take = pkgs.writers.writeDash "take" ''
        mkdir "$1" && cd "$1"
      '';
      cdt = pkgs.writers.writeDash "cdt" ''
        cd $(mktemp -p "$XDG_RUNTIME_DIR" -d "cdt-XXXXXX")
        pwd
      '';
      wcd = pkgs.writers.writeDash "wcd" ''
        cd "$(readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname)/.."
      '';
      where = pkgs.writers.writeDash "where" ''
        readlink "$(${pkgs.which}/bin/which --skip-alias "$1")" | xargs dirname
      '';
    in
    {
      nixi = "nix repl nixpkgs";
      take = "source ${take}";
      wcd = "source ${wcd}";
      where = "source ${where}";
      # temporary files and directories
      cdt = "source ${cdt}";
      vit = "$EDITOR $(mktemp)";
      # file safety
      mv = "${pkgs.coreutils}/bin/mv --interactive";
      rm = "${pkgs.coreutils}/bin/rm --interactive";
      cp = "${pkgs.coreutils}/bin/cp --interactive";
      # colours
      cat = "${pkgs.bat}/bin/bat --theme=ansi --style=plain";
      l = "${pkgs.coreutils}/bin/ls --color=auto --time-style=long-iso --almost-all";
      ls = "${pkgs.coreutils}/bin/ls --color=auto --time-style=long-iso";
      ll = "${pkgs.coreutils}/bin/ls --color=auto --time-style=long-iso -l";
      la = "${pkgs.coreutils}/bin/ls --color=auto --time-style=long-iso --almost-all -l";
    }
    // (
      if darwin then
        { }
      else
        {
          "ß" = "${pkgs.util-linux}/bin/setsid";
          ip = "${pkgs.iproute2}/bin/ip -c";
          # systemd
          s = "${pkgs.systemd}/bin/systemctl";
          us = "${pkgs.systemd}/bin/systemctl --user";
          j = "${pkgs.systemd}/bin/journalctl";
          uj = "${pkgs.systemd}/bin/journalctl --user";
        }
    );
}
