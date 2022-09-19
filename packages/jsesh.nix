{pkgs ? import <nixpkgs> {}}:
pkgs.writers.writeDashBin "jsesh" ''
  ${pkgs.jre}/bin/java -jar ${pkgs.fetchzip {
    url = "https://github.com/rosmord/jsesh/releases/download/release-7.5.5/JSesh-7.5.5.zip";
    sha256 = "1z7ln51cil9pypz855x9a8p9ip2aflvknh566wcaah1kmz3fp57r";
  }}/lib/jseshAppli-7.5.5.jar
''
