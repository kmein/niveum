{ lib }:
with lib;
let huServer = name: ''
  Host ${name}
      HostName ${name}.informatik.hu-berlin.de
      User ${(import ../secrets.nix).eduroam.identity}
  '';
in strings.concatMapStringsSep "\n\n" huServer [ "rabe" "star" "gruenau" "gruenau1" "gruenau2" "gruenau3" "gruenau4" "gruenau5" "gruenau6" "gruenau7" "gruenau8" ]
