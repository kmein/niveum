{ lib }:
with lib;
let huServer = name: ''
  Host ${name}
      User ${(import ../secrets.nix).eduroam.identity}
      HostName ${name}.informatik.hu-berlin.de
      IdentityFile ~/.ssh/hu
  '';
in ''
  Host github
      User git
      HostName github.com

  Host gitlab
      User git
      HostName gitlab.informatik.hu-berlin.de

  Host happysrv
      User meinhark
      HostName v22017123717458308.happysrv.de
      Port 49000
'' + strings.concatMapStringsSep "\n\n" huServer [ "rabe" "star" "gruenau" "gruenau1" "gruenau2" "gruenau3" "gruenau4" "gruenau5" "gruenau6" "gruenau7" "gruenau8" ]
