{ lib }:
with lib;
let huServer = name: {
  "${name}" = {
    hostname = "${name}.informatik.hu-berlin.de";
    user = (import ../secrets.nix).eduroam.identity;
    identityFile = "~/.ssh/hu";
  };
};
in lists.foldr (x: xs: huServer x // xs) {} [ "rabe" "star" "gruenau" "gruenau1" "gruenau2" "gruenau3" "gruenau4" "gruenau5" "gruenau6" "gruenau7" "gruenau8" ]
