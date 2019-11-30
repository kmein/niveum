{ lib, ... }:
let
  inherit (lib.sources) commitIdFromGitRepo;
in
{
  environment.etc."NIVEUM_VERSION".text = commitIdFromGitRepo <niveum/.git>;
}
