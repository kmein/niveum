{ lib, config, ... }:
let inherit (lib.sources) commitIdFromGitRepo;
in {
  environment.etc."niveum/version".text = commitIdFromGitRepo <niveum/.git>;
}
