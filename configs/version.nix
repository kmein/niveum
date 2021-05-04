{ lib, config, ... }:
{
  environment.etc."niveum/version".text = lib.sources.commitIdFromGitRepo <niveum/.git>;
}
