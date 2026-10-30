# instructions and skills for claude code; omp reads them from ~/.claude too.
# proprietary skills stay unmanaged in ~/.claude/skills, out of this public repo.
{ pkgs, self, ... }:
{
  home-manager.users.me.programs.claude-code = {
    enable = true;
    # the binary comes from configs/applicative.nix; settings.json stays writable
    # for /model, /effort and plugin installs
    package = null;
    context = ./instructions.md;
    skills.astrology = ./skills/astrology;
  };

  environment.systemPackages = [
    (self.wrapperModules.omp.apply {
      inherit pkgs;
      settings.enabledProviders = [ "claude" ];
    }).wrapper
  ];
}
