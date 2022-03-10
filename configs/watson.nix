{
  config,
  pkgs,
  ...
}: {
  environment.systemPackages = [pkgs.watson];

  environment.variables.WATSON_DIR = "${config.users.users.me.home}/cloud/Seafile/Documents/watson";
}
