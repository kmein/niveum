{ config, ... }:
let
  user = config.users.users.me.name;
in
{
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (subject.user == "${user}" && action.id == "org.freedesktop.systemd1.manage-units") {
        return polkit.Result.YES;
      }
    });
  '';
}
