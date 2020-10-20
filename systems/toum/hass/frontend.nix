let
  inherit (import ./lib.nix) triggers;
in
{
  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    <nixos-unstable/nixos/modules/services/misc/home-assistant.nix>
  ];


  services.home-assistant.config = {
    frontend = {
      themes = {
        day_theme = import ./themes/clear.nix;
        night_theme = import ./themes/clear-dark.nix;
      };
    };
    automation = [
      {
        alias = "Night Theme";
        hide_entity = true;
        trigger = triggers.night;
        action = [
          {
            service = "frontend.set_theme";
            data.name = "night_theme";
          }
        ];
      }
      {
        alias = "Day Theme";
        hide_entity = true;
        trigger = triggers.day;
        action = [
          {
            service = "frontend.set_theme";
            data.name = "day_theme";
          }
        ];
      }
    ];
  };
}
