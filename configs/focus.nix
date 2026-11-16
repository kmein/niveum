{ self, ... }:
let
  weekdays = [
    "Mon"
    "Tue"
    "Wed"
    "Thu"
    "Fri"
  ];
  everyDay = weekdays ++ [
    "Sat"
    "Sun"
  ];
in
{
  imports = [ self.nixosModules.focus ];

  niveum.focus = {
    enable = true;
    lists = {
      distract = [
        "instagram.com"
        "feed.kmein.de"
        "news.ycombinator.com"
        "youtube.com"
        "twitch.tv"
      ];
    };
    schedules = [
      {
        days = weekdays;
        from = "09:00";
        until = "17:00";
        block = [
          "distract"
        ];
      }
      {
        days = everyDay;
        from = "23:00";
        until = "08:00";
        block = [ "offline" ];
      }
    ];
  };

  # browser doh would resolve around systemd-resolved
  home-manager.users.me.programs.firefox.profiles.default.settings."network.trr.mode" = 5;
  environment.etc."brave/policies/managed/focus.json".text = builtins.toJSON {
    DnsOverHttpsMode = "off";
  };
}
