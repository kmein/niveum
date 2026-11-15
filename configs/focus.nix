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
      social = [
        "youtube.com"
        "reddit.com"
        "twitter.com"
        "x.com"
        "instagram.com"
        "facebook.com"
      ];
      news = [
        "spiegel.de"
        "zeit.de"
        "tagesschau.de"
        "news.ycombinator.com"
      ];
    };
    schedules = [
      {
        days = weekdays;
        from = "09:00";
        until = "17:00";
        block = [
          "social"
          "news"
        ];
      }
      {
        days = everyDay;
        from = "23:30";
        until = "06:30";
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
