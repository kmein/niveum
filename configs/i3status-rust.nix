{
  pkgs,
  config,
  ...
}: {
  home-manager.users.me = {
    programs.i3status-rust = {
      enable = true;
      bars.bottom = {
        icons = "awesome6";
        settings = {
          theme.overrides = let
            colours = config.lib.stylix.colors.withHashtag;
          in {
            idle_bg = colours.base00;
            idle_fg = colours.base05;
            good_bg = colours.base00;
            good_fg = colours.base0B;
            warning_bg = colours.base00;
            warning_fg = colours.base0A;
            critical_bg = colours.base00;
            critical_fg = colours.base09;
            info_bg = colours.base00;
            info_fg = colours.base04;
            separator_bg = colours.base00;
            separator = " ";
          };
        };
        blocks = [
          {
            block = "weather";
            autolocate = true;
            format = "$icon $location: $temp";
            service = {
              name = "openweathermap";
              city_id = "2950159";
              units = "metric";
            };
          }
          {
            block = "custom";
            interval = 60 * 5;
            command = let
              spacetime = import ../configs/spacetime.nix;
            in
              pkgs.writers.writePython3 "sun.py" {
                libraries = [pkgs.python3Packages.astral];
                flakeIgnore = ["E121" "E501"];
              }
              ''
                import astral
                import astral.moon
                import astral.sun

                current_phase = astral.moon.phase()

                city = astral.LocationInfo("Berlin", "Germany", "${spacetime.time.timeZone}", ${toString spacetime.location.latitude}, ${toString spacetime.location.longitude})
                sun = astral.sun.sun(city.observer, date=astral.today(), tzinfo=city.timezone)

                print("↑{} ↓{} {}{}".format(sun["sunrise"].strftime("%R"), sun["sunset"].strftime("%R"), "☽" if current_phase < 14 else "☾", round(current_phase, 1)))
              '';
          }
          {
            block = "github";
            info = ["total"];
            warning = ["mention" "review_requested" "team_mention" "manual" "invitation" "assign" "subscribed"];
          }
          {
            block = "custom";
            interval = 10;
            command = pkgs.writers.writeDash "todo" ''
              ${pkgs.todoman}/bin/todo --porcelain | ${pkgs.jq}/bin/jq -r '
                map(select(.due != null))
                | (map(select(.due < now)) | length) as $overdue
                | (map(select(.due >= now and .due < now + (60 * 60 * 24))) | length) as $dueToday
                | {
                  icon: "tasks",
                  text: (($overdue + $dueToday) as $sum | if $sum > 0 then $sum | tostring else "" end),
                  state: (
                    if $overdue > 0 then
                      "Critical"
                    elif $dueToday > 0 then
                      "Warning"
                    else
                      "Idle"
                    end
                  )
                }
              '
            '';
            json = true;
            hide_when_empty = true;
          }
          {
            block = "custom";
            interval = 60;
            command = pkgs.writers.writeDash "weechat" ''
              ssh -o ConnectTimeout=1 makanek cat /var/lib/weechat/hotlist.txt | sed 's/,/\n/g' | wc -l | jq '{
                text: (if . > 0 then . | tostring else "" end),
                state: (if . > 0 then "Info" else "Idle" end),
                icon: "bell"
              }'
            '';
            json = true;
            hide_when_empty = true;
          }
          {
            block = "net";
            format = " $icon HU";
            missing_format = "";
            device = "ppp0";
          }
          {
            block = "net";
            format = " $icon FU";
            missing_format = "";
            device = "tun0";
          }
          {
            block = "net";
            device = config.niveum.wirelessInterface;
            format = "$icon $ssid $signal_strength";
          }
          {
            block = "battery";
            device = config.niveum.batteryName;
          }
          {
            block = "sound";
          }
          {
            block = "disk_space";
            format = "$icon $available";
          }
          {
            block = "memory";
            format = "$icon $mem_used.eng(prefix:G)";
          }
          {block = "load";}
          {
            block = "custom";
            interval = 10;
            json = true;
            command = pkgs.writers.writeDash "time" ''
              ${pkgs.jq}/bin/jq -n \
                --arg now "$(${pkgs.coreutils}/bin/date +'%Y-%m-%d (%W %a) %H:%M')" \
                --argjson nextEvent "$(
                  ${pkgs.khal}/bin/khal list --format "{start}" --day-format "" $(${pkgs.coreutils}/bin/date +'%Y-%m-%d %H:%M') 2>/dev/null \
                  | ${pkgs.gnugrep}/bin/grep -E '[0-9]{2}:[0-9]{2}' \
                  | ${pkgs.coreutils}/bin/head -1 \
                  | ${pkgs.coreutils}/bin/date --date="$(cat)" +%s
                )" \
                '{
                  text: $now,
                  icon: "time",
                  state: (
                    ($nextEvent - now) as $deltaT
                    | if $deltaT < (5 * 60) then
                      "Critical"
                    elif $deltaT < (15 * 60) then
                      "Warning"
                    elif $deltaT < (60 * 60) then
                      "Info"
                    else
                      "Idle"
                    end
                  )
                }'
            '';
          }
        ];
      };
    };
  };
}
