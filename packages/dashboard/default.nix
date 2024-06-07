{
  writers,
  formats,
  wtf,
  himalaya,
  lib,
  jq,
  gh,
  curl,
  khal,
  todoman,
  gnused,
  coreutils,
  astrolog,
  weatherCityIds ? [2950159],
}: let
  rowCount = 10;
  columnCount = 6;

  yaml = formats.yaml {};
  command = args:
    {
      enabled = true;
      type = "cmdrunner";
    }
    // args;
  configuration.wtf = rec {
    grid = {
      columns = lib.replicate columnCount 32;
      rows = lib.replicate rowCount 5;
    };
    mods.vdir_khal = command {
      title = "Calendar";
      cmd = "${khal}/bin/khal";
      args = ["--color" "list" "--exclude-calendar" "calendarium-tridentinum"];
      refreshInterval = "1m";
      position = {
        top = 4;
        left = 0;
        height = 4;
        width = 2;
      };
    };
    mods.vdir_todo = command {
      enabled = true;
      title = "Agenda";
      cmd = writers.writeDash "vdir_todo" "${todoman}/bin/todo --color=always -h | ${coreutils}/bin/tac";
      refreshInterval = "1m";
      position = {
        top = 4;
        left = 2;
        height = 4;
        width = 2;
      };
    };
    mods.weather = {
      enabled = true;
      cityids = weatherCityIds;
      position = {
        top = 8;
        left = 2;
        height = 2;
        width = 2;
      };
      refreshInterval = "15m";
      language = "DE";
      tempUnit = "C";
      useEmoji = true;
      compact = true;
    };
    mods.top = command {
      title = "uptime";
      cmd = writers.writeDash "top" "top -b -n 1 -E g | ${gnused}/bin/sed -n '1,5p'";
      refreshInterval = "30s";
      position = {
        top = 4;
        left = 4;
        height = 2;
        width = 2;
      };
      enabled = true;
    };
    mods.resourceusage = {
      enabled = true;
      cpuCombined = false;
      position = {
        top = 6;
        left = 4;
        height = 2;
        width = 2;
      };
      refreshInterval = "1s";
      showCPU = true;
      showMem = true;
      showSwp = false;
    };
    mods.ipapi = {
      enabled = false;
      position = {
        top = 0;
        left = 1;
        height = 2;
        width = 2;
      };
      refreshInterval = "150s";
    };
    mods.disk-usage = command {
      enabled = true;
      cmd = "df";
      args = ["-h"];
      refreshInterval = "1m";
      position = {
        top = 8;
        left = 4;
        height = 2;
        width = 2;
      };
    };
    mods.email = command {
      title = "Email";
      cmd = writers.writeDash "email" ''
        ${himalaya}/bin/himalaya accounts --output json \
          | ${jq}/bin/jq -r 'map(.name) | join("\n")' \
          | while read -r account
            do
              ${himalaya}/bin/himalaya list --account "$account" -o json \
                | ${jq}/bin/jq -r '
                  map(select(.flags == [])
                  | "\u001b[33m\(.from.addr)\u001b[0m \(.subject)") | join("\n")
                '
            done
      '';
      refreshInterval = "5m";
      position = {
        top = 0;
        left = 0;
        height = 4;
        width = 2;
      };
    };
    mods.gh-status = command {
      enabled = true;
      title = "GitHub";
      cmd = writers.writeDash "gh-status" ''
        ${gh}/bin/gh api notifications \
          | ${jq}/bin/jq -r 'map("\u001b[35m\(.repository.full_name)\u001b[0m \(.subject.title)") | join("\n")'
      '';
      refreshInterval = "5m";
      position = {
        top = 0;
        left = 2;
        height = 2;
        width = 2;
      };
    };
    mods.gh-issues = command {
      enabled = true;
      title = "GitHub";
      cmd = writers.writeDash "gh-issues" ''
        ${gh}/bin/gh api issues \
          | ${jq}/bin/jq -r 'map(select(.repository.owner.login == "kmein") | "\u001b[35m\(.repository.name)\u001b[0m \(.title)") | join("\n")'
      '';
      refreshInterval = "5m";
      position = {
        top = 2;
        left = 2;
        height = 2;
        width = 2;
      };
    };
    mods.calendar = command {
      title = "Calendar";
      cmd = "cal";
      args = ["-3" "-m" "-w"];
      pty = true;
      refreshInterval = "5m";
      position = {
        top = 8;
        left = 0;
        height = 2;
        width = 2;
      };
    };
    mods.astro-aspects = command {
      title = "Aspects";
      enabled = false;
      cmd = writers.writeDash "astro-aspects" "${astrolog}/bin/astrolog -n -zN Berlin -d";
      refreshInterval = "1h";
      position = {
        top = 7;
        left = 3;
        height = 1;
        width = 2;
      };
    };
    mods.feed = command {
      enabled = true;
      title = "Feed";
      cmd = writers.writeDash "feed" ''
        ${curl}/bin/curl -u "$WTF_MINIFLUX_API_KEY" --basic -s 'https://feed.kmein.de/v1/entries?status=unread&direction=desc' \
          | ${jq}/bin/jq -r '
            .total as $total | (
              .entries
              | map(select(.feed | .hide_globally| not) | "\(.feed.category.title) \u001b[32m\(.author)\u001b[0m \(.title)")
              | join("\n")
            )'
      '';
      # position = { top = 0; left = 5; height = 5; width = 1; };
      position = {
        top = 0;
        left = 4;
        height = 4;
        width = 2;
      };
      refreshInterval = "15m";
    };
    mods.astro-positions = command {
      enabled = false;
      title = "Positions";
      cmd = writers.writeDash "astro-positions" "${astrolog}/bin/astrolog -q $(date +'%m %d %Y %H:%M') -zN Berlin | ${gnused}/bin/sed -n '4,16p' | ${coreutils}/bin/cut -c 1-33";
      refreshInterval = "1h";
      position = {
        top = 5;
        left = 5;
        height = 3;
        width = 1;
      };
    };
  };
in
  writers.writeDashBin "dashboard" ''
    exec ${wtf}/bin/wtfutil --config=${yaml.generate "config.yml" configuration}
  ''
