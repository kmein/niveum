{
  config,
  pkgs,
  ...
}:
{
  niveum.bots.nietzsche = {
    enable = true;
    time = "08:00";
    mastodon = {
      enable = true;
      tokenFile = config.age.secrets.mastodon-token-nietzsche.path;
      language = "de";
    };
    command = toString (
      pkgs.writers.writeBash "random-nietzsche" ''
        set -efu
        random_number=$(( ($RANDOM % 10) + 1 ))
        if [ "$random_number" -eq 1 ]; then
          ${pkgs.random-zeno}/bin/random-zeno "/Literatur/M/Nietzsche,+Friedrich"
        else
          ${pkgs.random-zeno}/bin/random-zeno "/Philosophie/M/Nietzsche,+Friedrich"
        fi
      ''
    );
  };

  systemd.timers.bot-nietzsche.timerConfig.RandomizedDelaySec = "10h";

  age.secrets = {
    mastodon-token-nietzsche.file = ../../secrets/mastodon-token-nietzsche.age;
  };
}
