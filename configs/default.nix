{
  config,
  ...
}:
{
  imports = [
    ./desktop-core.nix
    # every locale glibc has is a gigabyte
    { i18n.supportedLocales = [ "all" ]; }
    {
      users.users.me.hashedPasswordFile = config.age.secrets.kfm-password.path;
      age.secrets.kfm-password.file = ../secrets/kfm-password.age;
    }
    {
      age.secrets = {
        di-fm-key = {
          file = ../secrets/di-fm-key.age;
          owner = config.users.users.me.name;
          group = config.users.users.me.group;
          mode = "440";
        };
      };
    }
    ./android.nix
    ./backup.nix
    ./mail.nix
    ./irc.nix
    ./khal.nix
    ./focus.nix
    ./cloud.nix
    ./hledger.nix
    ./agents
    ./kiwix
    ./uni.nix
    ./lb.nix
    ./editor.nix
    ./packages.nix
    ./printing.nix
    ./tts
    ./vscode.nix
    {
      programs.zsh.interactiveShellInit = ''
        hash -d nixos=/etc/nixos niveum=${config.users.users.me.home}/state/src/niveum
      '';

      home-manager.users.me = {
        xdg.userDirs =
          let
            pictures = "${config.users.users.me.home}/sync/Bilder";
          in
          {
            setSessionVariables = true;
            enable = true;
            documents = "${config.users.users.me.home}/sync/Documents";
            desktop = "/tmp";
            download = "${config.users.users.me.home}/sync/Downloads";
            music = "${config.users.users.me.home}/mobile/audio";
            videos = pictures;
            pictures = pictures;
          };
      };
    }
  ];
}
