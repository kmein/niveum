{
  home-manager.users.me = {
    services.random-background = {
      enable = true;
      imageDirectory = toString <art>;
      interval = "2h";
    };
  };
}
