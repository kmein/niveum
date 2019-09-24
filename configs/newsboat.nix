{
  home-manager.users.me = {
    programs.newsboat = {
      enable = true;
      urls = [
        { url = "https://kmein.github.io/meteora/atom.xml"; tags = [ "poetry" ]; }
      ];
    };
  };
}
