{
  home-manager.users.me = {
    programs.newsboat = {
      enable = false;
      urls = [
        { url = "https://kmein.github.io/meteora/atom.xml"; tags = [ "poetry" ]; }
      ];
    };
  };
}
