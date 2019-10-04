{ pkgs, lib, ... }:
{
  home-manager.users.me = {
    services.random-background = {
      enable = true;
      imageDirectory = toString (pkgs.fetchFromGitHub {
        owner = "elementary";
        repo = "wallpapers";
        rev = "6c81141e33ef69702a3f48e7181cb979a680190d"; # tag: 5.4
        sha256 = "1ihvv9v8m5f2n2v3bgg769l52wbg241zgp3d45q6phk7p8s1gz3s";
      });
      interval = "2h";
    };
  };
}
