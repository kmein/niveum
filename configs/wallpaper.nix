{ pkgs, lib, ... }:
let
  /* elementary-wallpapers = pkgs.fetchFromGitHub {
       owner = "elementary";
       repo = "wallpapers";
       rev = "6c81141e33ef69702a3f48e7181cb979a680190d"; # tag: 5.4
       sha256 = "1ihvv9v8m5f2n2v3bgg769l52wbg241zgp3d45q6phk7p8s1gz3s";
     };
     elementary-wallpapers-jpg = pkgs.runCommand "wallpapers" {} ''
       mkdir $out
       cp ${elementary-wallpapers}/*.jpg $out/
     '';
     luke-smith-wallpapers = pkgs.fetchFromGitHub {
       owner = "LukeSmithxyz";
       repo = "wallpapers";
       rev = "33cad3099919366cea2627f930da9b47609e8554";
       sha256 = "1li6rrn016fpgvmnijqhvkp07kj83cjwcjx2l2b3asb99d51814i";
     };
  kmein-wallpapers = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "wallpapers";
    rev = "7c553bc6bd78afa6dbf2824691466bbad0d8e6e9";
    sha256 = "1zik5z1cq1084j1hdwm204afz89f5hpg21z0vvcbppzkmldfxnnq";
  };
  */
in {
  imports = [ <stockholm/krebs/3modules/fetchWallpaper.nix> ];

  krebs.fetchWallpaper = {
    enable = true;
    # unitConfig.ConditionPathExists = "!/var/run/ppp0.pid";
    url = "http://prism.r/realwallpaper-krebs-stars.png"; # http://prism.r/realwallpaper-krebs.png"; # "http://prism.r/realwallpaper-krebs-stars-berlin.png";
  };

  services.xserver = {
    display = lib.mkForce 0; # needed for fetchWallpaper to find the X display
    displayManager.sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +LOCAL:";
  };

  /*
  home-manager.users.me = {
    services.random-background = {
      enable = true;
      imageDirectory = "${kmein-wallpapers}/meteora";
      interval = "1h";
    };
  };
  */
}
