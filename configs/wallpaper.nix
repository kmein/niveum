{ pkgs, lib, ... }:
let
  elementary-wallpapers = pkgs.fetchFromGitHub {
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
    rev = "4d29730bf350e4dbab1845cc4115fbf33eaf51c7";
    sha256 = "0wfpxd3w67ydvg7a0ypbg0yfs5sbvl8vhzna8wi6lw6ca1zv95im";
  };
in
{
  home-manager.users.me = {
    services.random-background = {
      enable = true;
      imageDirectory = "${kmein-wallpapers}/meteora";
      interval = "1h";
    };
  };
}
