{ pkgs, lib, ... }:
let
  autorenkalender-package = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "autorenkalender";
    rev = "1971f082ec6e14d392a0dc3ac62e0b1e4187409b";
    sha256 = "0hipj616vcsa3f62s83jvlx8zx4bmbgl5h2n4w8ba5ngp40lkmb3";
  };
  autorenkalender = pkgs.python3Packages.callPackage autorenkalender-package {};
in
{
  niveum.telegramBots.autorenkalender = {
    enable = true;
    time = "07:00";
    token = lib.strings.fileContents <secrets/telegram/kmein.token>;
    chatIds = [ "@autorenkalender" ];
    parseMode = "Markdown";
    command = "${autorenkalender}/bin/autorenkalender";
  };
}
