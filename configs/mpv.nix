{
  pkgs,
  lib,
  ...
}: let
  swallow = command: "${pkgs.scripts.swallow}/bin/swallow ${command}";
in {
  environment.shellAliases.smpv = swallow "mpv";

  home-manager.users.me = {
    programs.mpv = {
      enable = true;
      config = {
        ytdl-raw-options = lib.concatStringsSep "," [''sub-lang="de,en"'' "write-sub=" "write-auto-sub="];
        screenshot-template = "%F-%wH%wM%wS-%#04n";
      };
      bindings = {
        "Alt+RIGHT" = "add video-rotate 90";
        "Alt+LEFT" = "add video-rotate -90";
        "Alt+-" = "add video-zoom -0.25";
        "Alt+=" = "add video-zoom 0.25";
        "Alt+l" = "add video-pan-x -0.05";
        "Alt+h" = "add video-pan-x 0.05";
        "Alt+k" = "add video-pan-y 0.05";
        "Alt+j" = "add video-pan-y -0.05";
      };
      scripts = [
        pkgs.mpvScripts.youtube-quality
      ];
    };
  };
}
