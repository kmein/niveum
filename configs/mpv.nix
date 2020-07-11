{ pkgs, ... }: let
  swallow = command: "${pkgs.scripts.swallow}/bin/swallow ${command}";
in {
  environment.shellAliases.mpv = swallow "mpv";

  home-manager.users.me = {
    programs.mpv = {
      enable = true;
      config = {
        force-window = "yes";
        ytdl-format = "bestvideo[height<=?720][fps<=?30][vcodec!=?vp9]+bestaudio/best";
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
    };
  };
}
