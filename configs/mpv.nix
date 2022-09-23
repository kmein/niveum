{
  pkgs,
  lib,
  config,
  ...
}: let
  scripts = import <niveum/packages/scripts> {inherit pkgs lib;};
  swallow = command: "${scripts.swallow}/bin/swallow ${command}";
in {
  environment.shellAliases.smpv = swallow "mpv";

  nixpkgs.overlays = [
    (self: super: {
      mpv = config.home-manager.users.me.programs.mpv.finalPackage;
    })
  ];

  home-manager.users.me = {
    programs.mpv = {
      enable = true;
      config = {
        ytdl-format = "bestvideo[height<=?720][fps<=?30][vcodec!=?vp9]+bestaudio/best";
        ytdl-raw-options = lib.concatStringsSep "," [''sub-lang="de,en"'' "write-sub=" "write-auto-sub="];
        screenshot-template = "%F-%wH%wM%wS-%#04n";
        script-opts = "ytdl_hook-ytdl_path=${pkgs.yt-dlp}/bin/yt-dlp";
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
        (pkgs.callPackage <niveum/packages/mpv-visualizer.nix> {})
      ];
    };
  };
}
