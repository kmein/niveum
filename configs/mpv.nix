{
  pkgs,
  lib,
  self,
  config,
  ...
}:
let
  swallow = command: "${pkgs.swallow}/bin/swallow ${command}";
  myMpv =
    pkgs:
    self.inputs.wrappers.wrapperModules.mpv.apply {
      inherit pkgs;
      scripts = [
        pkgs.mpvScripts.visualizer
      ];
      "mpv.conf".content = "";
      "mpv.input".content = ''
        Alt+- add video-zoom -0.25
        Alt+= add video-zoom 0.25
        Alt+LEFT add video-rotate -90
        Alt+RIGHT add video-rotate 90
        Alt+h add video-pan-x 0.05
        Alt+j add video-pan-y -0.05
        Alt+k add video-pan-y 0.05
        Alt+l add video-pan-x -0.05
      '';
    };
in
{
  environment.shellAliases.smpv = swallow "mpv";

  nixpkgs.overlays = [
    (final: prev: {
      mpv = (myMpv prev).wrapper;
    })
  ];

  environment.systemPackages = [
    ((myMpv pkgs).wrapper)
  ];
}
