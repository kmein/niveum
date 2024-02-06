{
  pkgs,
  lib,
  ...
}: let
  inherit (import ../../lib) serveHtml;
  remote = "https://cgit.lassul.us/stockholm";
in {
  services.nginx.virtualHosts."redaktion.r".locations."/".extraConfig = serveHtml ../../lib/radio-news.html pkgs;

  niveum.passport.services = [
    {
      title = "Retiolum Radio News";
      link = "http://redaktion.r";
      description = "supplies git history news to radio lassulus and lets you enter your own.";
    }
  ];
}
