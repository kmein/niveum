{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) serveHtml;
in
{
  services.nginx.virtualHosts."redaktion.r".locations."/".extraConfig = serveHtml <niveum/lib/radio-news.html> pkgs;
}
