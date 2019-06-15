{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      "hdokiejnpimakedhajhdlcegeplioahd" # LastPass
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
      "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
    ];
  };

  environment.systemPackages = [ pkgs.chromium ];

  niveum.applications.browser = "chromium";
}
