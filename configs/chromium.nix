{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      "hdokiejnpimakedhajhdlcegeplioahd" # LastPass
      "ihlenndgcmojhcghmfjfneahoeklbjjh" # cVim
      "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "ioomnmgjblnnolpdgdhebainmfbipjoh" # herp derp YouTube comments
    ];
  };

  environment.systemPackages = [ pkgs.chromium ];

  environment.variables.BROWSER = "chromium";
}
