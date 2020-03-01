{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      "hdokiejnpimakedhajhdlcegeplioahd" # LastPass
      "ihlenndgcmojhcghmfjfneahoeklbjjh" # cVim
      "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
    ];
  };

  environment.systemPackages = [ pkgs.chromium ];

  environment.variables.BROWSER = "chromium";
}
