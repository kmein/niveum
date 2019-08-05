{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      "hdokiejnpimakedhajhdlcegeplioahd" # LastPass
      "ihlenndgcmojhcghmfjfneahoeklbjjh" # cVim
      "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "iaalpfgpbocpdfblpnhhgllgbdbchmia" # asciidoctor
    ];
  };

  environment.systemPackages = [ pkgs.chromium ];

  niveum.applications.browser = "chromium";
}
