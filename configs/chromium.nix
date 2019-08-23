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
      "iaalpfgpbocpdfblpnhhgllgbdbchmia" # asciidoctor
    ];
  };

  environment.systemPackages = [ pkgs.chromium ];

  niveum.applications.browser = "chromium";
}
