{ pkgs, ... }: {
  programs.chromium = {
    enable = true;
    extensions = [
      "hdokiejnpimakedhajhdlcegeplioahd" # LastPass
      "ihlenndgcmojhcghmfjfneahoeklbjjh" # cVim
      # "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      # "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
    ];
  };

  environment.systemPackages = [ pkgs.chromium pkgs.brave ];

  environment.variables.BROWSER = "brave";
}
