{pkgs, ...}: {
  programs.chromium = {
    enable = true;
    extensions = [
      # "ihlenndgcmojhcghmfjfneahoeklbjjh" # cVim
      # "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "pjjgklgkfeoeiebjogplpnibpfnffkng" # undistracted
      "nhdogjmejiglipccpnnnanhbledajbpd" # vuejs devtools
      "eimadpbcbfnmbkopoojfekhnkhdbieeh" # dark reader
    ];
  };

  environment.systemPackages = [pkgs.brave];

  environment.variables.BROWSER = "brave";
}
