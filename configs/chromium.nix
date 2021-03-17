{ pkgs, ... }: {
  programs.chromium = {
    enable = true;
    extensions = [
      "nngceckbapebfimnlniiiahkandclblb" # BitWarden
      # "ihlenndgcmojhcghmfjfneahoeklbjjh" # cVim
      # "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "khncfooichmfjbepaaaebmommgaepoid" # Remove YouTube Recommended Videos
    ];
  };

  environment.systemPackages = [ pkgs.chromium pkgs.brave ];

  environment.variables.BROWSER = "brave";
}
