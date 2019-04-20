{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      "gighmmpiobklfepjocnamgkkbiglidom" # AdBlock
      "hdokiejnpimakedhajhdlcegeplioahd" # LastPass
      "jldhpllghnbhlbpcmnajkpdmadaolakh" # Todoist
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
      "fpnmgdkabkmnadcjpehmlllkndpkmiak" # Wayback Machine
    ];
  };

  environment.systemPackages = [ pkgs.chromium ];

  niveum.applications.browser = "chromium";
}
