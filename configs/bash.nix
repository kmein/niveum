{pkgs, ...}: {
  programs.bash = {
    promptInit = ''
      PS1="$(${pkgs.ncurses}/bin/tput bold)\w \$([[ \$? == 0 ]] && echo \"\[\033[1;32m\]\" || echo \"\[\033[1;31m\]\")\$$(${pkgs.ncurses}/bin/tput sgr0) "'';
    interactiveShellInit = ''
      set -o vi
    '';
    completion.enable = true;
  };
}
