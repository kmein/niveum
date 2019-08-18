{ pkgs, ... }:
{
  imports = [
    ./krebs.nix
    ./programming.nix
    ./writing.nix
    ./python.nix
    ./haskell
    {
      environment.systemPackages = with pkgs; [
      ] ++ [ # internet
        aria2
        firefox
        tor-browser-bundle-bin
        thunderbird
        tdesktop
        w3m
        wget
        httpie
        whois
        ddgr
        python3Packages.instaloader
        mtr # my traceroute
      ] ++ [ # media
        ffmpeg
        imagemagick
        sxiv
      ] ++ [ # archive
        unzip
        unrar
        p7zip
        zip
      ] ++ [ # monitor
        htop
        iotop
        iftop
        lsof
        psmisc
      ] ++ [ # shell
        bat
        dos2unix
        ncdu
        du-dust
        fd
        file
        jq # json manipulation
        jo # json creation
        kmein.nav # json navigation
        xsv
        xmlstarlet
        manpages
        posix_man_pages
        moreutils
        ranger
        ripgrep
        rlwrap
        progress # display progress bars for pipes
        up # universal plumber (piping tool)
      ] ++ [ # hardware
        usbutils
        pciutils
        lshw
      ] ++ [ # graphical
        arandr
        libnotify
        xclip
        xorg.xkill
      ] ++ [ # programming
        nix-prefetch-git
      ] ++ [ # media
        audacity
        calibre
        inkscape
        xpdf
        pdfgrep
        pdftk
        spotify
        python3Packages.spotify-cli-linux
        youtubeDL
      ] ++ [ # math
        bc
      ] ++ [ # shell
        (pass.withExtensions (ext: [ext.pass-otp]))
        fzf
        gnupg
        kmein.haskellPackages.mnemosyne
        kmein.autorenkalender
        kmein.bvg
        kmein.daybook
        kmein.depp
        kmein.font-size
        kmein.genius
        kmein.instaget
        kmein.literature-quote
        kmein.n
        kmein.odyssey
        kmein.wttr
        kmein.slide
        memo
        nix-git
        par
        qrencode
        unstable.hugo
        unstable.zola
        wtf
      ];
    }
  ];
}
