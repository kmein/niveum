[ ] environment.etc."sshd/banner-line".text = config.networking.hostName
[ ] fix man-pdf:
    man-pdf 1p env
    man-pdf env

    man-pdf:
    man $options -t $command | ps2pdf - $command.pdf
[ ] homeros: add guest-user
    (gnome, firefox with plugins and prefs, libreoffice)
    move gui stuff out of systemPackages and into users.me
[ ] package my wallpapers
[ ] package packages/_todo
