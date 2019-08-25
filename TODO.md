[ ] environment.etc."sshd/banner-line".text = config.networking.hostName
[ ] fix man-pdf:
    man-pdf 1p env
    man-pdf env

    man-pdf:
    man $options -t $command | ps2pdf - $command.pdf
