{
  config,
  pkgs,
  niveumPackages,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ../../configs/spacetime.nix
    ../../configs/admin-essentials.nix
    ../../configs/keyboard.nix
    ../../configs/sound.nix
    ../../configs/printing.nix
    ../../configs/nix.nix
    ../../configs/flix.nix
    ../../configs/fonts.nix
    ../../configs/mycelium.nix
    ../../configs/retiolum.nix
    ../../configs/sshd.nix
    ../../configs/sudo.nix
    ../../configs/zsh.nix
    ../../configs/tor.nix
  ];

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/kibbeh-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/kibbeh-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
  };

  services.gnome.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;

  services.openssh.enable = true;

  networking.hostName = "kibbeh";
  networking.networkmanager.enable = true;

  i18n.defaultLocale = "en_DK.UTF-8";

  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.pantheon.enable = true;
  # services.displayManager.autoLogin.enable = true;
  # services.displayManager.autoLogin.user = config.users.users.me.name;

  age.secrets = {
    di-fm-key.file = ../../secrets/di-fm-key.age;
  };

  users.users.me = {
    name = "kfm";
    isNormalUser = true;
    description = "किरण";
    extraGroups = [ "networkmanager" ];
    password = "hackme";
    packages = with pkgs; [
      # packages TODO
      firefox
      thunderbird
      alacritty
      tor-browser-bundle-bin
      zathura
      okular
      anki-bin
      libreoffice
      xournalpp
      jellyfin-media-player
      niveumPackages.mpv-tv
      telegram-desktop
      (niveumPackages.mpv-radio.override { di-fm-key-file = config.age.secrets.di-fm-key.path; })
      niveumPackages.meteo
      spotify
    ];
  };

  environment.systemPackages = with pkgs; [
    htop
    git
    vim
    tmux
    (niveumPackages.vim.override { colorscheme = "base16-gruvbox-dark-medium"; })
  ];

  system.stateVersion = "23.11";
}
