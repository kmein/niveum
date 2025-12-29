{
  config,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ../../configs/spacetime.nix
    ../../configs/admin-essentials.nix
    ../../configs/keyboard
    ../../configs/sound.nix
    ../../configs/printing.nix
    ../../configs/nix.nix
    ../../configs/fonts.nix
    ../../configs/mycelium.nix
    ../../configs/retiolum.nix
    ../../configs/sshd.nix
    ../../configs/sudo.nix
    ../../configs/zsh.nix
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
  services.desktopManager.pantheon.enable = true;
  # services.displayManager.autoLogin.enable = true;
  # services.displayManager.autoLogin.user = config.users.users.me.name;

  home-manager.users.me.home.stateVersion = "24.11";

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
      tor-browser
      zathura
      kdePackages.okular
      anki-bin
      libreoffice
      xournalpp
      mpv-tv
      telegram-desktop
      (mpv-radio.override { di-fm-key-file = config.age.secrets.di-fm-key.path; })
      spotify
    ];
  };

  environment.systemPackages = with pkgs; [
    htop
    git
    vim
    tmux
    (vim-kmein.override { colorscheme = "base16-gruvbox-dark-medium"; })
  ];

  system.stateVersion = "23.11";
}
