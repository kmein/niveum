{
  security.sudo = {
    enable = true;
    extraConfig = ''
      Defaults pwfeedback
    '';
  };

  users.users.me.extraGroups = [ "wheel" ];
}
