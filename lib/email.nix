rec {
  thunderbirdProfile = "donnervogel";
  defaults = {
    thunderbird = {
      enable = true;
      profiles = [thunderbirdProfile];
    };
    aerc.enable = true;
    realName = "Kierán Meinhardt";
    folders.inbox = "INBOX";
  };
}
