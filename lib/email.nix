let
  thunderbirdProfile = "donnervogel";
in
{
  inherit thunderbirdProfile;
  pronouns = builtins.concatStringsSep "/" [
    "er"
    "he"
    "is"
    "οὗτος"
    "هو"
    "ⲛ̄ⲧⲟϥ"
    "он"
    "han"
    "सः"
  ];
  defaults = {
    thunderbird = {
      enable = true;
      profiles = [ thunderbirdProfile ];
    };
    aerc.enable = true;
    realName = "Kierán Meinhardt";
    folders.inbox = "INBOX";
  };
}
