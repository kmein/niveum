{
  commaSep = strs: builtins.concatStringsSep "," strs;

  ignoredFiles = [ "*~" ".stack-work/" "__pycache__/" ".mypy_cache/" "*.o" "*.hi" "*.aux" "*.class" "*.dyn_hi" "*.dyn_o" ];

  user = {
    github = "kmein";
    name = "Kierán Meinhardt";
    email = "kieran.meinhardt@gmail.com";
  };
}
