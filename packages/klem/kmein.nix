{ pkgs, ... }:
let
  inherit (pkgs) lib;
in
{
  dmenu = "${lib.getExe pkgs.dmenu} -i -p klem";
  scripts = {
    "p.r paste" = pkgs.writers.writeDash "p.r" ''
      ${lib.getExe pkgs.curl} -fSs http://p.r --data-binary @- \
        | ${lib.getExe' pkgs.coreutils "tail"} --lines=1 \
        | ${lib.getExe pkgs.gnused} 's/\\<r\\>/krebsco.de/'
    '';
    "envs.sh paste" = pkgs.writers.writeDash "envs-host" ''
      ${lib.getExe pkgs.curl} -F "file=@-" https://envs.sh
    '';
    # this segfaults
    # "envs.sh mirror" = pkgs.writers.writeDash "envs-mirror" ''
    #   ${pkgs.curl}/bin/curl -F "url=$(${pkgs.coreutils}/bin/cat)" https://envs.sh
    # '';
    "envs.sh shorten" = pkgs.writers.writeDash "envs-shorten" ''
      ${lib.getExe pkgs.curl} -F "shorten=$(${lib.getExe' pkgs.coreutils "cat"})" https://envs.sh
    '';
    "go.r shorten" = pkgs.writers.writeDash "go.r" ''
      ${lib.getExe pkgs.curl} -fSs http://go.r -F "uri=$(${lib.getExe' pkgs.coreutils "cat"})"
    '';
    "4d2.org paste" = pkgs.writers.writeDash "4d2-paste" ''
      ${lib.getExe pkgs.curl} -F "file=@-" https://depot.4d2.org/
    '';
    "0x0.st shorten" = pkgs.writers.writeDash "0x0.st" ''
      ${lib.getExe pkgs.curl} -fSs https://0x0.st -F "shorten=$(${lib.getExe' pkgs.coreutils "cat"})"
    '';
    "rot13" = pkgs.writers.writeDash "rot13" ''
      ${lib.getExe' pkgs.coreutils "tr"} '[A-Za-z]' '[N-ZA-Mn-za-m]'
    '';
    "ipa" = pkgs.writers.writeDash "ipa" ''
      ${lib.getExe pkgs.ipa}
    '';
    "betacode" = pkgs.writers.writeDash "betacode" ''
      ${lib.getExe pkgs.betacode}
    '';
    "curl" = pkgs.writers.writeDash "curl" ''
      ${lib.getExe pkgs.curl} -fSs "$(${lib.getExe' pkgs.coreutils "cat"})"
    '';
    ocr = pkgs.writers.writeDash "ocr" ''
      ${lib.getExe pkgs.tesseract4} -l eng+deu - stdout
    '';
    emojai = pkgs.writers.writeDash "emojai" ''
      ${lib.getExe pkgs.curl} https://www.emojai.app/api/generate -X POST -H 'Content-Type: application/json' --data-raw "$(${lib.getExe pkgs.jq} -sR '{emoji:.}')" | ${lib.getExe pkgs.jq} -r .result
    '';
  };
}
