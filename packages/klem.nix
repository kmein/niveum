# klem < klemm < klemmbrett ~ clipboard
{
  pkgs,
  lib,
  ...
} @ args: let
  cfg = eval.config;

  eval = lib.evalModules {
    modules = [
      {
        _file = toString ./klem.nix;
        imports = [(args.config or {})];
        options = {
          selection = lib.mkOption {
            default = "clipboard";
            type = lib.types.enum ["primary" "secondary" "clipboard"];
          };
          dmenu = lib.mkOption {
            default = "${pkgs.dmenu}/bin/dmenu -i -p klem";
            type = lib.types.path;
          };
          scripts = lib.mkOption {
            default = {
              pastebin = "${pkgs.curl}/bin/curl -fSs -F 'f:1=<-' ix.io";
              shorten = ''
                ${pkgs.curl}/bin/curl -fSs -F "shorten=$(${pkgs.coreutils}/bin/cat)" https://0x0.st
              '';
              "replace p.r" = "${pkgs.gnused}/bin/sed 's/\\<r\\>/krebsco.de/'";
            };
            type = lib.types.attrs;
          };
        };
      }
    ];
  };

  scriptCase = option: script: ''
    '${option}') ${toString script} ;;
  '';
in
  pkgs.writers.writeDashBin "klem" ''
    ${pkgs.xclip}/bin/xclip -selection ${cfg.selection} -out \
      | case $(echo "${
      lib.concatStringsSep "\n" (lib.attrNames cfg.scripts)
    }" | ${cfg.dmenu}) in
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList scriptCase cfg.scripts)}
        *) ${pkgs.coreutils}/bin/cat ;;
      esac \
      | tr -d '\r\n' \
      | ${pkgs.xclip}/bin/xclip -selection ${cfg.selection} -in
  ''
