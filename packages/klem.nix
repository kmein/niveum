# klem < klemm < klemmbrett ~ clipboard
{
  lib,
  dmenu,
  curl,
  gnused,
  coreutils,
  xclip,
  libnotify,
  writers,
  options ? { },
  ...
}:
let
  eval = lib.evalModules {
    modules = [
      {
        imports = [ options ];
        options = {
          selection = lib.mkOption {
            default = "clipboard";
            type = lib.types.enum [
              "primary"
              "secondary"
              "clipboard"
            ];
          };
          dmenu = lib.mkOption {
            default = "${dmenu}/bin/dmenu -i -p klem";
            type = lib.types.path;
          };
          scripts = lib.mkOption {
            default = {
              pastebin = "${curl}/bin/curl -fSs -F 'f:1=<-' ix.io";
              shorten = ''
                ${curl}/bin/curl -fSs -F "shorten=$(${coreutils}/bin/cat)" https://0x0.st
              '';
              "replace p.r" = "${gnused}/bin/sed 's/\\<r\\>/krebsco.de/'";
            };
            type = lib.types.attrs;
          };
        };
      }
    ];
  };

  cfg = eval.config;
in
writers.writeDashBin "klem" ''
  set -efu

  ${xclip}/bin/xclip -selection ${cfg.selection} -out \
    | case $(echo "${lib.concatStringsSep "\n" (lib.attrNames cfg.scripts)}" | ${cfg.dmenu}) in
      ${lib.concatStringsSep "\n" (
        lib.mapAttrsToList (option: script: ''
          '${option}') ${toString script} ;;
        '') cfg.scripts
      )}
      *) ${coreutils}/bin/cat ;;
    esac \
    | ${xclip}/bin/xclip -selection ${cfg.selection} -in

  ${libnotify}/bin/notify-send --app-name="klem" "Result copied to clipboard."
''
