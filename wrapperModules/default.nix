# lassulus/wrappers modules: each wraps one program with its configuration
# rendered into the store and passed on the command line.
{ lib, wlib }:
lib.genAttrs [ "aerc" "mbsync" "msmtpq" "omp" ] (name: wlib.wrapModule (import ./${name}.nix))
