# expose lib.niveum (constants + helpers from ../lib) via pkgs.lib
final: prev: {
  lib = prev.lib // {
    niveum = import ../lib {
      lib = prev.lib;
      pkgs = final;
    };
  };
}
