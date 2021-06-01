{ fetchurl, fetchgit, linkFarm, runCommandNoCC, gnutar }: rec {
  offline_cache = linkFarm "offline" packages;
  packages = [
    {
      name = "_sanskrit_coders_sanscript___sanscript_1.1.5.tgz";
      path = fetchurl {
        name = "_sanskrit_coders_sanscript___sanscript_1.1.5.tgz";
        url  = "https://registry.yarnpkg.com/@sanskrit-coders/sanscript/-/sanscript-1.1.5.tgz";
        sha1 = "a22222cf7a5d55f7b19e210242c59f6f819fd643";
      };
    }
  ];
}
