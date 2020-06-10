{ fetchzip, symlinkJoin, lib, scardanelli ? false }:
let
  gfs-font = name: sha256:
    fetchzip {
      inherit name sha256;
      url = "http://www.greekfontsociety-gfs.gr/_assets/fonts/${name}.zip";
      postFetch = ''
        mkdir -p $out/share/fonts
        unzip -j -o $downloadedFile "*.otf" -d $out/share/fonts/opentype
        unzip -j -o $downloadedFile "**/*.otf" -d $out/share/fonts/opentype
      '';
    };
in symlinkJoin {
  name = "gfs-fonts";
  paths = lib.mapAttrsToList gfs-font {
    GFS_Artemisia = "1q39086pr2jhv118fjfv6l1li6japv4pdjnhh1scqw06mqrmydf4";
    GFS_Baskerville = "07gx5b9b43zv74d2lay37sajd4ba2wqn3b7xzvyhn265ds9x7cxk";
    GFS_Bodoni = "0jhl0728ikzha1krm01sk52nz3jzibidwmyvgidg61d87l8nbf2p";
    GFS_Bodoni_Classic = "06jw2irskn75s50mgwkx08rzwqi82gpc6lgjsimsi8p81566gfrh";
    GFS_Complutum =
      "1q7dxs2z3yrgchd2pz9h72mjrk62kdc2mmqw8kg9q76k28f8n3p0"; # -> GFSPolyglot.otf
    GFS_Decker = "016v1j5n9ph4i2cpmlk26pcxhp3q2fjwlaryppd5akl84dfkpncl";
    GFS_Didot = "0ysvrp527wm0wxfp6wmlgmxfx7ysr5mwpmjmqp1h605cy44jblfm";
    GFS_Didot_Classic = "0n5awqksvday3l3d85yhwmbmfj9bcpxivy4wpd4zrkgl7b85af2c";
    GFS_Didot_Display = if scardanelli then
      "0n2di2zyc76w6f8mc6hfilc2ir6igks7ldjp9fkw1gjp06330fi7"
    else
      "0gg5xb8jic646gz9p05ry62nk7w0k546fxp5p4vvnawmckql2dj1";
    GFS_Elpis = "02l7wd3nbn1kpv7ghxh19k4dbvd49ijyxd6gq83gcr9vlmxcq2s2";
    GFS_Gazis = "0x9iwj6pinaykrds0iw6552hf256d0dr41sipdb1jnnlr2d3bf9w";
    GFS_Goschen = "1jvbn33wzq2yj0aygwy9pd2msg3wkmdp0npjzazadrmfjpnpkcy9";
    GFS_NeoHellenic = "1ixm2frdc6i5lbn9h0h4gdsvsw2k4hny75q8ig4kgs28ac3dbzq3";
    GFS_Olga = "0f05ng02na84x9x6yhskxqwxwyabjisyl8a0k3fiah60i620p39d";
    GFS_Orpheus = if scardanelli then
      "18n6fag4pyr8jdwnsz0vixf47jz4ym8mjmppc1w3k7v27cg1z9dz"
    else
      "1rvjpvk1fx55vyp3dyxcbww1a24rm7xv5faqs735yf6lqzpkqnax";
    GFS_Orpheus_Classic = if scardanelli then
      "1rqy1kf7slw56zfhbv264yzarjisnqbqydj4f7hghiknhnmdakps"
    else
      "0d2yzwg6albmgl1d0xhsfrmxj79r42wp712rpry567nv1ry42k0a";
    GFS_Orpheus_Sans = if scardanelli then
      "02rh7z8c3h3xyfi52rn47z4finizx636d05bg5g23v0l0mqs6nkg"
    else
      "1rdlm2kmkvi4y3ii68ik8k3w3183vvb3q2pnk04lbb0ggg9w5jdb";
    GFS_Philostratos = "0zh3d0cn6b2fjbwnvmg379z20zh7w626w2bnj19xcazjvqkwhzx1";
    GFS_Porson = "0r3snwgxkdx7h34sg3s0hr8fac3rnpllq62bk44m266hj6a80a5k";
    GFS_Pyrsos = "0y0dv7y3n01bbhhnczflx1zcc7by56cffmr2xqixj2rd1nvchx0j";
    GFS_Solomos = "1mpx9mw566awvfjdfx5sbz3wz5gbnjjw56gz30mk1lw06vxf0dxz";
    GFS_Theokritos = "0haasx819x8c8yvna6pqywgi4060av2570jm34cddnz1fgnhv1b8";
  };
}
