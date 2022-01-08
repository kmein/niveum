{ config, pkgs, lib, ... }:
let
  classicsDictionaries = {
    Pape = pkgs.fetchzip {
      url = "http://tovotu.de/data/stardict/pape_gr-de.zip";
      sha256 = "1kmbdjqinrcxkc6jdyyrq5rl2wzhnrychyynnh91yhrjwjxlh44k";
    };
    Woodhouse = pkgs.fetchzip {
      url = "https://c.krebsco.de/Woodhouse.zip";
      sha256 = "1dvnc2679yb048q2f3hr2h34acvhan0n3iir6h9ajlrdzz48mlkq";
      stripRoot = false;
    };
    LSJ = pkgs.fetchzip {
      url = "https://github.com/nikita-moor/latin-dictionary/releases/download/2020-02-14/LiddellScott1940-stardict.zip";
      sha256 = "13rprgd9jvnhxk9735c91xr6ywr0j5jiwkjnpm3qpvy93isyjbys";
    };
    GreekMorphology = pkgs.fetchzip {
      url = "https://github.com/latin-dict/Morphologia-Graeca/releases/download/v0.5/morphology-mobile-goldendict.oxia.zip";
      sha256 = "0m75cppjjjmvv18cs7yh9f4p7ckqzxfznnndgkiw3yrfd50k8p96";
    };
    Frisk = pkgs.fetchzip {
      url = "https://github.com/latin-dict/Frisk1960/releases/download/v1.1/Frisk1960-stardict.zip";
      sha256 = "1rk5a3n3fpfdcmg4bc5945m88s6ldxql8cjn4jqs33rgklh7n046";
    };
    Georges-De-Lat = pkgs.fetchzip {
      url = "http://tovotu.de/data/stardict/georges_de-lat.zip";
      sha256 = "1gx4vv64bi9lxw2zgd861j469jvw4f2hhfwy1gglb12id8r7rdrl";
    };
    Georges-Lat-De = pkgs.fetchzip { # TODO find out why this does not work with sdcv
      url = "http://tovotu.de/data/stardict/georges_lat-de.zip";
      sha256 = "0cc5xipn60anxvq8z2mw53d4gi1k92wbrj9m4ws3g9rh87fmkvgz";
    };
    LewisShort = pkgs.fetchzip {
      url = "https://github.com/latin-dict/LewisShort1879/releases/download/v1.3/LewisShort1879-stardict.zip";
      sha256 = "1y3ans47iv8bzzb1paimdqvcid8ms04ikjbqy3iw077i2js3qbjk";
    };
    DoederleinSynonymes = pkgs.fetchzip {
      url = "https://github.com/latin-dict/Doederlein1874/releases/download/v1.1/Doederlein1875-stardict.zip";
      sha256 = "0mhik7gjxl8ncr9g5z2l4pfk60k1c5n0gc1w0cnp2x1v6lqvb57h";
    };
  };

  englishGermanDictionaries = {
    Etymonline = pkgs.fetchzip {
      url = "http://tovotu.de/data/stardict/etymonline.zip";
      sha256 = "1bjja3n3layfd08xa1r0a6375dxh5zi6hlv7chkhgnx800cx7hxn";
    };
    Roget = builtins.fetchTarball {
      url = "http://download.huzheng.org/bigdict/stardict-Roget_s_II_The_New_Thesaurus_3th_Ed-2.4.2.tar.bz2";
      sha256 = "1szyny9497bpyyccf9l5kr3bnw0wvl4cnsd0n1zscxpyzlsrqqbz";
    };
    JargonFile = builtins.fetchTarball {
      url = "http://download.huzheng.org/dict.org/stardict-dictd-jargon-2.4.2.tar.bz2";
      sha256 = "096phar9qpmm0fnaqv5nz8x9lpxwnfj78g4vjfcfyd7kqp7iqla4";
    };
    Oxford-Collocations = builtins.fetchTarball {
      url = "http://download.huzheng.org/bigdict/stardict-Oxford_Collocations_Dictionary_2nd_Ed-2.4.2.tar.bz2";
      sha256 = "1zkfs0zxkcn21z2lhcabrs77v4ma9hpv7qm119hpyi1d8ajcw07q";
    };
    Langenscheidt-Deu-En = builtins.fetchTarball {
      url = "http://download.huzheng.org/babylon/german/stardict-Handw_rterbuch_Deutsch_Englisc-2.4.2.tar.bz2";
      sha256 = "12q9i5azq7ylyrpb6jqbaf1rxalc3kzcwjvbinvb0yabdxb80y30";
    };
    Langenscheidt-En-Deu = builtins.fetchTarball {
      url = "http://download.huzheng.org/babylon/german/stardict-Handw_rterbuch_Englisch_Deutsc-2.4.2.tar.bz2";
      sha256 = "087b05h155j5ldshfgx91pz81h6ijq2zaqjirg7ma8ig3l96zb59";
    };
    Duden_Das_Fremdworterbuch = builtins.fetchTarball {
      url = "http://download.huzheng.org/babylon/german/stardict-Duden_Das_Fremdworterbuch-2.4.2.tar.bz2";
      sha256 = "1zrcay54ccl031s6dvjwsah5slhanmjab87d81rxlcy8fx0xd8wq";
    };
    Duden_De_De = builtins.fetchTarball {
      url = "http://download.huzheng.org/babylon/german/stardict-Duden_De_De-2.4.2.tar.bz2";
      sha256 = "1fhay04w5aaj83axfmla2ql34nb60gb05dgv0k94ig7p8x4yxxlf";
    };
    ConciseOED = builtins.fetchTarball {
      url = "http://download.huzheng.org/bigdict/stardict-Concise_Oxford_English_Dictionary-2.4.2.tar.bz2";
      sha256 = "19kpcxbhqzpmhi94mp48nalgmsh6s7rsx1gb4kwkhirp2pbjcyl7";
    };
    # Duden_Rechtschreibung = builtins.fetchTarball {
    #   url = "http://download.huzheng.org/babylon/german/stardict-Duden_Rechtschreibung-2.4.2.tar.bz2";
    #   sha256 = "0xiprb45s88w62rn8rlbjrsagbiliay9hszsiy20glwabf6zsfji";
    # };
    Duden_Synonym = builtins.fetchTarball {
      url = "http://download.huzheng.org/babylon/german/stardict-Duden_Synonym-2.4.2.tar.bz2";
      sha256 = "0cx086zvb86bmz7i8vnsch4cj4fb0cp165g4hig4982zakj6f2jd";
    };
    # Duden = builtins.fetchTarball {
    #   url = "http://download.huzheng.org/de/stardict-duden-2.4.2.tar.bz2";
    #   sha256 = "049i4ynfqqxykv1nlkyks94mvn14s22qdax5gg7hx1ks5y4xw64j";
    # };
    # FreeOnlineDictionaryOfComputing = builtins.fetchTarball {
    #   url = "http://download.huzheng.org/dict.org/stardict-dictd_www.dict.org_foldoc-2.4.2.tar.bz2";
    #   sha256 = "1lw2i8dzxpx929cpgvv0x366dnh4drr10wzqmrhcd0kvwglqawgm";
    # };
  };

  sanskritDictionaries = {
    BoehtlingkRoth = pkgs.fetchzip {
      url = "https://c.krebsco.de/Bohtlingk-and-Roth-Grosses-Petersburger-Worterbuch.zip";
      sha256 = "13414a8rgd7hd5ffar6nl68nk3ys60wjkgb7m11hp0ahaasmf6ly";
      stripRoot = false;
    };
    MonierWilliams = pkgs.fetchzip {
      url = "https://c.krebsco.de/mw-cologne.zip";
      sha256 = "0p99ybxwxmmd94hf035hvm2hhnfy84av7qq79xf28bh2rbx6s9ng";
      stripRoot = false;
    };
    MonierWilliamsEnglish = pkgs.fetchzip {
      url = "https://c.krebsco.de/mw-english-sanskrit.zip";
      sha256 = "09a61hhii4b1m2fkrlh4rm2xnlgwrllh84iypbc6wyj00w9jkl3x";
      stripRoot = false;
    };
  };
  makeStardictDataDir = dicts: pkgs.linkFarm "dictionaries" (lib.mapAttrsToList (name: path: { inherit name path; }) dicts);

  sdcvPager = pkgs.writeDash "sdcvPager" ''
    export PATH=${lib.makeBinPath [pkgs.gnused pkgs.ncurses]}
    sed "
      s! style=\"color: #...\"!!g;
      s!<span class=\"zenoTXSpaced\">\([^<>]*\)</span>!\1!g;
      s!</\?dictionary[^>]*>!!g;
      s!<style.*</style>!!g;
      s!<author>\([^<>]*\)</author>!\1 !g;
      s!<quote lang=\"\(greek\|la\)\">\([^<>]*\)</quote>!$(tput sitm)\2$(tput sgr0)!g;
      s!<biblScope>\([^<>]*\)</biblScope>!\1!g;
      s!<mood>\([^<>]*\)</mood>!$(tput sitm)\1$(tput sgr0)!g;
      s!<adv>\([^<>]*\)</adv>!$(tput sitm)\1$(tput sgr0)!g;
      s!<gram[^>]*>\([^<>]*\)</gram>!$(tput sitm)\1$(tput sgr0)!g;
      s!<bibl_title>\([^<>]*\)</bibl_title>!$(tput sitm)\1$(tput sgr0) !g;
      s!<hi rend=\"ital\">\([^<>]*\)</hi>!$(tput sitm)\1$(tput sgr0) !g;
      s!<dict_tr>\([^<>]*\)</dict_tr>!$(tput setaf 3)\1$(tput sgr0)!g;
      s!<headword>\([^<>]*\)</headword>!$(tput bold)\1$(tput sgr0)\t!g;
      s!</\?a[^>]*>!!g
      s!</\?[cp]b[^>]*>!!g
      s!</\?gramGrp[^>]*>!!g
      s!</\?lbl[^>]*>!!g
      s!</\?xr[^>]*>!!g
      s!</\?pron[^>]*>!!g
      s!</\?gen[^>]*>!!g
      s!</\?etym[^>]*>!!g
      s!<foreign[^>]*>!$(tput sitm)!g
      s!</foreign[^>]*>!$(tput sgr0)!g
      s!</\?date[^>]*>!!g
      s!</\?placeName[^>]*>!!g
      s!</\?itype[^>]*>!!g
      s!</\?p>!!g
      s!<input[^>]*>!!g
      s!</\?orth[^>]*>!!g
      s!</\?forename[^>]*>!!g
      s!</\?persName[^>]*>!!g
      s!</\?surname[^>]*>!!g
      s!</\?entryFree[^>]*>!!g
      s!</\?def[^>]*>!!g
      s!</\?cit[^>]*>!!g
      s!</\?pos[^>]*>!!g
      s!</\?usg[^>]*>!!g
      s!</\?span>!!g
      s!<bibl[^>]*>!$(tput setaf 245)!g
      s!</bibl[^>]*>!$(tput sgr0)!g
      s/<dt>/$(tput bold)/g;
      s:</dt>:$(tput sgr0):g;
      s/<dd>/\n/g;
      s:</dd>::g;
      s/<[bB]>/$(tput bold)/g;
      s:</[bB]>:$(tput sgr0):g;
      s:<[bB][rR]\s*/\?>:\n:g;
      s:<[iI]>:$(tput sitm):g;
      s:</[iI]>:$(tput sgr0):g;
      s:<[uU]>:$(tput smul):g;
      s:</[uU]>:$(tput sgr0):g;
      s:<FONT face=[^>]*>::g;
      s:</FONT>::g;
      s!<head>\([^<>]*\)</head>!$(tput bold)\1$(tput sgr0)!g;
      s!<span lang=\"\(gr\|la\)\">\([^<>]*\)</span>!\2!g
      s#<div style=\"margin-left:1em\">\(.*\)</div>#\\1#g;
      s:<font color=\"brown\">\([^<>]*\)</font>:$(tput setaf 3)\\1$(tput sgr0):g;
      s:<font color=\"blue\">\([^<>]*\)</font>:$(tput setaf 4)\\1$(tput sgr0):g;
      s:<font color=\"red\">\([^<>]*\)</font>:$(tput setaf 1)\\1$(tput sgr0):g;
      s:<font color=\"darkviolet\">\([^<>]*\)</font>:$(tput setaf 5)\\1$(tput sgr0):g;
      s:<font color=\"#a0a\">\([^<>]*\)</font>:$(tput bold)\1$(tput sgr0):g
      s:<font color=\"#838\">\([^<>]*\)</font>:$(tput setaf 3)\1$(tput sgr0):g
      s:&#x27;:':g
      s:&lt;:<:g
      s:&gt;:>:g
      s:<font color=\"#007000\">\([^<>]*\)</font>:$(tput setaf 2)\\1$(tput sgr0):g;
      s:<font color=\"#007000\">\([^<>]*\)</font>:$(tput setaf 2)\\1$(tput sgr0):g;
      s:<font color=#000099>\([^<>]*\)</font>:$(tput setaf 4)\\1$(tput sgr0):g;
      s:<font color=0000FF>\([^<>]*\)</font>:$(tput bold)\\1$(tput sgr0):g;
      s:<IMG src=\"223E9A06.bmp\"[^>]*>:ː:g;
      s:<IMG src=\"502F5DDA.bmp\"[^>]*>::g;
      s!</\?TABLE>!!g
      s!</\?TR[^>]*>!!g
      s!</\?TD>!!g
      s!</\?FONT[^>]*>!!g
      s!</\?A[^>]*>!!g
      s!<SPAN class=\"bsptext\">\([^<>]*\)</SPAN>!$(tput setaf 245)\1$(tput sgr0)!g
      s!</\?SPAN[^>]*>!!g
      s! +! !g;
      s!<div part=\"[^\"]*\">!\n\n&!g
      s!<sense n=\"\([^\"]*\)\"!\n$(tput setaf 5)\1.$(tput sgr0) &!g;
      s!</\?sense[^>]*>!!g
      s!</\?div[^>]*>!!g
      s!<span lang=\"gr\">!!g # unbalanced in Frisk
      s!^\s*[0-9])!$(tput setaf 5)&$(tput sgr0)!g
      s#^\(-->.*\)\$#$(tput bold)\1$(tput sgr0)#
    "
  '';
in
{
  # https://github.com/latin-dict/Georges1910/releases/download/v1.0/Georges1910-stardict.zip
  # https://github.com/nikita-moor/latin-dictionary/releases/download/2020-02-14/LiddellScott1940-stardict.zip
  # http://download.huzheng.org/bigdict/stardict-Cambridge_Dictionary_of_American_Idioms-2.4.2.tar.bz2
  # http://download.huzheng.org/bigdict/stardict-Concise_Oxford_Thesaurus_2nd_Ed-2.4.2.tar.bz2
  # http://download.huzheng.org/bigdict/stardict-Urban_Dictionary_P1-2.4.2.tar.bz2
  # http://download.huzheng.org/bigdict/stardict-Urban_Dictionary_P2-2.4.2.tar.bz2
  environment.etc.stardict.source = toString (makeStardictDataDir (classicsDictionaries // {
    Crum = builtins.fetchTarball {
      url = "http://download.huzheng.org/misc/stardict-Coptic-English_all_dialects-2.4.2.tar.bz2";
      sha256 = "1fi281mb9yzv40wjsdapi8fzpa7x2yscz582lv2qnss9g8zzzzr9";
    };
    LingvoGermanRussian = builtins.fetchTarball {
      url = "http://download.huzheng.org/lingvo/stardict-GR-LingvoUniversal-2.4.2.tar.bz2";
      sha256 = "0p353gs2z4vj70hqsdhffjaaw3a4zlmcs46flipmf35lm5wmaj0g";
    };
    LingvoRussianGerman = builtins.fetchTarball {
      url = "http://download.huzheng.org/lingvo/stardict-RG-LingvoUniversal-2.4.2.tar.bz2";
      sha256 = "03f9wdmkgpjifpms7dyh10ma29wf3ka1j3zlp1av0cybhdldk2a8";
    };
    SmithBiographyMythology = pkgs.fetchzip {
      url = "https://github.com/latin-dict/Smith1873/releases/download/v1.0/Smith1873-stardict.zip";
      sha256 = "01h5fxacp2m60xir8kzslkfy772vs3vmz07zhdwfhcwdaxif2af2";
    };
    SmithAntiquities = pkgs.fetchzip {
      url = "https://github.com/latin-dict/Smith1890/releases/download/v1.0/Smith1890-stardict.zip";
      sha256 = "0vpsv62p2lrzmgys4d1swpnc6lqhdi7rxwkj2ngy3lz5dk3fysyb";
    };
    OED1 = builtins.fetchTarball {
      url = "http://download.huzheng.org/bigdict/stardict-Oxford_English_Dictionary_2nd_Ed._P1-2.4.2.tar.bz2";
      sha256 = "0i5vv1rv44yfwyf9bfbdrb9brzhhpvz2jnh39fv8hh107nkv2vcf";
    };
    OED2 = builtins.fetchTarball {
      url = "http://download.huzheng.org/bigdict/stardict-Oxford_English_Dictionary_2nd_Ed._P2-2.4.2.tar.bz2";
      sha256 = "1pk234pbq4pk55d8sjk0pp9j5sajm82f8804kf2xm2x5p387q1rg";
    };
  } // sanskritDictionaries // englishGermanDictionaries));

  environment.variables = {
    SDCV_PAGER = toString sdcvPager;
  };

  home-manager.users.me = {
    home.file.".goldendict/config".text = import <niveum/lib/goldendict-config.nix> {
      path = "/etc/stardict";
      inherit pkgs;
    };
  };

  environment.systemPackages = [
    pkgs.goldendict
    (pkgs.writers.writeDashBin "sd-classics" ''${pkgs.sdcv}/bin/sdcv --data-dir ${makeStardictDataDir classicsDictionaries} "$@"'')
    (pkgs.writers.writeDashBin "sd-sanskrit" ''${pkgs.sdcv}/bin/sdcv --data-dir ${makeStardictDataDir sanskritDictionaries} "$@"'')
    (pkgs.writers.writeDashBin "sd" ''${pkgs.sdcv}/bin/sdcv --data-dir ${makeStardictDataDir englishGermanDictionaries} "$@"'')
  ];
}
