{
  lib,
  fetchzip,
  linkFarm,
  symlinkJoin,
  writers,
  sdcv,
  gnused,
  ncurses,
  less,
}:
let
  locker = x: "https://c.krebsco.de/${x}";
  dictionaries = {
    lojban = {
      jbo-deu = fetchzip {
        url = "https://guskant.github.io/lojbo/stardict/jbo-deu-20160110.tar.gz";
        sha256 = "18ia15wyvd8ksi7yxn939qmvpdlrip8mvfywafv7vjj685rdhk80";
      };
      rafsi-jbo = fetchzip {
        url = "https://guskant.github.io/lojbo/stardict/rafsi-jbo-20160110.tar.gz";
        sha256 = "00fkw964b48liz1jayfjb5jnpwihghkq4i28y8i11yqb56w6bn3c";
      };
      jbo-eng = fetchzip {
        url = "https://guskant.github.io/lojbo/stardict/jbo-eng-20160110.tar.gz";
        sha256 = "15l65yshqdp0a7fr4a7ffwx3m2v6ymchxgra9zmk5bsgcxsb4r8m";
      };
      jbo-jbo = fetchzip {
        url = "https://guskant.github.io/lojbo/stardict/jbo-jbo-20160110.tar.gz";
        sha256 = "1psfyrjrq84prr2s4jcm31ijykm7clyqygmbacr09n570xfwqcfw";
      };
    };
    classics = {
      Pape = fetchzip {
        url = "http://tovotu.de/data/stardict/pape_gr-de.zip";
        sha256 = "1kmbdjqinrcxkc6jdyyrq5rl2wzhnrychyynnh91yhrjwjxlh44k";
      };
      Woodhouse = fetchzip {
        url = locker "Woodhouse.zip";
        sha256 = "1dvnc2679yb048q2f3hr2h34acvhan0n3iir6h9ajlrdzz48mlkq";
        stripRoot = false;
      };
      LSJ = fetchzip {
        url = "https://github.com/nikita-moor/latin-dictionary/releases/download/2020-02-14/LiddellScott1940-stardict.zip";
        sha256 = "13rprgd9jvnhxk9735c91xr6ywr0j5jiwkjnpm3qpvy93isyjbys";
      };
      GreekMorphology = fetchzip {
        url = "https://github.com/latin-dict/Morphologia-Graeca/releases/download/v0.5/morphology-mobile-goldendict.oxia.zip";
        sha256 = "0m75cppjjjmvv18cs7yh9f4p7ckqzxfznnndgkiw3yrfd50k8p96";
      };
      Frisk = fetchzip {
        url = "https://github.com/latin-dict/Frisk1960/releases/download/v1.1/Frisk1960-stardict.zip";
        sha256 = "1rk5a3n3fpfdcmg4bc5945m88s6ldxql8cjn4jqs33rgklh7n046";
      };
      Georges-De-Lat = fetchzip {
        url = "http://tovotu.de/data/stardict/georges_de-lat.zip";
        sha256 = "1gx4vv64bi9lxw2zgd861j469jvw4f2hhfwy1gglb12id8r7rdrl";
      };
      Georges-Lat-De = fetchzip {
        url = "http://tovotu.de/data/stardict/georges_lat-de.zip";
        sha256 = "0cc5xipn60anxvq8z2mw53d4gi1k92wbrj9m4ws3g9rh87fmkvgz";
      };
      LewisShort = fetchzip {
        url = "https://github.com/latin-dict/LewisShort1879/releases/download/v1.3/LewisShort1879-stardict.zip";
        sha256 = "1y3ans47iv8bzzb1paimdqvcid8ms04ikjbqy3iw077i2js3qbjk";
      };
      DoederleinSynonymes = fetchzip {
        url = "https://github.com/latin-dict/Doederlein1874/releases/download/v1.1/Doederlein1875-stardict.zip";
        sha256 = "0mhik7gjxl8ncr9g5z2l4pfk60k1c5n0gc1w0cnp2x1v6lqvb57h";
      };
    };
    englishGerman = {
      Etymonline = fetchzip {
        url = "http://tovotu.de/data/stardict/etymonline.zip";
        sha256 = "1bjja3n3layfd08xa1r0a6375dxh5zi6hlv7chkhgnx800cx7hxn";
      };
      Roget = fetchzip {
        url = locker "stardict-Roget_s_II_The_New_Thesaurus_3th_Ed-2.4.2.tar.bz2";
        hash = "sha256-f2GcNf3+dqZ/sKBpywjdHHC7Rp6FJseY93edRJK3/us=";
      };
      JargonFile = fetchzip {
        url = locker "stardict-dictd-jargon-2.4.2.tar.bz2";
        hash = "sha256-RFEcz8XzNO+Yk5s8dKSzvF+aOvq2bKysA7VenLKC1yQ=";
      };
      Oxford-Collocations = fetchzip {
        url = locker "stardict-Oxford_Collocations_Dictionary_2nd_Ed-2.4.2.tar.bz2";
        sha256 = "1zkfs0zxkcn21z2lhcabrs77v4ma9hpv7qm119hpyi1d8ajcw07q";
      };
      Langenscheidt-Deu-En = fetchzip {
        url = locker "stardict-Handw_rterbuch_Deutsch_Englisc-2.4.2.tar.bz2";
        sha256 = "12q9i5azq7ylyrpb6jqbaf1rxalc3kzcwjvbinvb0yabdxb80y30";
      };
      Langenscheidt-En-Deu = fetchzip {
        url = locker "stardict-Handw_rterbuch_Englisch_Deutsc-2.4.2.tar.bz2";
        sha256 = "087b05h155j5ldshfgx91pz81h6ijq2zaqjirg7ma8ig3l96zb59";
      };
      Duden_Das_Fremdworterbuch = fetchzip {
        url = locker "stardict-Duden_Das_Fremdworterbuch-2.4.2.tar.bz2";
        sha256 = "1zrcay54ccl031s6dvjwsah5slhanmjab87d81rxlcy8fx0xd8wq";
      };
      Duden_De_De = fetchzip {
        url = locker "stardict-Duden_De_De-2.4.2.tar.bz2";
        sha256 = "1fhay04w5aaj83axfmla2ql34nb60gb05dgv0k94ig7p8x4yxxlf";
      };
      ConciseOED = fetchzip {
        url = locker "stardict-Concise_Oxford_English_Dictionary-2.4.2.tar.bz2";
        sha256 = "19kpcxbhqzpmhi94mp48nalgmsh6s7rsx1gb4kwkhirp2pbjcyl7";
      };
      Duden_Synonym = fetchzip {
        url = locker "stardict-Duden_Synonym-2.4.2.tar.bz2";
        sha256 = "0cx086zvb86bmz7i8vnsch4cj4fb0cp165g4hig4982zakj6f2jd";
      };
    };
    sanskrit =
      let
        repo = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f";
      in
      {
        BoehtlingkRoth = fetchzip {
          url = "${repo}/sa-head/german-entries/tars/Bohtlingk-and-Roth-Grosses-Petersburger-Worterbuch__2021-10-05_14-23-18Z__19MB.tar.gz";
          sha256 = "13414a8rgd7hd5ffar6nl68nk3ys60wjkgb7m11hp0ahaasmf6ly";
          stripRoot = false;
        };
        BoehtlingkRothKurz = fetchzip {
          url = "${repo}/sa-head/german-entries/tars/Bohtlingk-Sanskrit-Worterbuch-in-kurzerer-Fassung__2021-10-05_14-23-18Z__10MB.tar.gz";
          sha256 = "15yx31yrk40k9nn6kaysp4pprzj8dpd13dj3wafklc3izm8lr2wq";
          stripRoot = false;
        };
        MonierWilliams = fetchzip {
          url = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f/sa-head/en-entries/tars/mw-cologne__2021-10-06_00-16-23Z__16MB.tar.gz";
          sha256 = "0p99ybxwxmmd94hf035hvm2hhnfy84av7qq79xf28bh2rbx6s9ng";
          stripRoot = false;
        };
        MonierWilliamsEnglish = fetchzip {
          url = "${repo}/en-head/tars/mw-english-sanskrit__2021-10-05_14-23-18Z__3MB.tar.gz";
          sha256 = "09a61hhii4b1m2fkrlh4rm2xnlgwrllh84iypbc6wyj00w9jkl3x";
          stripRoot = false;
        };
        Borooah = fetchzip {
          url = "${repo}/en-head/tars/borooah__2021-10-05_14-23-18Z__2MB.tar.gz";
          sha256 = "0qmmfbynqgv125v48383i51ky9yi69zibhh7vwk95gyar2yrprn2";
          stripRoot = false;
        };
        ApteEnglish = fetchzip {
          url = "${repo}/en-head/tars/apte-english-sanskrit-cologne__2021-10-06_00-12-51Z__1MB.tar.gz";
          sha256 = "064ysm24ydc534ca689y5i2flnra8jkmh8zn0gsb6n8hdsb0d1lq";
          stripRoot = false;
        };
      };
    oed = {
      OED1 = fetchzip {
        url = locker "stardict-Oxford_English_Dictionary_2nd_Ed._P1-2.4.2.tar.bz2";
        sha256 = "0i5vv1rv44yfwyf9bfbdrb9brzhhpvz2jnh39fv8hh107nkv2vcf";
      };
      OED2 = fetchzip {
        url = locker "stardict-Oxford_English_Dictionary_2nd_Ed._P2-2.4.2.tar.bz2";
        sha256 = "1pk234pbq4pk55d8sjk0pp9j5sajm82f8804kf2xm2x5p387q1rg";
      };
    };
    coptic = {
      Crum = fetchzip {
        url = locker "stardict-Coptic-English_all_dialects-2.4.2.tar.bz2";
        sha256 = "1fi281mb9yzv40wjsdapi8fzpa7x2yscz582lv2qnss9g8zzzzr9";
      };
    };
    russian = {
      LingvoGermanRussian = fetchzip {
        url = locker "stardict-GR-LingvoUniversal-2.4.2.tar.bz2";
        sha256 = "0p353gs2z4vj70hqsdhffjaaw3a4zlmcs46flipmf35lm5wmaj0g";
      };
      LingvoRussianGerman = fetchzip {
        url = locker "stardict-RG-LingvoUniversal-2.4.2.tar.bz2";
        sha256 = "03f9wdmkgpjifpms7dyh10ma29wf3ka1j3zlp1av0cybhdldk2a8";
      };
    };
    turkish = {
      BabylonTurkishEnglish = fetchzip {
        url = locker "stardict-babylon-Babylon_Turkish_English-2.4.2.tar.bz2";
        sha256 = "1zpzgk3w0536gww31bj58cmn3imnkndyjwbcr7bay8ibq2kzv44z";
      };
      BabylonEnglishTurkish = fetchzip {
        url = locker "stardict-babylon-Babylon_English_Turkish-2.4.2.tar.bz2";
        sha256 = "0myx31xzb7nrn5m657h0bwdgm5xp93ccwp6lcpbxgjxdjm3q0hc5";
      };
    };
    wiktionary =
      let
        release = "https://github.com/kmein/kaikki-stardict/releases/download/2026-11-05";
      in
      lib.mapAttrs
        (
          language: hash:
          fetchzip {
            url = "${release}/${language}.zip";
            inherit hash;
            stripRoot = false;
          }
        )
        {
          Arabic = "sha256-Hf1jOhL0wf7x1QIAk1T9h/UB9qO1aPViRJWs2pSynDU=";
          Danish = "sha256-dDuo0tKPWOtPz6O+IFx4Nrwq6JF2HmGEV+dYSS3qRLw=";
          Egyptian = "sha256-6PbW+jLPZx1LBHLcMBHIVV5uokAVS0x3fQ1qGNll42A=";
          Gothic = "sha256-NblixiNu6z1rMfyJutgchXw3To8dYALwQtrEbMxxAgY=";
          Hebrew = "sha256-lONP+7pThoVR274H2ezCBe9DNotK/sYtvHRcy7b8iIE=";
          Hindi = "sha256-m1Hy+D1TIJk/zu/ooqSfBR1kmp4dzuFplTvpGJmy858=";
          Old_Church_Slavonic = "sha256-I1zmmw1q4DkQm6ZO8f37s3jsj1W9JOS/UR8sFFP+Ue8=";
          Old_English = "sha256-BtHaD/YFgbms6cUj3I19pa5UtK/VRv9UYFoCUTyDZSM=";
          Old_High_German = "sha256-R4XGzo6rElFZZNexNf2OWgd+AgwWmc/0YwjARJq5sEM=";
          Old_Norse = "sha256-tAyJMo7syER48lVKEBFD58mdKi0Twk0BoSwJjFim4Ac=";
          Persian = "sha256-rQcGPL/Sl3oeVamZsI+yiH+W3Nvdq1H8iaaQ3mkbJnM=";
          Proto-Brythonic = "sha256-mDib7mFSaX3Uw67c9mdigy5OzobUZgCVFwiauLYgWF4=";
          Proto-Celtic = "sha256-VkeumKFfeOxrm31BpxFtZ9jWiRbnskcavJREcHZZcYw=";
          Proto-Germanic = "sha256-REl+HNdPgq1wxTPKa34CuP91ms6gKs4YHuspcD+hbME=";
          Proto-Indo-European = "sha256-VWA6ecc7C8KdBUzsvLZZdfQI3W7qySXNZ9i2SeVmqDs=";
          Proto-Italic = "sha256-Tzifs09mHFXJ+aDQ6ANEuFK/aPTgl9WU0/5thckJ4jM=";
          Proto-Slavic = "sha256-BWdd434hiXJxWkavkbx3DxqTm9iPdokfVCNFpsHbGmM=";
          Proto-West_Germanic = "sha256-Hpg9oCyJmnp9pjIbFzsOf3NTFe5XqESkRI5bqn+/2SI=";
          Tocharian_A = "sha256-8mlz3L4YoRwgv3Eu93xh7bvCUa98VWGxiHVdKKP0BpU=";
          Tocharian_B = "sha256-+5gC9FecEG6hvotREZ6O2tgGxVGLht/OY4lAyaIWI80=";
          Turkish = "sha256-ZsoywHuwExXO5oZGI0izUfsbWbeJjXNBlfPYBgOEXUs=";
          Urdu = "sha256-i8Mr0MQcloNjXZFC+hmR1MR3zPyGkqqCOcqIBCaRs0g=";
        };
  };

  makeStardictDataDir =
    dicts: linkFarm "dictionaries" (lib.mapAttrsToList (name: path: { inherit name path; }) dicts);

  makeStardict =
    name: dicts:
    writers.writeDashBin name ''
      set -efu
      export SDCV_PAGER=${toString sdcvPager}
      exec ${sdcv}/bin/sdcv --color --only-data-dir --data-dir ${makeStardictDataDir dicts} "$@"
    '';

  sdcvPager = writers.writeDash "sdcvPager" ''
    export PATH=${
      lib.makeBinPath [
        gnused
        ncurses
        less
      ]
    }
    sed "
      s!<sup>1</sup>!¹!gI
      s!<sup>2</sup>!²!gI
      s!<sup>3</sup>!³!gI
      s!<sup>4</sup>!⁴!gI
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
      s!</\?tns[^>]*>!!g
      s!</\?per[^>]*>!!g
      s!</\?blockquote[^>]*>!!g
      s!</\?etym[^>]*>!!g
      s!<foreign[^>]*>!$(tput sitm)!g
      s!</foreign[^>]*>!$(tput sgr0)!g
      s!</\?date[^>]*>!!g
      s!</\?placeName[^>]*>!!g
      s!</\?itype[^>]*>!!g
      s!</\?p>!!g
      s!<input[^>]*>!!g
      s!</\?orth[^>]*>!!g
      s!</\?number[^>]*>!!g
      s!</\?forename[^>]*>!!g
      s!</\?persName[^>]*>!!g
      s!</\?surname[^>]*>!!g
      s!</\?entryFree[^>]*>!!g
      s!</\?def[^>]*>!!g
      s!</\?cit[^>]*>!!g
      s!</\?pos[^>]*>!!g
      s!</\?usg[^>]*>!!g
      s!</\?ul>!!g
      s!<li>!\n!g
      s!</li>!!g
      s!<bibl[^>]*>!$(tput setaf 245)!g
      s!</bibl[^>]*>!$(tput sgr0)!g
      s/<dt>/$(tput bold)/g;
      s:</dt>:$(tput sgr0):g;
      s/<dd>/\n/g;
      s:</dd>::g;
      s:<script>.*</script>::g;
      s/<b>/$(tput bold)/gI;
      s:</b>:$(tput sgr0):gI;
      s:<br\s*/\?>:\n:gI;
      s:<i>:$(tput sitm):gI;
      s:</i>:$(tput sgr0):gI;
      s:<u>:$(tput smul):gI;
      s:</u>:$(tput sgr0):gI;
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
      s:<IMG src=\"8DAD7054.bmp\"[^>]*>:n̩:g
      s!</\?TABLE>!!gI
      s!</\?TR[^>]*>!!gI
      s!</\?TD>!!gI
      s!</\?FONT[^>]*>!!gI
      s!</\?A[^>]*>!!gI
      s!<SPAN class=\"bsptext\">\([^<>]*\)</SPAN>!$(tput setaf 245)\1$(tput sgr0)!g
      s! +! !g;
      s!<div part=\"[^\"]*\">!\n\n&!g
      s!<sense n=\"\([^\"]*\)\"!\n$(tput setaf 5)\1.$(tput sgr0) &!g;
      s!</\?sense[^>]*>!!g
      s!</\?div[^>]*>!!g
      s!<span lang=\"gr\">!!g # unbalanced in Frisk
      s!^\s*[0-9])!$(tput setaf 5)&$(tput sgr0)!g
      s!</\?span[^>]*>!!gI
      s!</\?p[^>]*>!!gI
    " | less -FR
  '';
in
symlinkJoin {
  name = "sdcv-dictionaries";
  paths = lib.mapAttrsToList makeStardict {
    lsj = dictionaries.classics;
    sa = dictionaries.sanskrit;
    oed = dictionaries.oed;
    sd-russian = dictionaries.russian;
    sd = dictionaries.englishGerman;
    jbo = dictionaries.lojban;
    cop = dictionaries.coptic;
    sd-turkish = dictionaries.turkish;
    wikt = dictionaries.wiktionary;
  };
}
/*
  https://github.com/latin-dict/Georges1910/releases/download/v1.0/Georges1910-stardict.zip
  https://github.com/nikita-moor/latin-dictionary/releases/download/2020-02-14/LiddellScott1940-stardict.zip
  http://download.huzheng.org/bigdict/stardict-Cambridge_Dictionary_of_American_Idioms-2.4.2.tar.bz2
  http://download.huzheng.org/bigdict/stardict-Concise_Oxford_Thesaurus_2nd_Ed-2.4.2.tar.bz2
  http://download.huzheng.org/bigdict/stardict-Urban_Dictionary_P1-2.4.2.tar.bz2
  http://download.huzheng.org/bigdict/stardict-Urban_Dictionary_P2-2.4.2.tar.bz2

  Duden_Rechtschreibung = pkgs.fetchzip {
    url = "http://download.huzheng.org/babylon/german/stardict-Duden_Rechtschreibung-2.4.2.tar.bz2";
    sha256 = "0xiprb45s88w62rn8rlbjrsagbiliay9hszsiy20glwabf6zsfji";
  };
  Duden = pkgs.fetchzip {
    url = "http://download.huzheng.org/de/stardict-duden-2.4.2.tar.bz2";
    sha256 = "049i4ynfqqxykv1nlkyks94mvn14s22qdax5gg7hx1ks5y4xw64j";
  };
  FreeOnlineDictionaryOfComputing = pkgs.fetchzip {
    url = "http://download.huzheng.org/dict.org/stardict-dictd_www.dict.org_foldoc-2.4.2.tar.bz2";
    sha256 = "1lw2i8dzxpx929cpgvv0x366dnh4drr10wzqmrhcd0kvwglqawgm";
  };
  Cappeller = pkgs.fetchzip {
    url = "${repo}/sa-head/german-entries/tars/capeller-sanskrit-german__2021-10-05_14-23-18Z__1MB.tar.gz";
    sha256 = "0jwrj2aih2lrcjg0lqm8jrvq9vsas9s8j4c9ggbg2n0jyz03kci3";
    stripRoot = false;
  };
  Yates = pkgs.fetchzip {
    url = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f/sa-head/en-entries/tars/yates__2021-10-05_14-23-18Z__2MB.tar.gz";
    sha256 = "1k7gbalysf48pwa06zfykrqhdk466g35xy64b30k4z8bybgdn8z2";
    stripRoot = false;
  };
  Wilson = pkgs.fetchzip {
    url = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f/sa-head/en-entries/tars/wilson__2021-10-05_14-23-18Z__3MB.tar.gz";
    sha256 = "0r5z1xif56zlw9r2jp3fvwmcjv4f2fhd9r17j30nah9awx2m1isg";
    stripRoot = false;
  };
  SpokenSanskrit = pkgs.fetchzip {
    url = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f/sa-head/en-entries/tars/spokensanskrit__2019-01-12_05-13-52Z__12MB.tar.gz";
    sha256 = "0x8j657mawvdcyd1knzvf33yp15z77d661n3h6g9hcj7wn9s5xyk";
    stripRoot = false;
  };
  Grassmann = pkgs.fetchzip {
    url = "${repo}/sa-head/german-entries/tars/grassman-sanskrit-german__2021-10-05_14-23-18Z__2MB.tar.gz";
    sha256 = "0jalsykaxkl6wzrky72lz8g3jdz26lmjpyibbfaf7a5vvnr55k02";
    stripRoot = false;
  };
  Benfey = pkgs.fetchzip {
    url = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f/sa-head/en-entries/tars/benfey__2021-10-05_14-23-18Z__2MB.tar.gz";
    sha256 = "0lj3hgphqgnihn482g9kgjwbvdrcd38vc29v1fi36srn08qdhvcb";
    stripRoot = false;
  };
  ApteSa = pkgs.fetchzip {
    url = "${repo}/sa-head/en-entries/tars/apte-sa__2021-12-18_13-20-56Z__6MB.tar.gz";
    sha256 = "0cq1dd02d1pvmjnibbs2cscifjnk2z0nqccf5yzzilxkzsrarh32";
    stripRoot = false;
  };
  MacDonell = pkgs.fetchzip {
    url = "https://github.com/indic-dict/stardict-sanskrit/raw/4ebd2d3db5820f7cbe3a649c3d5aa8f83d19b29f/sa-head/en-entries/tars/macdonell__2021-10-05_14-23-18Z__2MB.tar.gz";
    sha256 = "1yzmj0393mxvjv4n2lnvd2c722v2bmxxiyq7pscdwni3bxip3h8s";
    stripRoot = false;
  };
*/
