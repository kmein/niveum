{ config, pkgs, lib, ... }:
let
  theme = import <dot/theme.nix>;
  unstable = import <nixos-unstable> {};
  scripts.dic = pkgs.callPackage (pkgs.fetchurl {
    url = "https://cgit.krebsco.de/stockholm/plain/krebs/5pkgs/simple/dic/default.nix?id=8371e21c10bdb5d5353cc581efba7e09e4ce7a91";
    sha256 = "1vd8mg1ac7wzrcs5bl20srkxcs65zr7rd7y3wxzrxspij5wrb23i";
  }) {};
  scripts.yt-next = pkgs.callPackage (pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/yt-next/default.nix;
    sha256 = "0j9r9xy34sl9ci5lz38060b3nakf0vd7gw46pykdiriwz6znbxn3";
  }) {};
  scripts.acronym = pkgs.callPackage (pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/acronym/default.nix;
    sha256 = "1rpr1rniz74vmkl4r3hgrg8q7ncxrvbf7zp0lq9b7lva85i12zx9";
  }) {};
  scripts.urban = pkgs.callPackage (pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/urban/default.nix;
    sha256 = "128v0znnapcqbyvc0nf112ddfyipr8sc1z4kcnggnbjf99i763ji";
  }) {};
  scripts.mpv-poll = pkgs.callPackage (pkgs.fetchurl {
    url = http://cgit.lassul.us/stockholm/plain/lass/5pkgs/mpv-poll/default.nix;
    sha256 = "0ccmm7spxll98j8gy58fc3p8331arznshsj5wn4kkcypcs16n6ci";
  }) {};
  scripts.instaget = unstable.writers.writeDashBin "instaget" ''
    for url in "$@"; do
      ${pkgs.curl}/bin/curl -s "$url" \
        | grep display_url \
        | grep -o '{.*}' \
        | ${pkgs.jq}/bin/jq -r '
            .entry_data.PostPage
            | .[].graphql.shortcode_media.edge_sidecar_to_children.edges
            | .[].node.display_url' \
        | xargs ${pkgs.wget}/bin/wget
    done
  '';
  bingWallpaper = unstable.writers.writeBash "bing-wallpaper.sh" ''
    PICTURE_DIR="$HOME/pictures/external/bing/"

    mkdir -p $PICTURE_DIR

    urls=( $(${pkgs.curl}/bin/curl -s http://www.bing.com \
        | grep -Eo "url:'.*?'" \
        | sed -e "s/url:'\([^']*\)'.*/http:\/\/bing.com\1/" \
        | sed -e "s/\\\//g") )

    for p in ''${urls[@]}; do
        filename=$(echo $p | sed -e "s/.*\/\(.*\)/\1/")
        if [ ! -f $PICTURE_DIR/$filename ]; then
            ${pkgs.curl}/bin/curl -Lo "$PICTURE_DIR/$filename" $p
        fi
    done
  '';
  scripts.pull-all = unstable.writers.writeDashBin "pull-all" ''
    CUR_DIR=$(pwd)
    echo -e "\n\033[1mPulling in latest changes for all repositories...\033[0m\n"
    for i in $(find . -name ".git" | cut -c 3-); do
      echo ""
      echo -e "\033[33m"+$i+"\033[0m"
      cd "$i"
      cd ..
      git pull origin master
      cd $CUR_DIR
    done
    echo -e "\n\033[32mComplete!\033[0m\n"
  '';
  scripts.genius = unstable.writers.writeDashBin "genius" ''
    test $# -eq 2 || (
      echo "usage: $0 <artist> <song>"
      exit 1
    )

    normalize() {
      tr -d -c '0-9A-Za-z ' | tr ' ' - | tr '[:upper:]' '[:lower:]'
    }

    ARTIST=$(echo "$1" | normalize | ${pkgs.gnused}/bin/sed 's/./\U&/')
    TITLE=$(echo "$2" | normalize)
    GENIUS_URL="https://genius.com/$ARTIST-$TITLE-lyrics"

    ${pkgs.curl}/bin/curl -s "$GENIUS_URL" \
      | ${pkgs.gnused}/bin/sed -ne '/class="lyrics"/,/<\/p>/p' \
      | ${pkgs.pandoc}/bin/pandoc -f html -s -t plain \
      | ${pkgs.gnused}/bin/sed 's/^_/\x1b[3m/g;s/_$/\x1b[0m/g;s/^\[/\n\x1b\[1m\[/g;s/\]$/\]\x1b[0m/g'

    printf "\n(source: $GENIUS_URL)\n" >/dev/stderr
  '';
  scripts.generate-shell-nix =
    let repository = pkgs.fetchFromGitHub {
        owner = "kmein";
        repo = "generate-shell-nix";
        rev = "1e6aed53701b5276d065f0dc70ba5479d2c6d041";
        sha256 = "1532w34p46mjqnm2bx72f98gacgb3ncd00rflxr54pq94klh26is";
      };
    in pkgs.writeScriptBin "generate-shell-nix" (builtins.readFile "${repository.out}/generate-shell-nix");
  scripts.font-size = unstable.writers.writeDashBin "font-size" ''
    set -efu

    # set_font NORMAL_FONT BOLD_FONT
    set_font() {
      printf '\033]710;%s\007' "$1"
      printf '\033]711;%s\007' "$2"
    }

    case ''${1-} in
      '''|0|--reset)
        set_font \
            "xft:${theme.terminalFont.name}:size=${toString theme.terminalFont.size}" \
            "xft:${theme.terminalFont.name}:size=${toString theme.terminalFont.size}:bold" \
        ;;
      [2-9]|[1-9][0-9]|[1-9][0-9][0-9])
        set_font \
            "xft:${theme.terminalFont.name}:size=$1" \
            "xft:${theme.terminalFont.name}:size=$1:bold" \
        ;;
      *)
        echo "$0: bad argument: $1" >&2
        exit 1
    esac
  '';
  scripts.wttr = unstable.writers.writeDashBin "wttr" ''
    ${pkgs.curl}/bin/curl -s -H "Accept-Language: ''${LANG%_*}" --compressed "wttr.in/''${1-"@$(${pkgs.curl}/bin/curl -s ipinfo.io | ${pkgs.jq}/bin/jq -r .ip)"}?0"
  '';
  scripts.nix-git = unstable.writers.writeDashBin "nix-git" ''
    ${pkgs.nix-prefetch-git}/bin/nix-prefetch-git "$@" 2> /dev/null | ${pkgs.jq}/bin/jq -r '"rev = \"\(.rev)\";\nsha256 = \"\(.sha256)\";"'
  '';
  scripts.autorenkalender = unstable.writers.writeDashBin "autorenkalender" ''
    ${pkgs.curl}/bin/curl -s https://gutenberg.spiegel.de | ${pkgs.gnused}/bin/sed -n '/Autorenkalender/,/<\/div>/p' | ${pkgs.html2text}/bin/html2text | ${pkgs.coreutils}/bin/tail +2
  '';
  scripts.n = unstable.writers.writeDashBin "n" ''
    nix-shell -p "''${2:-$1}" --run "$1"
  '';
in {
  environment.shellAliases =
    let rlwrap = cmd: "${pkgs.rlwrap}/bin/rlwrap ${cmd}";
    in {
      o = "${pkgs.xdg_utils}/bin/xdg-open";
      clipboard = "${pkgs.xclip}/bin/xclip -se c";
      ip = "${pkgs.iproute}/bin/ip -c";
      ocaml = rlwrap "${pkgs.ocaml}/bin/ocaml";
      tmux = "${pkgs.tmux}/bin/tmux -2";
    };

  environment.systemPackages = lib.attrsets.attrValues scripts;
}
