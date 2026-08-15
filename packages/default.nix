# Single registry of local packages.
#
# The default overlay (overlays/packages.nix) merges this map into pkgs, and
# the flake's `packages` output exports exactly these names — add a package
# here and it is registered everywhere.
pkgs: {
  # packaged from .bin/
  two56color = pkgs.callPackage ./256color.nix { };
  avesta = pkgs.callPackage ./avesta.nix { };
  bvg = pkgs.callPackage ./bvg.nix { };
  charinfo = pkgs.callPackage ./charinfo.nix { };
  chunk-pdf = pkgs.callPackage ./chunk-pdf.nix { };
  csv2json = pkgs.callPackage ./csv2json.nix { };
  fix-sd = pkgs.callPackage ./fix-sd.nix { };
  json2csv = pkgs.callPackage ./json2csv.nix { };
  mp3player-write = pkgs.callPackage ./mp3player-write.nix { };
  mushakkil = pkgs.callPackage ./mushakkil.nix { };
  nix-haddock-index = pkgs.callPackage ./nix-haddock-index.nix { };
  pdf-ocr = pkgs.callPackage ./pdf-ocr.nix { };
  prospekte = pkgs.callPackage ./prospekte.nix { };
  readme = pkgs.callPackage ./readme.nix { };

  auc = pkgs.callPackage ./auc.nix { };
  cheat-sh = pkgs.callPackage ./cheat-sh.nix { };
  brassica = pkgs.callPackage ./brassica.nix { }; # TODO upstream
  dawn-editor = pkgs.callPackage ./dawn.nix { };
  text2pdf = pkgs.callPackage ./text2pdf.nix { }; # TODO upstream
  wttr = pkgs.callPackage ./wttr.nix { }; # TODO upstream
  jsesh = pkgs.callPackage ./jsesh.nix { }; # TODO upstream
  opustags = pkgs.callPackage ./opustags.nix { }; # TODO upstream
  trans = pkgs.callPackage ./trans.nix { }; # TODO upstream
  stag = pkgs.callPackage ./stag.nix { }; # TODO upstream
  morris = pkgs.callPackage ./morris.nix { };
  cro = pkgs.callPackage ./cro.nix { };
  exodus = pkgs.callPackage ./exodus.nix { };
  picoclaw = pkgs.callPackage ./picoclaw.nix { };

  # krebs
  brainmelter = pkgs.callPackage ./brainmelter.nix { };
  cyberlocker-tools = pkgs.callPackage ./cyberlocker-tools.nix { };
  hc = pkgs.callPackage ./hc.nix { };
  pls = pkgs.callPackage ./pls.nix { };
  radio-news = pkgs.callPackage ./radio-news { };
  untilport = pkgs.callPackage ./untilport.nix { };
  weechat-declarative = pkgs.callPackage ./weechat-declarative.nix { };

  # my packages
  betacode = pkgs.callPackage ./betacode.nix { };
  bring-out-the-gimp = pkgs.callPackage ./gimp.nix { };
  closest = pkgs.callPackage ./closest { };
  default-gateway = pkgs.callPackage ./default-gateway.nix { };
  depp = pkgs.callPackage ./depp.nix { };
  radioStreams = pkgs.callPackage ./streams { };
  devour = pkgs.callPackage ./devour.nix { };
  dmenu-randr = pkgs.callPackage ./dmenu-randr.nix { };
  emailmenu = pkgs.callPackage ./emailmenu.nix { };
  fkill = pkgs.callPackage ./fkill.nix { };
  fzfmenu = pkgs.callPackage ./fzfmenu.nix { };
  gfs-fonts = pkgs.callPackage ./gfs-fonts.nix { };
  heuretes = pkgs.callPackage ./heuretes.nix { };
  image-convert-favicon = pkgs.callPackage ./image-convert-favicon.nix { };
  image-convert-tolino = pkgs.callPackage ./image-convert-tolino.nix { };
  ipa = pkgs.writers.writePython3Bin "ipa" { flakeIgnore = [ "E501" ]; } ./ipa.py;
  kirciuoklis = pkgs.callPackage ./kirciuoklis.nix { };
  kpaste = pkgs.callPackage ./kpaste.nix { };
  literature-quote = pkgs.callPackage ./literature-quote.nix { };
  man-pdf = pkgs.callPackage ./man-pdf.nix { };
  mansplain = pkgs.callPackage ./mansplain.nix { };
  manual-sort = pkgs.callPackage ./manual-sort.nix { };
  mpv-iptv = pkgs.callPackage ./mpv-iptv.nix { };
  mpv-radio = pkgs.callPackage ./mpv-radio.nix { di-fm-key-file = "/dev/null"; };
  mpv-tuner = pkgs.callPackage ./mpv-tuner.nix { di-fm-key-file = "/dev/null"; };
  mpv-tv = pkgs.callPackage ./mpv-tv.nix { };
  new-mac = pkgs.callPackage ./new-mac.nix { };
  nix-git = pkgs.callPackage ./nix-git.nix { };
  noise-waves = pkgs.callPackage ./noise-waves.nix { };
  notemenu = pkgs.callPackage ./notemenu.nix { };
  obsidian-vim = pkgs.callPackage ./obsidian-vim.nix { };
  vim-typewriter = pkgs.callPackage ./vim-typewriter.nix { };
  vimacs = pkgs.callPackage ./vimacs.nix { };
  vim-email = pkgs.callPackage ./vim-email.nix { };
  polyglot = pkgs.callPackage ./polyglot.nix { };
  q = pkgs.callPackage ./q.nix { };
  qrpaste = pkgs.callPackage ./qrpaste.nix { };
  random-zeno = pkgs.callPackage ./random-zeno.nix { };
  scanned = pkgs.callPackage ./scanned.nix { };
  stardict-tools = pkgs.callPackage ./stardict-tools.nix { };
  swallow = pkgs.callPackage ./swallow.nix { };
  tocharian-font = pkgs.callPackage ./tocharian-font.nix { };
  ttspaste = pkgs.callPackage ./ttspaste.nix { };
  niveum-ssh = pkgs.callPackage ./niveum-ssh.nix { };
  try-connect = pkgs.callPackage ./try-connect.nix { };
  unicodmenu = pkgs.callPackage ./unicodmenu.nix { };
  vg = pkgs.callPackage ./vg.nix { };
  klem = pkgs.callPackage ./klem { };
  radiorec = pkgs.callPackage ./radiorec.nix { };
  yt-dlp-master = pkgs.callPackage ./yt-dlp-master.nix { };
}
