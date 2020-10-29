{
  SomaFM = map (name: "http://ice1.somafm.com/${name}-128-aac") [
    "specials" # For Halloween: Dark industrial/ambient music for tortured souls.
    "deepspaceone" # Deep ambient electronic, experimental and space music. For inner and outer space exploration.
    "beatblender" # A late night blend of deep-house and downtempo chill.
    "defcon" # Music for Hacking. The DEF CON Year-Round Channel.
    "sonicuniverse" # Transcending the world of jazz with eclectic, avant-garde takes on tradition.
    "reggae" # NEW! Reggae, Ska, Rocksteady classic and deep tracks.
    "7soul" # Vintage soul tracks from the original 45 RPM vinyl.
    "seventies" # Mellow album rock from the Seventies. Yacht not required.
    "u80s" # Early 80s UK Synthpop and a bit of New Wave.
    "secretagent" # The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!
    "thistle" # Exploring music from Celtic roots and branches
    "fluid" # Drown in the electronic sound of instrumental hiphop, future soul and liquid trap.
    "poptron" # Electropop and indie dance rock with sparkle and pop.
    "illstreet" # Classic bachelor pad, playful exotica and vintage music of tomorrow.
    "bagel" # What alternative rock radio should sound like. [explicit]
    "indiepop" # New and classic favorite indie pop tracks.
    "digitalis" # Digitally affected analog rock to calm the agitated heart.
    "folkfwd" # Indie Folk, Alt-folk and the occasional folk classics.
    "cliqhop" # Blips'n'beeps backed mostly w/beats. Intelligent Dance Music.
    "dubstep" # Dubstep, Dub and Deep Bass. May damage speakers at high volume.
    "suburbsofgoa" # Desi-influenced Asian world beats and beyond.
    "sf1033" # Ambient music mixed with the sounds of San Francisco public safety radio traffic.
    "missioncontrol" # Celebrating NASA and Space Explorers everywhere.
    "scanner" # San Francisco Public Safety Scanner Feed
    "metal" # From black to doom, prog to sludge, thrash to post, stoner to crossover, punk to industrial.
    "covers" # Just covers. Songs you know by artists you don't. We've got you covered.
    "brfm" # From the Playa to the world, for the annual Burning Man festival.
    "live" # Special Live Events and rebroadcasts of past live events
    "xmasinfrisko" # SomaFM's wacky and eclectic holiday mix. Not for the easily offended.
    "christmas" # Chilled holiday grooves and classic winter lounge tracks. (Kid and Parent safe!)
    "xmasrocks" # Have your self an indie/alternative holiday season!
    "jollysoul" # Where we cut right to the soul of the season.
    "vaporwaves" # All Vaporwave. All the time.
    "dronezone" # Served best chilled, safe with most medications. Atmospheric textures with minimal beats.
    "groovesalad" # A nicely chilled plate of ambient/downtempo beats and grooves.
    "gsclassic" # The classic (early 2000s) version of a nicely chilled plate of ambient/downtempo beats and grooves.
    "bootliquor" # Americana Roots music for Cowhands, Cowpokes and Cowtippers
    "lush" # Sensuous and mellow vocals, mostly female, with an electronic influence.
    "silent"
    "spacestation" # Tune in, turn on, space out. Spaced-out ambient and mid-tempo electronica.
    "thetrip" # Progressive house / trance. Tip top tunes.
  ];
  "DI.FM" = map (genre: "http://prem2.di.fm:80/${genre}_hi?4527f2ba1755917") [
    "bassline"
    "breaks"
    "chillout"
    "classicelectronica"
    "deephouse"
    "djmixes"
    "drumandbass"
    "drumstep"
    "dub"
    "dubstep"
    "electrohouse"
    "electroswing"
    "glitchhop"
    "handsup"
    "hardtechno"
    "liquiddnb"
    "liquiddubstep"
    "lounge"
    "minimal"
    "progressive"
    "progressivepsy"
    "techhouse"
    "trap"

    "ambient"
    "chillhop"
    "chillntropicalhouse"
    "chilloutdreams"
    "chillstep"
    "eurodance"
    "techno"
    "vocaltrance"
    "spacemusic"
    # TODO extend from https://www.di.fm/channels
  ];
  Misc = [
    "https://radio.lassul.us/radio.ogg"
    "http://bytefm.cast.addradio.de/bytefm/main/mid/stream"
  ];
  BigFM = map (name: "https://streams.bigfm.de/bigfm-${name}-128-aac?usid=0-0-H-A-D-01") [
    "deutschland"
    "charts"
    "hiphop"
    "deutschrap"
    "usrap"
    "oldschool"
    "dance"
    "mashup"
    "sunsetlounge"
    "reggaevibes"
    "latinbeats"
    "groovenight"
    "rapfeature"
    "urbanclubbeats"
    "oldschooldeutsch"
    "nitroxedm"
    "nitroxdeep"
    "worldbeats"
    "turkey"
    "balkan"
    "orient"
    "russia"
  ];
}
