let
  di-fm-key = builtins.readFile <secrets/di.fm/key>;

  soma-fm = name: ''
    #EXTINF:-1,soma.fm ${name}
    http://ice1.somafm.com/${name}-128-aac
  '';
  di-fm = name: ''
    #EXTINF:-1,di.fm ${name}
    http://prem2.di.fm/${name}_hi?${di-fm-key}
  '';
  big-fm = name: ''
    #EXTINF:-1,big.fm ${name}
    https://streams.bigfm.de/bigfm-${name}-128-aac?usid=0-0-H-A-D-01
  '';
  named = name: link: ''
    #EXTINF:-1,${name}
    ${link}
  '';

  extm3u = links: ''
    #EXTM3U
    ${builtins.concatStringsSep "\n" links}
  '';

in
{
  SomaFM = extm3u [
    (soma-fm "specials") # For Halloween: Dark industrial/ambient music for tortured souls.
    (soma-fm "deepspaceone") # Deep ambient electronic, experimental and space music. For inner and outer space exploration.
    (soma-fm "beatblender") # A late night blend of deep-house and downtempo chill.
    (soma-fm "defcon") # Music for Hacking. The DEF CON Year-Round Channel.
    (soma-fm "sonicuniverse") # Transcending the world of jazz with eclectic, avant-garde takes on tradition.
    (soma-fm "reggae") # NEW! Reggae, Ska, Rocksteady classic and deep tracks.
    (soma-fm "7soul") # Vintage soul tracks from the original 45 RPM vinyl.
    (soma-fm "seventies") # Mellow album rock from the Seventies. Yacht not required.
    (soma-fm "u80s") # Early 80s UK Synthpop and a bit of New Wave.
    (soma-fm "secretagent") # The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!
    (soma-fm "thistle") # Exploring music from Celtic roots and branches
    (soma-fm "fluid") # Drown in the electronic sound of instrumental hiphop, future soul and liquid trap.
    (soma-fm "poptron") # Electropop and indie dance rock with sparkle and pop.
    (soma-fm "illstreet") # Classic bachelor pad, playful exotica and vintage music of tomorrow.
    (soma-fm "bagel") # What alternative rock radio should sound like. [explicit]
    (soma-fm "indiepop") # New and classic favorite indie pop tracks.
    (soma-fm "digitalis") # Digitally affected analog rock to calm the agitated heart.
    (soma-fm "folkfwd") # Indie Folk, Alt-folk and the occasional folk classics.
    (soma-fm "cliqhop") # Blips'n'beeps backed mostly w/beats. Intelligent Dance Music.
    (soma-fm "dubstep") # Dubstep, Dub and Deep Bass. May damage speakers at high volume.
    (soma-fm "suburbsofgoa") # Desi-influenced Asian world beats and beyond.
    (soma-fm "sf1033") # Ambient music mixed with the sounds of San Francisco public safety radio traffic.
    (soma-fm "missioncontrol") # Celebrating NASA and Space Explorers everywhere.
    (soma-fm "scanner") # San Francisco Public Safety Scanner Feed
    (soma-fm "metal") # From black to doom, prog to sludge, thrash to post, stoner to crossover, punk to industrial.
    (soma-fm "covers") # Just covers. Songs you know by artists you don't. We've got you covered.
    (soma-fm "brfm") # From the Playa to the world, for the annual Burning Man festival.
    (soma-fm "live") # Special Live Events and rebroadcasts of past live events
    (soma-fm "xmasinfrisko") # SomaFM's wacky and eclectic holiday mix. Not for the easily offended.
    (soma-fm "christmas") # Chilled holiday grooves and classic winter lounge tracks. (Kid and Parent safe!)
    (soma-fm "xmasrocks") # Have your self an indie/alternative holiday season!
    (soma-fm "jollysoul") # Where we cut right to the soul of the season.
    (soma-fm "vaporwaves") # All Vaporwave. All the time.
    (soma-fm "dronezone") # Served best chilled, safe with most medications. Atmospheric textures with minimal beats.
    (soma-fm "groovesalad") # A nicely chilled plate of ambient/downtempo beats and grooves.
    (soma-fm "gsclassic") # The classic (early 2000s) version of a nicely chilled plate of ambient/downtempo beats and grooves.
    (soma-fm "bootliquor") # Americana Roots music for Cowhands, Cowpokes and Cowtippers
    (soma-fm "lush") # Sensuous and mellow vocals, mostly female, with an electronic influence.
    (soma-fm "silent")
    (soma-fm "spacestation") # Tune in, turn on, space out. Spaced-out ambient and mid-tempo electronica.
    (soma-fm "thetrip") # Progressive house / trance. Tip top tunes.
  ];
  "DI.FM" = extm3u [
    (di-fm "00sclubhits")
    (di-fm "ambient")
    (di-fm "atmosphericbreaks")
    (di-fm "bassline") # Blending together elements of house music, speed garage, and techno – it’s all about the low end frequencies.
    (di-fm "bassnjackinhouse") # From the funkiest grooves to the dirtiest beats. Hard-hitting, high energy 4/4 club cuts to move the masses.
    (di-fm "bigbeat") # Heavily focused on breakbeats and dusty samples. A defining 90s musical movement still going strong today.
    (di-fm "bigroomhouse") # Fusing together house elements from the past and the present - prime time music full of uplifting high energy.
    (di-fm "breaks") # Inspired by hip hop and UK rave music, breaks features broken up drum loops and creative samples, synths and fx.
    (di-fm "chilledm")
    (di-fm "chillhop")
    (di-fm "chillntropicalhouse")
    (di-fm "chillout")
    (di-fm "chilloutdreams")
    (di-fm "chillstep")
    (di-fm "classicelectronica")
    (di-fm "classiceurodance")
    (di-fm "classiceurodisco") # Conceived in the European discos in the 70s, evolving through the decades into modern electronic masterpieces.
    (di-fm "classictrance")
    (di-fm "classicvocaltrance")
    (di-fm "club")
    (di-fm "clubdubstep")
    (di-fm "darkdnb") # Evil, gritty and twisted DnB / Drum & Bass. at 160+ BPM, hear the darkest basslines and the hardest hitting percussion.
    (di-fm "darkpsytrance") # The darker form of PsyTrance, which is a sound all its own – direct from Goa to your headphones.
    (di-fm "deephouse")
    (di-fm "deepnudisco")
    (di-fm "deepprogressivehouse")
    (di-fm "deeptech")
    (di-fm "detroithousentechno") # Where would dance music be without Detroit? The city that started it all continues to inspire and educate.
    (di-fm "discohouse")
    (di-fm "djmixes")
    (di-fm "downtempolounge")
    (di-fm "drumandbass")
    (di-fm "drumstep")
    (di-fm "drumstep") # A hybrid of half-time Dubstep and intense Drum and Bass / DnB.
    (di-fm "dub") # An emphasis on the bass and drums / DnB, delayed effects, sampled vocals and smokey Reggae inspired vibes.
    (di-fm "dubstep")
    (di-fm "dubtechno") # The beloved sounds of deep techno saturated with tape delays, heavy reverb and ice cold atmospherics.
    (di-fm "edm")
    (di-fm "edmfestival")
    (di-fm "electrohouse")
    (di-fm "electrohouse")
    (di-fm "electronicpioneers") # The trailblazers, the renegades and the experimental musicians who gave early inspiration with electronic instruments.
    (di-fm "electropop") # Catchy pop music blended together with vintage synthesizers and electronic instrumentation.
    (di-fm "electroswing") # The combination of 1920s-1940s jazz and swing music, big band horns and modern day electro house.
    (di-fm "epictrance")
    (di-fm "eurodance")
    (di-fm "funkyhouse")
    (di-fm "futurebass") # Hard basslines, booming beats and insatiable grooves. Inspired by Trap, Juke and Garage - molded together into a unique booming style.
    (di-fm "futuregarage") # 2step Garage rhythms, chunky bass line driven grooves and plenty of forward thinking innovation.
    (di-fm "futuresynthpop") # Finest selection of futurepop and synthpop.
    (di-fm "gabber") # The hardest form of techno with punishing tracks designed to drive the crowds into a sweaty frenzy.
    (di-fm "glitchhop") # The sound of digital malfunctions, electric hum and bit rate distortions perfectly placed alongside laid-back hip hop beats.
    (di-fm "goapsy")
    (di-fm "handsup") # A channel showcasing everything from hard dance, trance and happy hardcore to lift the spirits (and the arms).
    (di-fm "hardcore") # Strictly for the hardcore. These are the biggest and boldest bangers, and the hardest hitting tracks.
    (di-fm "harddance") # Concrete kicks and punching rhythms, hard dance is a tougher side of music with sharp edges and aggressive power.
    (di-fm "hardstyle") # Hard techno & hardcore. A global phenomenon with powerful kicks, distorted effects and infectious melodies.
    (di-fm "hardtechno")
    (di-fm "hardtechno") # Tough as nails warehouse jams full of cold aggression, sinister structures and pounding rhythms that hit hard.
    (di-fm "house")
    (di-fm "indiebeats") # Smooth, groovy and full of cutting-edge, fresh ideas - beats to kick back and enjoy far from the club setting.
    (di-fm "indiedance")
    (di-fm "jazzhouse") # One of the biggest cultural soundtracks with the infectious thump of house music. Expect sultry saxophones, trumpets, and finger snapping grooves.
    (di-fm "jungle") # Jungle keeps the breakbeat tempos high and celebrates the diverse ideas found within urban and rave music.
    (di-fm "latinhouse") # The sounds of Salsa, Brazilian beats and Latin Jazz with the steady grooves of modern East Coast dance music.
    (di-fm "liquiddnb")
    (di-fm "liquiddubstep")
    (di-fm "liquidtrap") # The smoother side of Trap but still packed with mechanical grooves and hip hop moods.
    (di-fm "lofihiphop")
    (di-fm "lofiloungenchill")
    (di-fm "lounge")
    (di-fm "melodicprogressive")
    (di-fm "minimal")
    (di-fm "nightcore") # Pitched up vocals, happy hardcore beats, and high energy music non-stop.
    (di-fm "nudisco") # Modern disco music blending the familiar funk of the 70s and 80s with futuristic beats and up to date grooves.
    (di-fm "oldschoolacid") # Acid, one of the characteristics of the TB-303, is celebrated here with the best tracks from house, techno and trance.
    (di-fm "oldschoolhouse") # The biggest classics and secret weapons – this is a true treasure chest of house tracks from back in the day.
    (di-fm "oldschoolrave") # Grab your whistles, white gloves and reach for the laser beams. This is the sound of raving when raving was new.
    (di-fm "progressive")
    (di-fm "progressivepsy")
    (di-fm "psybient") # The psychedelic side of ambient.
    (di-fm "psychill")
    (di-fm "psydub")
    (di-fm "russianclubhits")
    (di-fm "soulfulhouse") # House music saturated with feeling – full of melodies, vocals and true soul. Steady warm 4/4 vibes.
    (di-fm "spacemusic") # Ambient space music for expanding minds.
    (di-fm "summerchillhouse")
    (di-fm "synthwave")
    (di-fm "techhouse")
    (di-fm "techno")
    (di-fm "trance")
    (di-fm "trap")
    (di-fm "tribalhouse") # The percussive side of the house and tech house scene, tribal house takes drums and puts them in the forefront.
    (di-fm "umfradio")
    (di-fm "undergroundtechno") # From gritty Berlin streets to dark corners of Brooklyn, this is techno made by artists pushing the genre further.
    (di-fm "vocalchillout")
    (di-fm "vocalhouse")
    (di-fm "vocallounge")
    (di-fm "vocaltrance")
  ];
  Misc = extm3u [
    (named "lassulus' radio" "https://radio.lassul.us/radio.ogg")
  ];
  BigFM = extm3u [
    (big-fm "deutschland")
    (big-fm "charts")
    (big-fm "hiphop")
    (big-fm "deutschrap")
    (big-fm "usrap")
    (big-fm "oldschool")
    (big-fm "dance")
    (big-fm "mashup")
    (big-fm "sunsetlounge")
    (big-fm "reggaevibes")
    (big-fm "latinbeats")
    (big-fm "groovenight")
    (big-fm "rapfeature")
    (big-fm "urbanclubbeats")
    (big-fm "oldschooldeutsch")
    (big-fm "nitroxedm")
    (big-fm "nitroxdeep")
    (big-fm "worldbeats")
    (big-fm "turkey")
    (big-fm "balkan")
    (big-fm "orient")
    (big-fm "russia")
  ];
}
