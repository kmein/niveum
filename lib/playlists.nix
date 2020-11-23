let
  di-fm-key = builtins.readFile <secrets/di.fm/key>;

  soma-fm = name: {
    url = "http://ice1.somafm.com/${name}-128-aac";
    provider = "soma.fm";
    inherit name;
  };
  di-fm = name: {
    url = "http://prem2.di.fm/${name}_hi?${di-fm-key}";
    provider = "di.fm";
    inherit name;
  };
  big-fm = name: {
    url = "https://streams.bigfm.de/bigfm-${name}-128-aac";
    provider = "big.fm";
    inherit name;
  };
  we-are-one = name: {
    url = "http://listen.${name}.fm/tunein-aac-hd-pls";
    provider = "WeAreOne";
    inherit name;
  };
  rte = name: {
    url = "https://www.rte.ie/manifests/${name}.m3u8";
    provider = "Raidió Teilifís Éireann";
    inherit name;
  };
  laut-fm = name: {
    url = "http://stream.laut.fm/${name}";
    provider = "laut.fm";
    inherit name;
  };
  rautemusik = name: {
    url = "http://${name}-high.rautemusik.fm/";
    provider = "rautemusik.fm";
    inherit name;
  };
  radiosai = name: port: {
    url = "http://stream.radiosai.net:${toString port}";
    provider = "Radio Sai";
    inherit name;
  };
in
{
  Chill = {
    description = "Kühlen.";
    tracks = [
      {
        url = "https://radio.lassul.us/radio.ogg";
        name = "Radio";
        provider = "lassulus";
      }
      {
        url = "https://streamer.radio.co/s2c3cc784b/listen";
        name = "Radio";
        provider = "electroswing-radio.com";
      }
      (rautemusik "study")
      (big-fm "reggaevibes")
      (big-fm "sunsetlounge")
      (di-fm "ambient")
      (di-fm "chilledm")
      (di-fm "chillhop")
      (di-fm "chillntropicalhouse")
      (di-fm "chillout")
      (di-fm "chilloutdreams")
      (di-fm "chillstep")
      (di-fm "deephouse")
      (di-fm "deepprogressivehouse")
      (di-fm "downtempolounge")
      (di-fm "dub") # An emphasis on the bass and drums / DnB, delayed effects, sampled vocals and smokey Reggae inspired vibes.
      (di-fm "indiebeats") # Smooth, groovy and full of cutting-edge, fresh ideas - beats to kick back and enjoy far from the club setting.
      (di-fm "liquidtrap") # The smoother side of Trap but still packed with mechanical grooves and hip hop moods.
      (di-fm "lofihiphop")
      (di-fm "lofiloungenchill")
      (di-fm "lounge")
      (di-fm "melodicprogressive")
      (di-fm "psybient") # The psychedelic side of ambient.
      (di-fm "psychill")
      (di-fm "psydub")
      (di-fm "spacemusic") # Ambient space music for expanding minds.
      (di-fm "trap")
      (di-fm "vocalchillout")
      (di-fm "vocallounge")
      (soma-fm "beatblender") # A late night blend of deep-house and downtempo chill.
      (soma-fm "deepspaceone") # Deep ambient electronic, experimental and space music. For inner and outer space exploration.
      (soma-fm "digitalis") # Digitally affected analog rock to calm the agitated heart.
      (soma-fm "dronezone") # Served best chilled, safe with most medications. Atmospheric textures with minimal beats.
      (soma-fm "fluid") # Drown in the electronic sound of instrumental hiphop, future soul and liquid trap.
      (soma-fm "indiepop") # New and classic favorite indie pop tracks.
      (soma-fm "lush") # Sensuous and mellow vocals, mostly female, with an electronic influence.
      (soma-fm "missioncontrol") # Celebrating NASA and Space Explorers everywhere.
      (soma-fm "reggae") # NEW! Reggae, Ska, Rocksteady classic and deep tracks.
      (soma-fm "sf1033") # Ambient music mixed with the sounds of San Francisco public safety radio traffic.
    ];
  };

  Brennpunkt = {
    description = "What focus means.";
    tracks = [ # What Focus Means
      (laut-fm "dnbzone")
      (di-fm "atmosphericbreaks")
      (di-fm "bigbeat") # Heavily focused on breakbeats and dusty samples. A defining 90s musical movement still going strong today.
      (di-fm "darkdnb") # Evil, gritty and twisted DnB / Drum & Bass. at 160+ BPM, hear the darkest basslines and the hardest hitting percussion.
      (di-fm "deeptech")
      (di-fm "drumandbass")
      (di-fm "drumstep") # A hybrid of half-time Dubstep and intense Drum and Bass / DnB.
      (di-fm "dubstep")
      (di-fm "dubtechno") # The beloved sounds of deep techno saturated with tape delays, heavy reverb and ice cold atmospherics.
      (di-fm "futuregarage") # 2step Garage rhythms, chunky bass line driven grooves and plenty of forward thinking innovation.
      (di-fm "jungle") # Jungle keeps the breakbeat tempos high and celebrates the diverse ideas found within urban and rave music.
      (di-fm "liquiddnb")
      (di-fm "liquiddubstep")
      (di-fm "minimal")
      (di-fm "oldschoolacid") # Acid, one of the characteristics of the TB-303, is celebrated here with the best tracks from house, techno and trance.
      (di-fm "progressive")
      (di-fm "techhouse")
      (di-fm "techno")
      (di-fm "umfradio")
      (soma-fm "defcon") # Music for Hacking. The DEF CON Year-Round Channel.
      (soma-fm "dubstep") # Dubstep, Dub and Deep Bass. May damage speakers at high volume.
      (soma-fm "groovesalad") # A nicely chilled plate of ambient/downtempo beats and grooves.
      (soma-fm "gsclassic") # The classic (early 2000s) version of a nicely chilled plate of ambient/downtempo beats and grooves.
      (soma-fm "secretagent") # The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!
    ];
  };

  Post-Musicality = {
    description = "Makes you wonder whether you are listening to music at all.";
    tracks = [
      (rautemusik "wackenradio")
      (di-fm "classicelectronica")
      (di-fm "darkpsytrance") # The darker form of PsyTrance, which is a sound all its own – direct from Goa to your headphones.
      (di-fm "gabber") # The hardest form of techno with punishing tracks designed to drive the crowds into a sweaty frenzy.
      (di-fm "goapsy")
      (di-fm "hardtechno") # Tough as nails warehouse jams full of cold aggression, sinister structures and pounding rhythms that hit hard.
      (di-fm "progressivepsy")
      (di-fm "undergroundtechno") # From gritty Berlin streets to dark corners of Brooklyn, this is techno made by artists pushing the genre further.
      (soma-fm "cliqhop") # Blips'n'beeps backed mostly w/beats. Intelligent Dance Music.
      (soma-fm "metal") # From black to doom, prog to sludge, thrash to post, stoner to crossover, punk to industrial.
      (we-are-one "coretime")
    ];
  };

  "Club Albrecht" = {
    description = "Party!";
    tracks = [
      (rautemusik "club")
      (rautemusik "house")
      (rautemusik "bass")
      (rautemusik "breakz")
      (laut-fm "electro-swing")
      (big-fm "dance")
      (big-fm "groovenight")
      (big-fm "nitroxdeep")
      (big-fm "nitroxedm")
      (big-fm "urbanclubbeats")
      (di-fm "00sclubhits")
      (di-fm "bassline") # Blending together elements of house music, speed garage, and techno – it’s all about the low end frequencies.
      (di-fm "bassnjackinhouse") # From the funkiest grooves to the dirtiest beats. Hard-hitting, high energy 4/4 club cuts to move the masses.
      (di-fm "bigroomhouse") # Fusing together house elements from the past and the present - prime time music full of uplifting high energy.
      (di-fm "classiceurodance")
      (di-fm "club")
      (di-fm "clubdubstep")
      (di-fm "deepnudisco")
      (di-fm "detroithousentechno") # Where would dance music be without Detroit? The city that started it all continues to inspire and educate.
      (di-fm "discohouse")
      (di-fm "djmixes")
      (di-fm "edm")
      (di-fm "edmfestival")
      (di-fm "electrohouse")
      (di-fm "electronicpioneers") # The trailblazers, the renegades and the experimental musicians who gave early inspiration with electronic instruments.
      (di-fm "electroswing") # The combination of 1920s-1940s jazz and swing music, big band horns and modern day electro house.
      (di-fm "eurodance")
      (di-fm "funkyhouse")
      (di-fm "futurebass") # Hard basslines, booming beats and insatiable grooves. Inspired by Trap, Juke and Garage - molded together into a unique booming style.
      (di-fm "futuresynthpop") # Finest selection of futurepop and synthpop.
      (di-fm "glitchhop") # The sound of digital malfunctions, electric hum and bit rate distortions perfectly placed alongside laid-back hip hop beats.
      (di-fm "handsup") # A channel showcasing everything from hard dance, trance and happy hardcore to lift the spirits (and the arms).
      (di-fm "hardcore") # Strictly for the hardcore. These are the biggest and boldest bangers, and the hardest hitting tracks.
      (di-fm "harddance") # Concrete kicks and punching rhythms, hard dance is a tougher side of music with sharp edges and aggressive power.
      (di-fm "hardstyle") # Hard techno & hardcore. A global phenomenon with powerful kicks, distorted effects and infectious melodies.
      (di-fm "house")
      (di-fm "indiedance")
      (di-fm "jazzhouse") # One of the biggest cultural soundtracks with the infectious thump of house music. Expect sultry saxophones, trumpets, and finger snapping grooves.
      (di-fm "latinhouse") # The sounds of Salsa, Brazilian beats and Latin Jazz with the steady grooves of modern East Coast dance music.
      (di-fm "nightcore") # Pitched up vocals, happy hardcore beats, and high energy music non-stop.
      (di-fm "nudisco") # Modern disco music blending the familiar funk of the 70s and 80s with futuristic beats and up to date grooves.
      (di-fm "oldschoolhouse") # The biggest classics and secret weapons – this is a true treasure chest of house tracks from back in the day.
      (di-fm "oldschoolrave") # Grab your whistles, white gloves and reach for the laser beams. This is the sound of raving when raving was new.
      (di-fm "soulfulhouse") # House music saturated with feeling – full of melodies, vocals and true soul. Steady warm 4/4 vibes.
      (di-fm "summerchillhouse")
      (di-fm "tribalhouse") # The percussive side of the house and tech house scene, tribal house takes drums and puts them in the forefront.
      (di-fm "vocalhouse")
      (soma-fm "poptron") # Electropop and indie dance rock with sparkle and pop.
      (soma-fm "spacestation") # Tune in, turn on, space out. Spaced-out ambient and mid-tempo electronica.
      (we-are-one "clubtime")
      (we-are-one "hardbase")
      (we-are-one "housetime")
      (we-are-one "teatime")
      (we-are-one "technobase")
    ];
  };

  HipHop = {
    description = "";
    tracks = [
      (rautemusik "deutschrap-charts")
      (rautemusik "deutschrap-classic")
      (big-fm "deutschrap")
      (big-fm "hiphop")
      (big-fm "oldschool")
      (big-fm "oldschooldeutsch")
      (big-fm "rapfeature")
      (big-fm "usrap")
      # (di-fm "breaks") # Inspired by hip hop and UK rave music, breaks features broken up drum loops and creative samples, synths and fx.
      (rte "pulse")
    ];
  };

  Wave = {
    description = "";
    tracks = [
      (di-fm "classiceurodisco") # Conceived in the European discos in the 70s, evolving through the decades into modern electronic masterpieces.
      (di-fm "electropop") # Catchy pop music blended together with vintage synthesizers and electronic instrumentation.
      (di-fm "synthwave")
      (soma-fm "seventies") # Mellow album rock from the Seventies. Yacht not required.
      (soma-fm "u80s") # Early 80s UK Synthpop and a bit of New Wave.
      (soma-fm "vaporwaves") # All Vaporwave. All the time.
    ];
  };

  Trance = {
    description = "";
    tracks = [
      (laut-fm "uplifting-trance-radio")
      (rautemusik "trance")
      (di-fm "classictrance")
      (di-fm "classicvocaltrance")
      (di-fm "epictrance")
      (di-fm "trance")
      (di-fm "vocaltrance")
      (soma-fm "thetrip") # Progressive house / trance. Tip top tunes.
      (we-are-one "trancebase")
    ];
  };

  i18n = {
    description = "Country and culture specific music.";
    tracks = [
      {
        url = "http://62.210.24.124:8379/;stream.mp3"; # Gjirokastër
        provider = "Alpomedia";
        name = "Gjirokastër";
      }
      {
        url = "http://iphone.live24.gr/derty1000"; # derti – μόνο λαϊκά
        name = "μόνο λαϊκά";
        provider = "Derti";
      }
      {
        url = "https://knr.gl/radiolivestream"; # kalaallit nunaata radioa
        provider = "KNR";
        name = "Kalaallit Nunaata Radioa";
      }
      {
        provider = "VahonFM";
        name = "Hindustani";
        url = "http://94.23.148.11:8058/";
      }
      (radiosai "Asia" 8002)
      (radiosai "Ameri" 8006)
      (radiosai "Bhajan" 8000)
      (big-fm "balkan")
      (big-fm "latinbeats")
      (big-fm "orient")
      (big-fm "russia")
      (big-fm "turkey")
      (big-fm "worldbeats")
      (di-fm "russianclubhits")
      (rautemusik "partyhits")
      (rautemusik "volksmusik")
      (rte "rnag") # Raidió na Gaeltachta
      (soma-fm "bootliquor") # Americana Roots music for Cowhands, Cowpokes and Cowtippers
      (soma-fm "suburbsofgoa") # Desi-influenced Asian world beats and beyond.
      (soma-fm "thistle") # Exploring music from Celtic roots and branches
    ];
  };

  Out-There = {
    description = "Music that is very out-there.";
    tracks = [
      {
        url = "http://klassikr.streamabc.net/klassikradio-simulcast-mp3-hq"; # Klassikradio
        name = "Klassikradio";
        provider = "Klassik Radio GmbH & Co. KG";
      }
      # "http://stream.klassikradio.de/live/mp3-192/stream.klassikradio.de"
      # (soma-fm "scanner") # San Francisco Public Safety Scanner Feed
      (rautemusik "rock")
      (rautemusik "12punks")
      (rte "2xm") # Alternative
      (rte "gold") # Oldies
      (rte "lyric") # "Classical and specialist music"
      (soma-fm "7soul") # Vintage soul tracks from the original 45 RPM vinyl.
      (soma-fm "bagel") # What alternative rock radio should sound like. [explicit]
      (soma-fm "brfm") # From the Playa to the world, for the annual Burning Man festival.
      (soma-fm "covers") # Just covers. Songs you know by artists you don't. We've got you covered.
      (soma-fm "folkfwd") # Indie Folk, Alt-folk and the occasional folk classics.
      (soma-fm "illstreet") # Classic bachelor pad, playful exotica and vintage music of tomorrow.
      (soma-fm "live") # Special Live Events and rebroadcasts of past live events
      (soma-fm "sonicuniverse") # Transcending the world of jazz with eclectic, avant-garde takes on tradition.
      (soma-fm "specials") # For Halloween: Dark industrial/ambient music for tortured souls.
    ];
  };

  Weihnacht = {
    description = "";
    tracks = [
      (soma-fm "christmas") # Chilled holiday grooves and classic winter lounge tracks. (Kid and Parent safe!)
      (soma-fm "jollysoul") # Where we cut right to the soul of the season.
      (soma-fm "xmasinfrisko") # SomaFM's wacky and eclectic holiday mix. Not for the easily offended.
      (soma-fm "xmasrocks") # Have your self an indie/alternative holiday season!
    ];
  };

  Charts = {
    description = "";
    tracks = [
      (rautemusik "main")
      (rautemusik "workout")
      (rautemusik "jam")
      (rautemusik "charthits")
      (rautemusik "top40")
      {
        url = "http://185.80.220.12:8166/stream"; # "Raidió Rírá"
        name = "Raidió Rí-Rá";
        provider = "Conradh na Gaeilge";
      }
      (big-fm "charts")
      (big-fm "deutschland")
      (big-fm "mashup")
      (rte "2fm")
    ];
  };

  Talk = {
    description = "";
    tracks = [
      (rte "radio1")
      (rte "radio1extra")
    ];
  };
}
