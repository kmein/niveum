{di-fm-key}:
# DI generated via: cat /tmp/di-fm.html| pup '.channel-tile-component json{}' | jq 'map({logo: .children[0].children[1].children[1].src | sub("^"; "http:"), station: .children[3].children[0].children[0].text, desc: .children[3].children[1].text | gsub("\\s+"; " ")})'  > /tmp/di-fm.json
# soma.fm generated via: curl https://somafm.com/ | pup '.cbshort json{}' | jq 'map({logo:.children[0].children[0].src|sub("^"; "http://soma.fm"), desc: .children[2].text, station: .children[1].text})'
let
  tags = {
    ambient = "🧘 Ambient";
    american = "🇺🇸 USA";
    amro = "👦 Amro";
    arabic = "🇸🇦 عربيic";
    balkan = "🇧🇦 Balkan";
    berlin = "🐻 Berlin";
    brazilian = "🇧🇷 Brasil";
    chill = "🧊 Chill";
    chinese = "🇨🇳 中国";
    classical = "🎻 Classical";
    discover = "😲 Discover";
    dnb = "🥁 DnB";
    dubstep = "🎆 Dubstep";
    french = "🇫🇷 France";
    geschepper = "🤯 Geschepper";
    greek = "🇬🇷 Ελλάδα";
    greenlandic = "🇬🇱 Kalaallit Nunaat";
    groovy = "🕺 Groovy";
    holy = "⛪ Holy";
    indian = "🇮🇳 भारत";
    irie = "🇯🇲 Irie";
    irish = "🇮🇪 Éire";
    jazz = "🎷 Jazz";
    lofi = "✍ Lo-Fi";
    metal = "🤘 Metal";
    party = "🪩 Party";
    pop = "🎙 Pop";
    rap = "💸 Rap";
    rock = "🎸 Rock";
    russian = "🇷🇺 Россия";
    schlager = "💩 Schlager";
    soma = "🍄 σῶμα – सोमः";
    text = "📚 Text";
    top40 = "♻️ Top 40";
    trad = "👘 Trad";
    trance = "🎇 Trance";
    trap = "🪤 Trap";
    turkish = "🇹🇷  Türkiye";
    vintage = "🕰️ Vintage";
    wave = "🌊 ＷＡＶＥ";
    xmas = "🎅 Christmas";
  };

  # https://github.com/NixOS/nixpkgs/blob/bc06c93905f60a82d6ebbb78f78cf289257860cc/lib/trivial.nix#L281-L282
  importJSON = path: builtins.fromJSON (builtins.readFile path);

  di-fm-name = name: "${name} | DI.FM";
  di-fm = name: "http://prem3.di.fm/${name}_hi?${di-fm-key}";

  soma-fm-name = name: "${name} | soma.fm";
  soma-fm = name: "http://ice1.somafm.com/${name}-128-aac";

  we-are-one-name = name: "${name} | We aRe oNe";
  we-are-one = name: "http://listen.${name}.fm/tunein-aac-hd-pls";

  big-fm-name = name: "${name} | bigFM";
  big-fm = name: "https://streams.bigfm.de/bigfm-${name}-128-aac";

  rautemusik-name = name: "${name} | rm.fm";
  rautemusik = name: "http://${name}-high.rautemusik.fm/";

  rte-name = name: "RTÉ ${name}";
  rte = name: "https://www.rte.ie/manifests/${name}.m3u8";

  royal-name = name: "${name} | RoyalRadio";
  royal = name: "http://193.33.170.218:8000/${name}";

  bhaktiworld-name = name: "${name} | Bhaktiworld";
  bhaktiworld = name: "http://${name}.out.airtime.pro:8000/${name}_a";
  bhaktiworld-logo = "http://www.bhaktiworld.com/Bhakti-world-logo.png";

  stereoscenic-name = name: "${name} | Stereoscenic";
  stereoscenic = name: "http://radio.stereoscenic.com/${name}";

  paloma = name: "https://pool.radiopaloma.de/${name}.mp3";
  paloma-name = name: "${name} | Radio Paloma";
  paloma-logo = "https://schlager.radio/wp-content/uploads/2020/01/Paloma-Logo-s.svg";

  radiorecord-name = name: "${name} | Radio Record";
  radiorecord = name: "http://air.radiorecord.ru:8102/${name}_320";
  radiorecord-logo = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 144 46'%3E%3Cpath d='M68.3 24H79c8-11.4 29.8-18.8 41.7-24l-5.4 1.6-.3.2-15.3 4.6-3.1.9-17 5.3-3.4-3.1h-5.4l-3.5 3.2L43 5 32 1.6 26.6 0c12.3 5.4 35.1 14.5 41.7 24zm5.2-11.4c.9 0 1.6.3 2.2.8.6.5.9 1.1.9 1.9 0 .8-.4 2-1.1 3-.5.7-1.2 1.2-1.9 1.3-.7 0-1.3-.5-1.8-1.2-.7-.9-1.1-2-1.1-2.9v-.2c-.2-1.5 1.1-2.7 2.8-2.7zm70.3 17.5c-.5-1.3-2.4-1.5-4.1-1.5h-15.9l-.1.4-.1.2-4.8 16-.1.2-.2.7h16.8c2.8 0 4.6-1 5.5-3.6L143 35c1-2.6 1.2-3.9.8-4.9zm-10.2 11.7h-6.3l2.7-8.9h6.3l-2.7 8.9zM94.5 29.1c-.8-.4-2-.5-3.1-.5H79.9c-2.8 0-4.6 1-5.5 3.6l-2.2 7.5c-.6 2-1.8 4.8.3 5.9.8.4 2 .5 3.1.5h11.5c2.8 0 4.6-1 5.5-3.6l2.2-7.5c.6-2 1.8-4.9-.3-5.9zm-9.2 12.7h-6.4l2.7-8.9H88l-2.7 8.9zm-39.8 0H32.8l.7-2.3h10.9l.1-.4.1-.2.9-2.9.1-.2.2-.7h-11l.7-2.3h13.8l.1-.4.1-.2.8-2.8.1-.2.2-.7H29.3l-.1.4-.2.2-4.8 16-.1.2-.1.7h21.2l.1-.4.1-.2.8-2.8.1-.2.2-.7h-1zm70.2-13.3H99.2c-1.6 5.5-3.3 11-4.9 16.6l-.1.2-.3.7h7.5l.1-.4.1-.2 1.7-5.6h2.5l2.7 5.7.1.1.1.3h8.2l-3.3-6.6c3.4-.4 4.2-1.8 4.8-4.2.2-.6.3-1.2.5-1.7 1.2-3.5-.1-4.8-3.2-4.9zm-4.3 5.1L111 35c-.1.4-.3.5-.7.6l-6.4 1.7 1.4-4.5h5.5c.6 0 .8.2.6.8zm-86.9 1.6c.2-.6.3-1.2.5-1.7 1.2-3.6-.1-4.9-3.2-4.9H5.3C3.7 34.1 2 39.6.4 45.2l-.1.2-.3.6h7.4l.1-.4.1-.2 1.7-5.6h2.5l2.7 5.7.1.1.1.3h8.2l-3.3-6.6c3.5-.3 4.3-1.7 4.9-4.1zm-7-1.6l-.4 1.4c-.1.4-.3.5-.7.6L10 37.3l1.4-4.5h5.5c.6 0 .8.2.6.8zM71.1 35l1.2-3.8c.5-1.3-.4-2.8-1.9-2.7H57c-2.8 0-4.6 1-5.5 3.6l-2.2 7.5c-.6 2-1.8 4.8.3 5.9.8.4 2 .5 3.1.5h12.5c1.5 0 3.2-1.4 3.6-2.7l1-3.7h-5.7l-.6 2.1h-7.3l2.7-8.9h7l-.8 2.2h6z' fill-rule='evenodd' clip-rule='evenodd' fill='%23fff'/%3E%3C/svg%3E";

  caster-fm = subdomain: port: "http://${subdomain}.caster.fm:${toString port}/listen.mp3?authn0b0236758bd0e178156d0787327a055d";
in
  [
    {
      stream = "http://lassul.us:8000/radio.ogg";
      station = "Radio lassulus";
      logo = "http://lassul.us/art/portraits/selbstportrait.jpg";
      desc = "Diminutive from lassus (“weary, faint, tired”). A programming human. Doing superior professional art.";
      tags = [tags.chill];
    }
    {
      station = "C3 Lounge";
      stream = "https://c3lounge.de/radio/8000/radio.mp3";
    }
    {
      stream = "https://radio.kmein.de/lyrik.ogg";
      station = "Lyrik";
      desc = "Lyrik-Lesung rund um die Uhr.";
      tags = [tags.text];
    }
    {
      stream = "https://radio.kmein.de/lyrikline.ogg";
      station = "Lyrikline";
      logo = "https://www.lyrikline.org/themes/lyrik/svg/Logo_lyrikline_pure.svg";
      desc = "24/7 zufällige Wiedergaben von lyrikline.org.";
      tags = [tags.text];
    }
    {
      stream = "https://radio.kmein.de/wikipedia.ogg";
      station = "Wikipedia";
      desc = "Zufällige Wikipedia-Artikel";
      logo = "https://de.wikipedia.org/wiki/Wikipedia:Enzyklop%C3%A4die/Logo_von_Wikipedia#/media/Datei:Wikipedia-logo-v2.svg";
      tags = [tags.text];
    }
    {
      stream = "http://162.244.80.20:6948";
      station = "Cool Jazz Florida";
      logo = "https://d3kle7qwymxpcy.cloudfront.net/images/broadcasts/d1/30/24209/c300.png";
      desc = "Sit Back and Relax and Enjoy the Sophisticated Cool Jazz on Cool Jazz Florida!";
      tags = [tags.jazz];
    }
    {
      stream = "https://stream1.mfm.plexpark.com/radio/8000/simulcast-berlin-sd-64.aac";
      station = "Metropol FM Berlin";
      logo = "https://www.metropolfm.de/wp-content/uploads/2017/04/berlin.png";
      tags = [tags.berlin tags.turkish];
    }
    {
      stream = "https://stream2.mfm.plexpark.com/radio/8040/genre-keyf-sd-64.aac";
      station = "Metropol FM KEYF";
      logo = "https://www.metropolfm.de/wp-content/uploads/2017/04/berlin.png";
      tags = [tags.turkish];
    }
    {
      stream = "https://stream2.mfm.plexpark.com/radio/8050/genre-popslow-sd-64.aac";
      station = "Metropol FM Slow";
      logo = "https://www.metropolfm.de/wp-content/uploads/2017/04/berlin.png";
      tags = [tags.turkish];
    }
    {
      stream = "https://stream1.mfm.plexpark.com/radio/8030/genre-arabesk-sd-64.aac";
      station = "Metropol FM Arabesk";
      logo = "https://www.metropolfm.de/wp-content/uploads/2017/04/berlin.png";
      tags = [tags.turkish];
    }
    {
      stream = "http://rb-stream.de:8000/rrb_128.mp3";
      station = "Radio Russkij Berlin";
      logo = "http://radio-rb.de/img/site/logo.png";
      desc = "Голос нашего города ...";
      tags = [tags.berlin tags.russian];
    }
    {
      stream = "https://drachenhits.stream.laut.fm/drachenhits";
      station = "Drachenhits";
    }
  ]
  ++
  # generated via: curl https://radiorecord.ru/api/stations | jq '.result.stations | sort_by(.sort) | map({station:.title,desc:.tooltip,logo:.icon_fill_colored,stream:.stream_320})' > radiorecord.json
  importJSON ./radiorecord.json
  ++ [
    {
      desc = "Your favorite dance tunes from the start of the decade. Familiar hits and overlooked classics in abundance.";
      station = di-fm-name "00s Club Hits";
      stream = di-fm "00sclubhits";
      tags = [tags.party];
    }
    {
      desc = "Electronic sounds and atmospheric textures create a genre to enhance your state of mind and take you deeper.";
      station = di-fm-name "Ambient";
      stream = di-fm "ambient";
      tags = [tags.chill tags.ambient];
    }
    {
      desc = "Spaced out, melodic and full of warmth – these broken beat dance tunes will keep you dazed and amused.";
      station = di-fm-name "Atmospheric Breaks";
      stream = di-fm "atmosphericbreaks";
      tags = [tags.chill];
    }
    {
      desc = "From the funkiest grooves to the dirtiest beats. Hard-hitting, high energy 4/4 club cuts to move the masses.";
      station = di-fm-name "Bass & Jackin' House";
      stream = di-fm "bassnjackinhouse";
      tags = [tags.party];
    }
    {
      desc = "Blending together elements of house music, speed garage, and techno – it's all about the low end frequencies.";
      station = di-fm-name "Bassline";
      stream = di-fm "bassline";
      tags = [tags.party];
    }
    {
      desc = "Heavily focused on breakbeats and dusty samples. A defining 90s musical movement still going strong today.";
      station = di-fm-name "Big Beat";
      stream = di-fm "bigbeat";
    }
    {
      desc = "Fusing together house elements from the past and the present – prime time music full of uplifting high energy.";
      station = di-fm-name "Big Room House";
      stream = di-fm "bigroomhouse";
      tags = [tags.party];
    }
    {
      desc = "Inspired by hip hop and UK rave music, breaks features broken up drum loops and creative samples, synths and fx.";
      station = di-fm-name "Breaks";
      stream = di-fm "breaks";
      tags = [tags.party];
    }
    {
      desc = "The sounds of Chill & Tropical House are expertly made for lounging and dancing alike with its deeper house vibes.";
      station = di-fm-name "Chill & Tropical House";
      stream = di-fm "chillntropicalhouse";
      tags = [tags.chill];
    }
    {
      desc = "Good EDM doesn't have to blow out your speakers and our curated selection of Chill EDM is a testament to the strength of mellow, chilled out electronic music.";
      station = di-fm-name "Chill EDM";
      stream = di-fm "chilledm";
      tags = [tags.chill tags.amro];
    }
    {
      desc = "Mellow chill beats, lofi hip-hop, trip hop, downtempo beats and jazz, blended together in a laid back style for perfect listening.";
      station = di-fm-name "ChillHop";
      stream = di-fm "chillhop";
      tags = [tags.chill tags.lofi];
    }
    {
      desc = "Electronic sounds, mellow mid-tempo rhythms, and a groove meant to calm the senses and ease the mind.";
      station = di-fm-name "Chillout";
      stream = di-fm "chillout";
      tags = [tags.chill];
    }
    {
      desc = "The perfect musical soundtrack for when you want to close your eyes, get truly comfortable, and drift away.";
      station = di-fm-name "Chillout Dreams";
      stream = di-fm "chilloutdreams";
      tags = [tags.chill];
    }
    {
      desc = "The brilliant combination of dubstep rhythms with the mellow grooves of chillout. A unique sound all its own.";
      station = di-fm-name "Chillstep";
      stream = di-fm "chillstep";
      tags = [tags.chill];
    }
    {
      desc = "European pop music born in the 90s full of high energy sounds and big hooks – now heard in gyms and malls worldwide.";
      station = di-fm-name "Classic EuroDance";
      stream = di-fm "classiceurodance";
      tags = [tags.party];
    }
    {
      desc = "Conceived in the European discos in the 70s, evolving through the decades into modern electronic masterpieces.";
      station = di-fm-name "Classic EuroDisco";
      stream = di-fm "classiceurodisco";
      tags = [tags.party];
    }
    {
      desc = "The classic melodies, the epic breakdowns and gigantic builds. Re-experience Trance music in her prime.";
      station = di-fm-name "Classic Trance";
      stream = di-fm "classictrance";
      tags = [tags.party tags.trance];
    }
    {
      desc = "Classic sounds of Vocal Trance";
      station = di-fm-name "Classic Vocal Trance";
      stream = di-fm "classicvocaltrance";
      tags = [tags.party tags.trance];
    }
    {
      desc = "The bassbin rattling, speaker-freaking hits of Dubstep – all tried, tested and approved to work in the clubs.";
      station = di-fm-name "Club Dubstep";
      stream = di-fm "clubdubstep";
      tags = [tags.party tags.dubstep];
    }
    {
      desc = "The music heard in the biggest venues worldwide. From prime time pushers to deeper house shakers – the sounds of now.";
      station = di-fm-name "Club Sounds";
      stream = di-fm "club";
      tags = [tags.party];
    }
    {
      desc = "From techno, deep house, progressive and trance – check out the sounds of the DJ deep in the mix.";
      station = di-fm-name "DJ Mixes";
      stream = di-fm "djmixes";
      tags = [tags.discover];
    }
    {
      desc = "Evil, gritty and twisted DnB / Drum & Bass. at 160+ BPM, hear the darkest basslines and the hardest hitting percussion.";
      station = di-fm-name "Dark DnB";
      stream = di-fm "darkdnb";
      tags = [tags.dnb];
    }
    {
      desc = "The darker form of PsyTrance, which is a sound all its own – direct from Goa to your headphones.";
      station = di-fm-name "Dark PsyTrance";
      stream = di-fm "darkpsytrance";
      tags = [tags.trance tags.geschepper];
    }
    {
      desc = "House music crafted for the smaller and mid-sized rooms – deeper tracks full of silky, smooth grooves.";
      station = di-fm-name "Deep House";
      stream = di-fm "deephouse";
      tags = [tags.party tags.chill];
    }
    {
      desc = "Elements of house, funk, and disco. Mid-tempo beats, soulful grooves and head nodding selections.";
      station = di-fm-name "Deep Nu-Disco";
      stream = di-fm "deepnudisco";
      tags = [tags.party tags.chill];
    }
    {
      desc = "This smooth, groove-heavy selection of deep progressive house tracks is the perfect soundtrack for smaller and mid-sized rooms.";
      station = di-fm-name "Deep Progressive House";
      stream = di-fm "deepprogressivehouse";
      tags = [tags.party tags.amro];
    }
    {
      desc = "A fusion of deep house & techno. Punchy grooves, spaced out sounds and forward thinking productions.";
      station = di-fm-name "Deep Tech";
      stream = di-fm "deeptech";
      tags = [tags.party tags.chill];
    }
    {
      desc = "Where would dance music be without Detroit? The city that started it all continues to inspire and educate.";
      station = di-fm-name "Detroit House & Techno";
      stream = di-fm "detroithousentechno";
      tags = [tags.party tags.geschepper];
    }
    {
      desc = "The feel good sound inspired from 70s disco combined with the warm kick drum of modern house music.";
      station = di-fm-name "Disco House";
      stream = di-fm "discohouse";
      tags = [tags.party];
    }
    {
      desc = "Head nodding beats, chilled vocals, and lush soundscapes to bring down the sun and start the night.";
      station = di-fm-name "Downtempo Lounge";
      stream = di-fm "downtempolounge";
      tags = [tags.chill];
    }
    {
      desc = "Born in the mid 90s, Drum and Bass / DnB is all about fast breakbeats, urban vibes, and rib rattling basslines.";
      station = di-fm-name "Drum and Bass";
      stream = di-fm "drumandbass";
      tags = [tags.dnb];
    }
    {
      desc = "A hybrid of half-time Dubstep and intense Drum and Bass / DnB.";
      station = di-fm-name "Drumstep";
      stream = di-fm "drumstep";
      tags = [tags.dubstep];
    }
    {
      desc = "An emphasis on the bass and drums / DnB, delayed effects, sampled vocals and smokey Reggae inspired vibes.";
      station = di-fm-name "Dub";
      stream = di-fm "dub";
      tags = [tags.irie];
    }
    {
      desc = "The beloved sounds of deep techno saturated with tape delays, heavy reverb and ice cold atmospherics.";
      station = di-fm-name "Dub Techno";
      stream = di-fm "dubtechno";
      tags = [tags.chill];
    }
    {
      desc = "The wobbles of the bass, the party rocking beats, and the biggest crowd destroying drops.";
      station = di-fm-name "Dubstep";
      stream = di-fm "dubstep";
      tags = [tags.dubstep];
    }
    {
      desc = "The sound of the largest events. From the gargantuan festivals, the huge main rooms and the biggest DJs.";
      station = di-fm-name "EDM Festival";
      stream = di-fm "edmfestival";
      tags = [tags.party];
    }
    {
      desc = "Where dance meets pop. Crossover favorites, stadium-sized anthems and the biggest electronic tunes in existence.";
      station = di-fm-name "EDM Hits";
      stream = di-fm "edm";
      tags = [tags.party];
    }
    {
      desc = "Buzzing basslines, huge kicks, party rocking drops. House music packed full of gigantic bass and massive synths.";
      station = di-fm-name "Electro House";
      stream = di-fm "electrohouse";
      tags = [tags.party];
    }
    {
      desc = "The combination of 1920s-1940s jazz and swing music, big band horns and modern day electro house.";
      station = di-fm-name "Electro Swing";
      stream = di-fm "electroswing";
      tags = [tags.vintage];
    }
    {
      desc = "The trailblazers, the renegades and the experimental musicians who gave early inspiration with electronic instruments.";
      station = di-fm-name "Electronic Pioneers";
      stream = di-fm "electronicpioneers";
      tags = [tags.discover];
    }
    {
      desc = "Catchy pop music blended together with vintage synthesizers and electronic instrumentation.";
      station = di-fm-name "Electropop";
      stream = di-fm "electropop";
      tags = [tags.party];
    }
    {
      desc = "Trance in its most boisterous form. Uplifting melodies on top of high energy beats create these euphoric anthems.";
      station = di-fm-name "Epic Trance";
      stream = di-fm "epictrance";
      tags = [tags.trance];
    }
    {
      desc = "Pop music infused with a high energy 4/4 pulse. Heavy on the synthesizers, the melodies and the vocals.";
      station = di-fm-name "EuroDance";
      stream = di-fm "eurodance";
      tags = [tags.party];
    }
    {
      desc = "Focused on the funkiest grooves, with plenty of the guitar licks and clever samples placed around a 4/4 swing.";
      station = di-fm-name "Funky House";
      stream = di-fm "funkyhouse";
      tags = [tags.party];
    }
    {
      desc = "Hard basslines, booming beats and insatiable grooves. Inspired by Trap, Juke and Garage – molded together into a unique booming style.";
      station = di-fm-name "Future Bass";
      stream = di-fm "futurebass";
      tags = [tags.amro];
    }
    {
      desc = "2step Garage rhythms, chunky bass line driven grooves and plenty of forward thinking innovation.";
      station = di-fm-name "Future Garage";
      stream = di-fm "futuregarage";
      tags = [tags.chill];
    }
    {
      desc = "Finest selection of futurepop and synthpop.";
      station = di-fm-name "Future Synthpop";
      stream = di-fm "futuresynthpop";
      tags = [tags.wave];
    }
    {
      desc = "The hardest form of techno with punishing tracks designed to drive the crowds into a sweaty frenzy.";
      station = di-fm-name "Gabber";
      stream = di-fm "gabber";
      tags = [tags.geschepper];
    }
    {
      desc = "The sound of digital malfunctions, electric hum and bit rate distortions perfectly placed alongside laid-back hip hop beats.";
      station = di-fm-name "Glitch Hop";
      stream = di-fm "glitchhop";
      tags = [tags.party];
    }
    {
      desc = "A very psychedelic form of trance, Goa-Psy Trance is a sound full of arpeggiated synths and trippy effects.";
      station = di-fm-name "Goa-Psy Trance";
      stream = di-fm "goapsy";
      tags = [tags.trance];
    }
    {
      desc = "A channel showcasing everything from hard dance, trance and happy hardcore to lift the spirits (and the arms).";
      station = di-fm-name "Hands Up";
      stream = di-fm "handsup";
      tags = [tags.party tags.amro];
    }
    {
      desc = "Concrete kicks and punching rhythms, hard dance is a tougher side of music with sharp edges and aggressive power.";
      station = di-fm-name "Hard Dance";
      stream = di-fm "harddance";
      tags = [tags.party];
    }
    {
      desc = "Tough as nails warehouse jams full of cold aggression, sinister structures and pounding rhythms that hit hard.";
      station = di-fm-name "Hard Techno";
      stream = di-fm "hardtechno";
      tags = [tags.party tags.geschepper];
    }
    {
      desc = "Strictly for the hardcore. These are the biggest and boldest bangers, and the hardest hitting tracks.";
      station = di-fm-name "Hardcore";
      stream = di-fm "hardcore";
      tags = [tags.geschepper];
    }
    {
      desc = "Hard techno & hardcore. A global phenomenon with powerful kicks, distorted effects and infectious melodies.";
      station = di-fm-name "Hardstyle";
      stream = di-fm "hardstyle";
      tags = [tags.geschepper];
    }
    {
      desc = "Born in Chicago and now global, house music is always evolving but remains true to its pure 4/4 structure.";
      station = di-fm-name "House";
      stream = di-fm "house";
      tags = [tags.party];
    }
    {
      desc = "Smooth, groovy and full of cutting-edge, fresh ideas – beats to kick back and enjoy far from the club setting.";
      station = di-fm-name "Indie Beats";
      stream = di-fm "indiebeats";
      tags = [tags.chill];
    }
    {
      desc = "The spirit of Rock & Roll with an electronic soul. Club culture and live music combined.";
      station = di-fm-name "Indie Dance";
      stream = di-fm "indiedance";
      tags = [tags.chill];
    }
    {
      desc = "One of the biggest cultural soundtracks with the infectious thump of house music. Expect sultry saxophones, trumpets, and finger snapping grooves.";
      station = di-fm-name "Jazz House";
      stream = di-fm "jazzhouse";
      tags = [tags.chill];
    }
    {
      desc = "Jungle keeps the breakbeat tempos high and celebrates the diverse ideas found within urban and rave music.";
      station = di-fm-name "Jungle";
      stream = di-fm "jungle";
    }
    {
      desc = "The sounds of Salsa, Brazilian beats and Latin Jazz with the steady grooves of modern East Coast dance music.";
      station = di-fm-name "Latin House";
      stream = di-fm "latinhouse";
      tags = [tags.party tags.groovy];
    }
    {
      desc = "Smooth as water, with the fast paced rhythms, Liquid DNB / Drum and Bass flows with rolling ease without losing momentum.";
      station = di-fm-name "Liquid DnB";
      stream = di-fm "liquiddnb";
      tags = [tags.chill tags.dnb];
    }
    {
      desc = "Smooth, rolling and steady – this fresh formation of Dubstep keeps the sounds you love with a flowing Drum and Bass groove.";
      station = di-fm-name "Liquid Dubstep";
      stream = di-fm "liquiddubstep";
      tags = [tags.chill tags.dubstep];
    }
    {
      desc = "The smoother side of Trap but still packed with mechanical grooves and hip hop moods.";
      station = di-fm-name "Liquid Trap";
      stream = di-fm "liquidtrap";
      tags = [tags.chill tags.amro tags.trap];
    }
    {
      desc = "Tastefully selected LoFi Hip-Hop tunes with textured atmospheres & laid back beats – with a dash of chillhop and perfectly designed to chill your ears.";
      station = di-fm-name "LoFi Hip-Hop";
      stream = di-fm "lofihiphop";
      tags = [tags.chill tags.lofi];
    }
    {
      desc = "Punch your one-way ticket to peace of mind and mental clarity with this curated selection of LoFi Lounge & Chill tracks today.";
      station = di-fm-name "LoFi Lounge & Chill";
      stream = di-fm "lofiloungenchill";
      tags = [tags.chill tags.lofi];
    }
    {
      desc = "Music to chill to. Music made for when it's all about kicking off your shoes, laying back, and totally relaxing.";
      station = di-fm-name "Lounge";
      stream = di-fm "lounge";
      tags = [tags.chill];
    }
    {
      desc = "The melodic side of progressive house, packed with driving rhythms and forward thinking sounds.";
      station = di-fm-name "Melodic Progressive";
      stream = di-fm "melodicprogressive";
      tags = [tags.amro];
    }
    {
      desc = "Minimal fuses elements of house, techno and electronica and strips it back to focus on the spaces between the sound.";
      station = di-fm-name "Minimal";
      stream = di-fm "minimal";
      tags = [tags.chill];
    }
    {
      desc = "Pitched up vocals, happy hardcore beats, and high energy music non-stop.";
      station = di-fm-name "Nightcore";
      stream = di-fm "nightcore";
      tags = [tags.geschepper tags.party];
    }
    {
      desc = "Modern disco music blending the familiar funk of the 70s and 80s with futuristic beats and up to date grooves.";
      station = di-fm-name "Nu Disco";
      stream = di-fm "nudisco";
      tags = [tags.party];
    }
    {
      desc = "Acid, one of the characteristics of the TB-303, is celebrated here with the best tracks from house, techno and trance.";
      station = di-fm-name "Oldschool Acid";
      stream = di-fm "oldschoolacid";
      tags = [tags.geschepper tags.party];
    }
    {
      desc = "The biggest classics and secret weapons – this is a true treasure chest of house tracks from back in the day.";
      station = di-fm-name "Oldschool House";
      stream = di-fm "oldschoolhouse";
      tags = [tags.party];
    }
    {
      desc = "Grab your whistles, white gloves and reach for the laser beams. This is the sound of raving when raving was new.";
      station = di-fm-name "Oldschool Rave";
      stream = di-fm "oldschoolrave";
      tags = [tags.party];
    }
    {
      desc = "Always moving forward, progressive continues to reinvent itself into new sounds and styles made for the floor.";
      station = di-fm-name "Progressive";
      stream = di-fm "progressive";
      tags = [tags.party tags.amro];
    }
    {
      desc = "Progress your mind to undiscovered psychedelic dimensions.";
      station = di-fm-name "Progressive Psy";
      stream = di-fm "progressivepsy";
      tags = [tags.geschepper tags.party];
    }
    {
      desc = "Downtempo psychedelic dub grooves, goa ambient, and world beats.";
      station = di-fm-name "PsyChill";
      stream = di-fm "psychill";
      tags = [tags.party];
    }
    {
      desc = "Dub, ambient, and psychedelic trance, fused together in atmospheric harmony.";
      station = di-fm-name "PsyDub";
      stream = di-fm "psydub";
      tags = [tags.irie];
    }
    {
      desc = "The psychedelic side of ambient.";
      station = di-fm-name "Psybient";
      stream = di-fm "psybient";
      tags = [tags.chill tags.ambient];
    }
    {
      desc = "Russia's hottest club hits.";
      station = di-fm-name "Russian Club Hits";
      stream = di-fm "russianclubhits";
      tags = [tags.russian tags.party];
    }
    {
      desc = "House music saturated with feeling – full of melodies, vocals and true soul. Steady warm 4/4 vibes.";
      station = di-fm-name "Soulful House";
      stream = di-fm "soulfulhouse";
      tags = [tags.groovy];
    }
    {
      desc = "Ambient space music for expanding minds.";
      station = di-fm-name "Space Dreams";
      stream = di-fm "spacemusic";
      tags = [tags.chill tags.ambient];
    }
    {
      desc = "This selection of summer chill house classics has been handpicked to elicit that special summer feeling year-round.";
      station = di-fm-name "Summer Chill House";
      stream = di-fm "summerchillhouse";
      tags = [tags.amro tags.party tags.chill];
    }
    {
      desc = "Influenced by video games and movie soundtracks of the 80s, Synthwave's mission continues today with great new music keeping things future retro.";
      station = di-fm-name "Synthwave";
      stream = di-fm "synthwave";
      tags = [tags.wave];
    }
    {
      desc = "Blending the warmth of house music with the cold structural precision of techno, tech house bridges the divide.";
      station = di-fm-name "Tech House";
      stream = di-fm "techhouse";
      tags = [tags.party];
    }
    {
      desc = "Techno is a true musical force full of structure and style. Robotic, mechanical and full of soul, always facing the future.";
      station = di-fm-name "Techno";
      stream = di-fm "techno";
      tags = [tags.party];
    }
    {
      desc = "Emotive dance music which embraces incredible melodies, future-facing production and energetic anthems heard worldwide.";
      station = di-fm-name "Trance";
      stream = di-fm "trance";
      tags = [tags.trance];
    }
    {
      desc = "Born out of Southern Hip-Hop and influenced by techno, trap is analog drum machines / DnB & with hip-hop aesthetics.";
      station = di-fm-name "Trap";
      stream = di-fm "trap";
      tags = [tags.trap tags.rap];
    }
    {
      desc = "The percussive side of the house and tech house scene, tribal house takes drums and puts them in the forefront.";
      station = di-fm-name "Tribal House";
      stream = di-fm "tribalhouse";
      tags = [tags.party];
    }
    {
      desc = "UMF Radio 24/7";
      station = di-fm-name "UMF Radio";
      stream = di-fm "umfradio";
      tags = [tags.party];
    }
    {
      desc = "From gritty Berlin streets to dark corners of Brooklyn, this is techno made by artists pushing the genre further.";
      station = di-fm-name "Underground Techno";
      stream = di-fm "undergroundtechno";
      tags = [tags.party tags.discover];
    }
    {
      desc = "Relaxing vibes and a collection of vocal songs providing the laid back soundtrack to your day.";
      station = di-fm-name "Vocal Chillout";
      stream = di-fm "vocalchillout";
      tags = [tags.chill];
    }
    {
      desc = "The glorious 4/4 thump of House music paired perfectly with the human voice. Sultry, soulful, sexy sounds.";
      station = di-fm-name "Vocal House";
      stream = di-fm "vocalhouse";
      tags = [tags.party];
    }
    {
      desc = "Laid back grooves and a collection of smooth vocals soothe the ears and relax the mind.";
      station = di-fm-name "Vocal Lounge";
      stream = di-fm "vocallounge";
      tags = [tags.party];
    }
    {
      desc = "Lush vocals paired together with emotive dance music. Beautiful melodies and endless energy.";
      station = di-fm-name "Vocal Trance";
      stream = di-fm "vocaltrance";
      tags = [tags.trance];
    }
    {
      desc = "All Vaporwave. All the time.";
      logo = "http://soma.fm/img/vaporwaves120.jpg";
      station = soma-fm-name "Vaporwaves";
      stream = soma-fm "vaporwaves";
      tags = [tags.soma tags.wave];
    }
    {
      desc = "Featuring the music from an independent record label focused on modern electronic ambient and space music.";
      logo = "https://somafm.com/img3/synphaera120.jpg";
      station = soma-fm-name "Synphaera";
      stream = soma-fm "synphaera";
      tags = [tags.soma tags.ambient];
    }
    {
      desc = "Emotional Experiments in Music: Ambient, modern composition, post-rock, & experimental electronic music";
      logo = "http://soma.fm/img/n5md120.png";
      station = soma-fm-name "n5MD Radio";
      stream = soma-fm "n5md";
      tags = [tags.soma tags.discover];
    }
    {
      desc = "A nicely chilled plate of ambient/downtempo beats and grooves.";
      logo = "http://soma.fm/img/groovesalad120.png";
      station = soma-fm-name "Groove Salad";
      stream = soma-fm "groovesalad";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "The classic (early 2000s) version of a nicely chilled plate of ambient/downtempo beats and grooves.";
      logo = "http://soma.fm/img3/gsclassic120.jpg";
      station = soma-fm-name "Groove Salad Classic";
      stream = soma-fm "gsclassic";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Deep ambient electronic, experimental and space music. For inner and outer space exploration.";
      logo = "http://soma.fm/img/deepspaceone120.gif";
      station = soma-fm-name "Deep Space One";
      stream = soma-fm "deepspaceone";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Tune in, turn on, space out. Spaced-out ambient and mid-tempo electronica.";
      logo = "http://soma.fm/img/sss.jpg";
      station = soma-fm-name "Space Station Soma";
      stream = soma-fm "spacestation";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Served best chilled, safe with most medications. Atmospheric textures with minimal beats.";
      logo = "http://soma.fm/img/dronezone120.jpg";
      station = soma-fm-name "Drone Zone";
      stream = soma-fm "dronezone";
      tags = [tags.soma tags.ambient];
    }
    {
      desc = "Progressive house / trance. Tip top tunes.";
      logo = "http://soma.fm/img/thetrip120.jpg";
      station = soma-fm-name "The Trip";
      stream = soma-fm "thetrip";
      tags = [tags.soma tags.trance tags.party];
    }
    {
      desc = "Music for Hacking. The DEF CON Year-Round Channel.";
      logo = "http://soma.fm/img/defcon120.png";
      station = soma-fm-name "DEF CON Radio";
      stream = soma-fm "defcon";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Transcending the world of jazz with eclectic, avant-garde takes on tradition.";
      logo = "http://soma.fm/img/sonicuniverse120.jpg";
      station = soma-fm-name "Sonic Universe";
      stream = soma-fm "sonicuniverse";
      tags = [tags.soma tags.jazz];
    }
    {
      desc = "NEW! Reggae, Ska, Rocksteady classic and deep tracks.";
      logo = "http://soma.fm/img3/reggae120.png";
      station = soma-fm-name "Heavyweight Reggae";
      stream = soma-fm "reggae";
      tags = [tags.soma tags.irie];
    }
    {
      desc = "Vintage soul tracks from the original 45 RPM vinyl.";
      logo = "http://soma.fm/img/7soul120.png";
      station = soma-fm-name "Seven Inch Soul";
      stream = soma-fm "7soul";
      tags = [tags.soma tags.groovy tags.vintage];
    }
    {
      desc = "Mellow album rock from the Seventies. Yacht not required.";
      logo = "http://soma.fm/img/seventies120.jpg";
      station = soma-fm-name "Left Coast 70s";
      stream = soma-fm "seventies";
      tags = [tags.soma tags.vintage];
    }
    {
      desc = "Early 80s UK Synthpop and a bit of New Wave.";
      logo = "http://soma.fm/img/u80s-120.png";
      station = soma-fm-name "Underground 80s";
      stream = soma-fm "u80s";
      tags = [tags.soma tags.wave tags.vintage];
    }
    {
      desc = "The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!";
      logo = "http://soma.fm/img/secretagent120.jpg";
      station = soma-fm-name "Secret Agent";
      stream = soma-fm "secretagent";
      tags = [tags.soma tags.discover];
    }
    {
      desc = "Sensuous and mellow vocals, mostly female, with an electronic influence.";
      logo = "http://soma.fm/img/lush120.jpg";
      station = soma-fm-name "Lush";
      stream = soma-fm "lush";
      tags = [tags.soma tags.amro];
    }
    {
      desc = "Exploring music from Celtic roots and branches";
      logo = "http://soma.fm/img/thistle120.png";
      station = soma-fm-name "ThistleRadio";
      stream = soma-fm "thistle";
      tags = [tags.soma tags.trad tags.irish];
    }
    {
      desc = "Drown in the electronic sound of instrumental hiphop, future soul and liquid trap.";
      logo = "http://soma.fm/img/fluid120.jpg";
      station = soma-fm-name "Fluid";
      stream = soma-fm "fluid";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Electropop and indie dance rock with sparkle and pop.";
      logo = "http://soma.fm/img/poptron120.png";
      station = soma-fm-name "PopTron";
      stream = soma-fm "poptron";
      tags = [tags.soma tags.pop];
    }
    {
      desc = "A late night blend of deep-house and downtempo chill.";
      logo = "http://soma.fm/img/blender120.png";
      station = soma-fm-name "Beat Blender";
      stream = soma-fm "beatblender";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Americana Roots music for Cowhands, Cowpokes and Cowtippers";
      logo = "http://soma.fm/img/bootliquor120.jpg";
      station = soma-fm-name "Boot Liquor";
      stream = soma-fm "bootliquor";
      tags = [tags.soma tags.trad tags.american];
    }
    {
      desc = "Classic bachelor pad, playful exotica and vintage music of tomorrow.";
      logo = "http://soma.fm/img/illstreet.jpg";
      station = soma-fm-name "Illinois Street Lounge";
      stream = soma-fm "illstreet";
      tags = [tags.soma tags.jazz tags.groovy];
    }
    {
      desc = "New and classic favorite indie pop tracks.";
      logo = "http://soma.fm/img/indychick.jpg";
      station = soma-fm-name "Indie Pop Rocks!";
      stream = soma-fm "indiepop";
      tags = [tags.soma tags.rock tags.pop];
    }
    {
      desc = "Digitally affected analog rock to calm the agitated heart.";
      logo = "http://soma.fm/img/digitalis120.png";
      station = soma-fm-name "Digitalis";
      stream = soma-fm "digitalis";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Indie Folk, Alt-folk and the occasional folk classics.";
      logo = "http://soma.fm/img/folkfwd120.jpg";
      station = soma-fm-name "Folk Forward";
      stream = soma-fm "folkfwd";
      tags = [tags.soma tags.trad];
    }
    {
      desc = "Blips'n'beeps backed mostly w/beats. Intelligent Dance Music.";
      logo = "http://soma.fm/img/cliqhop120.png";
      station = soma-fm-name "cliqhop idm";
      stream = soma-fm "cliqhop";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Dubstep, Dub and Deep Bass. May damage speakers at high volume.";
      logo = "http://soma.fm/img/dubstep120.png";
      station = soma-fm-name "Dub Step Beyond";
      stream = soma-fm "dubstep";
      tags = [tags.soma tags.dubstep];
    }
    {
      desc = "Desi-influenced Asian world beats and beyond.";
      logo = "http://soma.fm/img/sog120.jpg";
      station = soma-fm-name "Suburbs of Goa";
      stream = soma-fm "suburbsofgoa";
      tags = [tags.soma];
    }
    {
      desc = "Ambient music mixed with the sounds of San Francisco public safety radio traffic.";
      logo = "http://soma.fm/img/sf1033120.png";
      station = soma-fm-name "SF 10-33";
      stream = soma-fm "sf1033";
      tags = [tags.soma tags.ambient];
    }
    {
      desc = "San Francisco Public Safety Scanner Feed";
      logo = "http://soma.fm/img/sf1033120.png";
      station = soma-fm-name "SF Police Scanner";
      stream = soma-fm "scanner";
      tags = [tags.soma tags.text];
    }
    {
      desc = "Celebrating NASA and Space Explorers everywhere.";
      logo = "http://soma.fm/img/missioncontrol120.jpg";
      station = soma-fm-name "Mission Control";
      stream = soma-fm "missioncontrol";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "From black to doom, prog to sludge, thrash to post, stoner to crossover, punk to industrial.";
      logo = "http://soma.fm/img3/metal120.png";
      station = soma-fm-name "Metal Detector";
      stream = soma-fm "metal";
      tags = [tags.soma tags.metal tags.rock];
    }
    {
      desc = "Just covers. Songs you know by artists you don't. We've got you covered.";
      logo = "http://soma.fm/img/covers120.jpg";
      station = soma-fm-name "Covers";
      stream = soma-fm "covers";
      tags = [tags.soma tags.pop tags.rock];
    }
    {
      desc = "From the Playa to the world, for the annual Burning Man festival.";
      logo = "http://soma.fm/img/1023brc.jpg";
      station = soma-fm-name "Black Rock FM";
      stream = soma-fm "brfm";
      tags = [tags.soma tags.chill];
    }
    {
      desc = "Special Live Events and rebroadcasts of past live events";
      logo = "http://soma.fm/img/SomaFMDJSquare120.jpg";
      station = soma-fm-name "SomaFM Live";
      stream = soma-fm "live";
      tags = [tags.soma tags.discover];
    }
    {
      desc = "SomaFM's wacky and eclectic holiday mix. Not for the easily offended.";
      logo = "http://soma.fm/img/xmasinfrisco120.jpg";
      station = soma-fm-name "Xmas in Frisko";
      stream = soma-fm "xmasinfrisko";
      tags = [tags.soma tags.xmas];
    }
    {
      desc = "Chilled holiday grooves and classic winter lounge tracks. (Kid and Parent safe!)";
      logo = "http://soma.fm/img/christmaslounge120.png";
      station = soma-fm-name "Christmas Lounge";
      stream = soma-fm "christmas";
      tags = [tags.soma tags.xmas];
    }
    {
      desc = "Have your self an indie/alternative holiday season!";
      logo = "http://soma.fm/img/xmasrocks120.png";
      station = soma-fm-name "Christmas Rocks!";
      stream = soma-fm "xmasrocks";
      tags = [tags.soma tags.xmas tags.rock];
    }
    {
      desc = "Where we cut right to the soul of the season.";
      logo = "http://soma.fm/img/jollysoul120.png";
      station = soma-fm-name "Jolly Ol' Soul";
      stream = soma-fm "jollysoul";
      tags = [tags.soma tags.xmas tags.groovy];
    }
    {
      desc = "Department Store Christmas (extended through Jan 31)";
      logo = "http://soma.fm/img/SomaFMDJSquare120.jpg";
      station = soma-fm-name "SomaFM Specials";
      stream = soma-fm "specials";
      tags = [tags.soma tags.discover];
    }
    {
      desc = "HandsUp / Dance";
      logo = "https://www.technobase.fm/content/images/site/logo-technobase.fm.png";
      station = we-are-one-name "TechnoBase.FM";
      stream = we-are-one "technobase";
      tags = [tags.party];
    }
    {
      desc = "Dance & Pop";
      logo = "https://www.housetime.fm/content/images/site/logo-housetime.fm.png";
      station = we-are-one-name "HouseTime.FM";
      stream = we-are-one "housetime";
      tags = [tags.party];
    }
    {
      desc = "Hardstyle";
      logo = "https://www.hardbase.fm/content/images/site/logo-hardbase.fm.png";
      station = we-are-one-name "HardBase.FM";
      stream = we-are-one "hardbase";
      tags = [tags.party];
    }
    {
      desc = "Vocal & Uplifting Trance";
      logo = "https://www.trancebase.fm/content/images/site/logo-trancebase.fm.png";
      station = we-are-one-name "TranceBase.FM";
      stream = we-are-one "trancebase";
      tags = [tags.trance];
    }
    {
      desc = "Hardcore";
      logo = "https://www.coretime.fm/content/images/site/logo-coretime.fm.png";
      station = we-are-one-name "CoreTime.FM";
      stream = we-are-one "coretime";
      tags = [tags.geschepper];
    }
    {
      desc = "Techno / Minimal";
      logo = "https://www.clubtime.fm/content/images/site/logo-clubtime.fm.png";
      station = we-are-one-name "ClubTime.FM";
      stream = we-are-one "clubtime";
      tags = [tags.party];
    }
    {
      desc = "Happy Hardcore / DnB";
      logo = "https://www.teatime.fm/content/images/site/logo-teatime.fm.png";
      station = we-are-one-name "TeaTime.FM";
      stream = we-are-one "teatime";
      tags = [tags.geschepper];
    }
    {
      desc = "90s / 00s";
      logo = "https://www.replay.fm/content/images/site/logo-replay.fm.png";
      station = we-are-one-name "Replay.FM";
      stream = we-are-one "replay";
      tags = [tags.party tags.pop];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Bigg-G-Quadrat-Webstream-final-klein.jpg";
      station = big-fm-name "Dancehall – Reggae – Afrobeat";
      stream = big-fm "reggaevibes";
      tags = [tags.irie];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigBALKAN_0.jpg";
      station = big-fm-name "bigBALKAN";
      stream = big-fm "balkan";
      tags = [tags.balkan];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigSES_0.jpg";
      station = big-fm-name "bigSES (Türkei)";
      stream = big-fm "turkey";
      tags = [tags.turkish];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_CHARTS.jpg";
      station = big-fm-name "Charts";
      stream = big-fm "charts";
      tags = [tags.pop tags.top40];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Rock_am_Ring_720_Webradio_Crowd_2.jpg";
      station = big-fm-name "Rock am Ring";
      stream = big-fm "rockamring";
      tags = [tags.rock];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_MASHUP_0.jpg";
      station = big-fm-name "Mashup";
      stream = big-fm "mashup";
      tags = [tags.party tags.amro];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigFM_US_Rap_Hip-Hop.jpg";
      station = big-fm-name "US Rap & Hip Hop";
      stream = big-fm "usrap";
      tags = [tags.rap];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_HIP-HOP_0.jpg";
      station = big-fm-name "Hip-Hop";
      stream = big-fm "hiphop";
      tags = [tags.rap];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Oldschool_Rap_Hip_Hop_720x720.jpg";
      station = big-fm-name "Oldschool Rap & Hip Hop";
      stream = big-fm "oldschool";
      tags = [tags.rap];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Hip_Hop_Charts_DJ_blau.jpg";
      station = big-fm-name "Deutscher Hip-Hop Charts";
      stream = big-fm "dhiphopcharts";
      tags = [tags.rap];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigFM_DEUTSCHRAP.jpg";
      station = big-fm-name "Deutschrap";
      stream = big-fm "deutschrap";
      tags = [tags.rap];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Markus-Spiske-unsplash-quadrat-berlin.jpg";
      station = big-fm-name "Neu Berlin Deutschrap";
      stream = big-fm "rapfeature";
      tags = [tags.rap];
    }
    {
      logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Oldschool_Deutsch_720x720.jpeg";
      station = big-fm-name "Oldschool Deutschrap";
      stream = big-fm "oldschooldeutsch";
      tags = [tags.rap];
    }
    {
      station = "Schlagerparadies";
      stream = "https://webstream.schlagerparadies.de/schlagerparadies128k.mp3";
      logo = "https://cdn.schlagerparadies.de/images/rsp_setup/logo-radio-schlagerparadies.svg";
      tags = [tags.schlager];
    }
    {
      station = rautemusik-name "Volksmusik";
      desc = "Volksmusik, Blasmusik, Schlager";
      stream = rautemusik "volksmusik";
      tags = [tags.schlager];
    }
    {
      station = rautemusik-name "Study";
      stream = rautemusik "study";
      desc = "Lo-Fi, Chillout, Easy Listening";
      tags = [tags.lofi tags.chill];
    }
    {
      station = rautemusik-name "TechHouse";
      stream = rautemusik "techhouse";
      desc = "Techhouse, Deephouse, Techno, Minimal";
      tags = [tags.party];
    }
    {
      station = rautemusik-name "Goldies";
      stream = rautemusik "goldies";
      desc = "Oldies, 60s, 70s, 80s";
      tags = [tags.vintage];
    }
    {
      station = rautemusik-name "90s";
      stream = rautemusik "90s";
      desc = "90s, Eurodance, Pop, HipHop";
      tags = [tags.vintage];
    }
    {
      station = rautemusik-name "Schlager";
      stream = rautemusik "schlager";
      desc = "Schlager, Discofox, Deutsch, Pop";
      tags = [tags.schlager];
    }
    {
      station = rautemusik-name "Country";
      stream = rautemusik "country";
      desc = "Country, Western, Americana";
      tags = [tags.trad tags.american];
    }
    {
      station = rautemusik-name "Sex";
      stream = rautemusik "sex";
      desc = "RnB, Pop, Easy Listening";
      tags = [tags.chill];
    }
    {
      station = rautemusik-name "LoveHits";
      stream = rautemusik "lovehits";
      desc = "Lovesongs, Balladen, RnB, Pop";
      tags = [tags.pop];
    }
    {
      station = rautemusik-name "Klassik";
      stream = rautemusik "klassik";
      desc = "Symphonie, Orchester, Klassik";
      tags = [tags.classical];
    }
    {
      station = rautemusik-name "Traurig";
      stream = rautemusik "traurig";
      desc = "Balladen, Pop, Easy Listening";
      tags = [tags.pop];
    }
    {
      station = rautemusik-name "Happy";
      stream = rautemusik "happy";
      desc = "Pop, Dance, Charts";
      tags = [tags.pop];
    }
    {
      station = rautemusik-name "Solo Piano";
      stream = rautemusik "solopiano";
      desc = "Klavier, Instrumental, Easy Listening";
      tags = [tags.classical];
    }
    {
      station = rautemusik-name "HappyHardcore";
      stream = rautemusik "happyhardcore";
      desc = "UK Core, Happy Hardcore, Dance";
      tags = [tags.party];
    }
    {
      station = rautemusik-name "HardeR";
      stream = rautemusik "harder";
      desc = "Hardstyle, Hardcore, Jumpstyle";
      tags = [tags.party];
    }
    {
      station = rautemusik-name "BigCityBeats";
      stream = rautemusik "bigcitybeats";
      desc = "EDM, Dance, House, Electro, Star DJs";
      tags = [tags.pop tags.party];
    }
    {
      station = rautemusik-name "Lounge";
      stream = rautemusik "lounge";
      desc = "Ambient, Jazz, Chillout, Easy Listening";
      tags = [tags.chill];
    }
    {
      station = rautemusik-name "Oriental";
      stream = rautemusik "oriental";
      desc = "Arabisch, Oriental, HipHop";
      tags = [tags.arabic];
    }
    {
      station = rautemusik-name "Salsa";
      stream = rautemusik "salsa";
      desc = "Salsa, Latina, Tropical";
      tags = [tags.groovy];
    }
    {
      station = rautemusik-name "Christmas";
      stream = rautemusik "christmas";
      desc = "Weihnachtslieder, Balladen, Schlager";
      tags = [tags.xmas];
    }
    {
      station = rautemusik-name "Christmas Chor";
      stream = rautemusik "christmas-chor";
      desc = "Chor, Weihnachtslieder, Gesang";
      tags = [tags.xmas];
    }
    {
      station = rautemusik-name "Christmas Schlager";
      stream = rautemusik "christmas-schlager";
      desc = "Schlager, Weihnachtslieder";
      tags = [tags.xmas tags.schlager];
    }
    {
      station = rautemusik-name "Weihnachten.FM";
      stream = rautemusik "weihnachten";
      desc = "Weihnachtslieder, Pop";
      tags = [tags.xmas];
    }
    {
      station = rautemusik-name "Top40";
      stream = rautemusik "top40";
      desc = "Charts, Top40, Dance, Hiphop";
      tags = [tags.top40];
    }
    {
      station = rautemusik-name "Rock";
      desc = "Rock, Alternative, Punk";
      stream = rautemusik "rock";
      tags = [tags.rock];
    }
    {
      station = rautemusik-name "PartyHits";
      desc = "Karneval, Mallorca, Après Ski, Schlager";
      stream = rautemusik "partyhits";
      tags = [tags.schlager];
    }
    {
      station = rautemusik-name "Deutschrap Charts";
      stream = rautemusik "deutschrap-charts";
      desc = "Deutschrap, HipHop, Rap, Charts";
      tags = [tags.rap];
    }
    {
      station = rautemusik-name "Deutschrap Classic";
      stream = rautemusik "deutschrap-classic";
      desc = "Oldschool, Rap, HipHop, Deutschrap";
      tags = [tags.rap];
    }
    {
      station = rautemusik-name "ChartHits";
      stream = rautemusik "ChartHits";
      desc = "House, RnB, Dance, Electro";
      tags = [tags.top40];
    }
    {
      station = rautemusik-name "BreakZ.FM";
      stream = rautemusik "breakz";
      desc = "RnB, House, HipHop, Dance, Mixtapes";
      tags = [tags.top40];
    }
    {
      station = rautemusik-name "Bass";
      stream = rautemusik "bass";
      desc = "DnB, Dubstep, Trap & Bass House";
      tags = [tags.party];
    }
    {
      station = rautemusik-name "12punks";
      stream = rautemusik "12punks";
      desc = "Punk, Punk Rock, Ska, Hardcore";
      tags = [tags.rock];
    }
    {
      logo = "https://d3kle7qwymxpcy.cloudfront.net/images/broadcasts/77/a4/13931/1/c175.png";
      station = "Raidió Rírá";
      stream = "http://185.80.220.12:8166/stream";
      desc = "Is cairt-staisiún ceoil é Raidió Rí-Rá a bhíonn ag craoladh go hiomlán trí Ghaeilge! Bíonn an ceol ar fad ó na cairteacha le cloisteáil ar an stáisiún, mar aon leis an bpopnuacht, an nuacht spóirt agus an nuacht scannánaíochta is déanaí!";
      tags = [tags.top40];
    }
    {
      stream = "http://188.247.86.67:8008";
      station = "Rotana Tarab";
      logo = "https://liveonlineradio.net/wp-content/uploads/2017/11/Rotana-Tarab-100x47.jpg";
      tags = [tags.trad tags.arabic];
    }
    {
      stream = "http://asima.out.airtime.pro:8000/asima_a";
      station = "Asima";
      tags = [tags.arabic];
    }
    {
      station = "ARTA FM";
      stream = "http://edge.mixlr.com/channel/qtgru";
      tags = [tags.arabic];
    }
    {
      station = "Ninar FM";
      stream = "http://ninarfm.grtvstream.com:8896/stream";
      tags = [tags.arabic];
    }
    {
      stream = "http://5.9.16.111:8210/arabic_live";
      station = "Radio Arabica";
      logo = "https://radioarabica.de/wp-content/uploads/2020/09/LOGO_klein-1.png";
      tags = [tags.berlin tags.arabic];
    }
    {
      stream = "http://iphone.live24.gr/derty1000";
      station = "Derti FM";
      desc = "Μόνο λαϊκά";
      logo = "https://cdn-radiotime-logos.tunein.com/s87063q.png";
      tags = [tags.trad tags.greek];
    }
    {
      logo = "http://www.stoxosfm.gr/images/stoxosfm-logo-small.png";
      desc = "Ο Στόχος FM μετράει σχεδόν 25 χρόνια ηχηρής παρουσίας στα ερτζιανά. Ιδρύθηκε το 1990 και έκτοτε έχει διαγράψει μια δυναμική και άκρως επιτυχημένη πορεία, κατακτώντας την προτίμηση ενός σταθερού και ολοένα αυξανόμενου ακροατηρίου.";
      station = "Stoxos FM";
      stream = "http://s3.onweb.gr:8016/;";
      tags = [tags.greek];
    }
    {
      stream = bhaktiworld "2bhanuman";
      station = bhaktiworld-name "Hanuman";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "djbeat";
      station = bhaktiworld-name "Mantra Shakti";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "gurbani";
      station = bhaktiworld-name "Sangam";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "hot";
      station = bhaktiworld-name "Shiv";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "ibadat";
      station = bhaktiworld-name "Devi Maa";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "iskon2b";
      station = bhaktiworld-name "Om Sai";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "millenniumhits";
      station = bhaktiworld-name "Krishna";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "dard";
      station = bhaktiworld-name "Shri Ram";
      tags = [tags.indian tags.trad];
    }
    {
      stream = bhaktiworld "bhaktiworldindia";
      station = bhaktiworld-name "Ganesh";
      tags = [tags.indian tags.trad];
    }
    {
      station = "Rockabilly Radio";
      stream = "http://lin3.ash.fast-serv.com:6026/stream_96";
      logo = "https://static.wixstatic.com/media/c62c90_074ca7d75d204a7a9f9ee576e3e5c6fb~mv2.png/v1/fill/w_521,h_168,al_c,q_85,usm_0.66_1.00_0.01/rbrbannernewblue_edited.webp";
      desc = "The home of rockin' music.";
      tags = [tags.trad tags.american tags.vintage];
    }
    {
      logo = "https://www.liveradio.ie/files/images/115732/resized/180x172c/rte_raidio_na_gaeltachta.jpg";
      station = rte-name "Raidió Na Gaeltachta";
      desc = "Stáisiún Náisiúnta na Gaeltachta agus na Gaeilge, ag craoladh as Gaeilge.";
      stream = rte "rnag";
      tags = [tags.irish tags.text];
    }
    {
      logo = "https://www.liveradio.ie/files/images/115762/resized/180x172c/rte_gold.jpg";
      stream = rte "gold";
      station = rte-name "Gold";
      desc = "RTÉ Gold is a play list service offering a carefully chosen selection of classic hits as well as album tracks from top selling artists spanning the '50s, '60s, ‘70s and ‘80s.";
      tags = [tags.vintage];
    }
    {
      logo = "https://www.liveradio.ie/files/images/101096/resized/180x172c/rte_lyric_fm.png";
      stream = rte "lyric";
      station = rte-name "Lyric FM";
      desc = "“Without doubt RTÉ lyric fm is for listeners that are looking for a real alternative. With an increase in people tuning to the station ‘where life sounds better’ across weekends, there is something to satisfy everyone’s taste for specialist or more mainstream music.”";
      tags = [tags.classical];
    }
    {
      logo = "https://www.liveradio.ie/files/images/329303/resized/180x172c/rte_pulse.jpg";
      station = rte-name "Pulse";
      stream = rte "pulse";
      desc = "RTÉ Pulse is an electronic dance music station from Raidió Teilifís Éireann (RTÉ), Ireland's national broadcaster.";
      tags = [tags.top40];
    }
    {
      logo = "https://www.liveradio.ie/files/images/338090/resized/180x172c/rte_radio_1.jpg";
      station = rte-name "Radio 1";
      stream = rte "radio1";
      desc = "RTÉ Radio 1 is the principal radio channel of Irish public-service broadcaster Raidió Teilifís Éireann. The station is a rare modern example of a mixed radio network, broadcasting a mixture of music and speech programming.";
      tags = [tags.top40 tags.irish];
    }
    {
      logo = "https://www.liveradio.ie/files/images/115731/resized/180x172c/rte_radio_1_extra.jpg";
      station = rte-name "Radio 1 Extra";
      stream = rte "radio1extra";
      desc = "RTE Radio 1 Extra (aka RTE Radio 1xtra) – Quality speech radio from home and abroad.";
      tags = [tags.text tags.irish];
    }
    {
      logo = "https://cdn-profiles.tunein.com/s96877/images/logoq.png";
      station = "DWG Radio";
      desc = "Bibeltreues Radio im Internet";
      stream = "http://server25531.streamplus.de/;stream.mp3";
      tags = [tags.text tags.holy];
    }
    {
      station = "Wake News";
      logo = "https://stream.wakenews.net/wakenews-radio-200px.jpg";
      stream = "https://stream.wakenews.net/radio-high.ogg";
      desc = "Ohne Blatt vor dem Mund! Für alle, die aufwachen wollen.";
      tags = [tags.text];
    }
    {
      logo = "http://www.beatlesradio.com/content/images/thumbs/0000587.gif";
      station = "Beatles Radio";
      stream = "http://www.beatlesradio.com:8000/stream";
      desc = "";
      tags = [tags.vintage];
    }
    {
      stream = "http://www.c64.com:8000";
      station = "C64";
      logo = "http://www.c64.com/games/recommended.small.jpg.php?id=1969";
      desc = "ChipTune 24/7.";
    }
    {
      logo = "https://knr.gl/sites/knr/themes/knr/gfx/sprite.png";
      station = "Kalaallit Nunaata Radioa";
      stream = "https://knr.gl/radiolivestream";
      tags = [tags.greenlandic];
    }
    {
      logo = "http://sithafm.lk/sithafm.jpg";
      station = "Sitha.FM";
      stream = caster-fm "shaincast" 48148;
      tags = [tags.indian];
    }
    {
      stream = royal "RoyalPopsa";
      station = royal-name "Popsa";
      desc = "из Санкт-Петербурга";
      tags = [tags.pop];
    }
    {
      stream = royal "RoyalTrance";
      station = royal-name "Trance";
      desc = "из Санкт-Петербурга";
      tags = [tags.trance];
    }
    {
      stream = royal "RoyalDrum";
      station = royal-name "Drum";
      desc = "из Санкт-Петербурга";
      tags = [tags.dnb];
    }
    {
      stream = royal "RoyalTrap";
      station = royal-name "Trap";
      desc = "из Санкт-Петербурга";
      tags = [tags.trap];
    }
    {
      stream = royal "RoyalRock";
      station = royal-name "Rock";
      desc = "из Санкт-Петербурга";
      tags = [tags.rock];
    }
    {
      stream = royal "RoyalLounge";
      station = royal-name "Lounge";
      desc = "из Санкт-Петербурга";
      tags = [tags.chill];
    }
    {
      stream = royal "RoyaLove";
      station = royal-name "Love";
      desc = "из Санкт-Петербурга";
      tags = [tags.pop];
    }
    {
      station = "Radio Сигма";
      stream = "http://195.191.130.125:8000/sigma";
      desc = "Novy Urengoy 102.3 FM";
      tags = [tags.russian];
    }
    {
      station = "Rap Français | Mouv";
      stream = "http://icecast.radiofrance.fr/mouvrapfr-midfi.mp3";
      logo = "https://cdn.radiofrance.fr/s3/cruiser-production/2019/01/3c4dc967-ed2c-4ce5-a998-9437a64e05d5/300x300_rapfr.jpg";
      tags = [tags.french tags.rap];
    }
    {
      stream = "http://66.45.232.131:9994/;stream.mp3";
      station = "ERTU Al Quran Al Kareem";
      tags = [tags.arabic tags.text tags.holy];
    }
    {
      stream = "http://onair15.xdevel.com:7064/1/";
      station = "Radio Mozart Italia";
      logo = "https://www.lafenicepubblicita.it/rmi/wp-content/uploads/2020/12/360x180.jpg";
      desc = "Emittente ufficiale delle Associazioni Mozart Italia nel mondo";
      tags = [tags.classical];
    }
    {
      stream = "http://onair7.xdevel.com:7126/1/";
      station = "Opera Radio Budapest";
      logo = "https://www.opera.hu/static/default/asset/img/common/opera-logo.svg";
      tags = [tags.classical];
    }
    {
      stream = "http://peacefulpiano.stream.publicradio.org/peacefulpiano.mp3";
      station = "Peaceful Piano";
      tags = [tags.classical];
    }
    {
      logo = "https://cdn.promodj.com/afs/11a5f0be108d5f48084aac34ec54da9f11:e598f2";
      stream = "https://radio.promodj.com/fullmoon";
      station = "Trance | PromoDJ";
      tags = [tags.trance];
    }
    {
      stream = "http://worship.lobpreisradio.de:8000/anbetung-lobpreis-radio-37k-ogg-stereo";
      station = "Lobpreisradio";
      desc = "Aufladen mit Lobpreis und Anbetung über Lobpreisradio, täglich auch Evangelium und Predigten sowie die Worship-Hits. Wir wollen Herzen von Gott berühren vor allem mit deutschen aber auch englischem Lobpreis";
      tags = [tags.holy];
    }
    {
      stream = stereoscenic "mod-h";
      station = stereoscenic-name "Ambient Modern";
      tags = [tags.ambient];
    }
    {
      stream = stereoscenic "asp-h";
      station = stereoscenic-name "Ambient Sleeping Pill";
      tags = [tags.ambient];
    }
    {
      stream = stereoscenic "ama-h";
      station = stereoscenic-name "A. M. Ambient";
      tags = [tags.ambient];
    }
    {
      stream = "http://1a-schlagergold.radionetz.de/1a-schlagergold.aac";
      station = "1A Schlagergold";
      tags = [tags.schlager];
    }
    {
      stream = "http://streams.freetalklive.com:8000/";
      station = "Free Talk Live";
      logo = "https://upload.wikimedia.org/wikipedia/en/8/8d/FreeTalkLive_Logo.png";
      desc = "Talk Radio You Control";
      tags = [tags.text];
    }
    {
      stream = "https://listen1.outpostradio.com/omagic";
      station = "Organ Magic";
      desc = "Pipe Organ music 24/7. An Outpost Radio station.";
      logo = "https://outpostradio.com/organmagic/organ-magic-1-web.jpg";
      tags = [tags.classical];
    }
    {
      stream = "http://79.120.77.11:8002/poetryru";
      station = "Стихи";
      desc = "Russian poetry";
      tags = [tags.text tags.russian];
    }
    {
      stream = "http://listen.radiopartywelle.com:8000";
      station = "Radio Partywelle";
      logo = "https://www.radiopartywelle.com/wp-content/uploads/2020/11/RPW-HAUPTLOGO-einfach-gute-Laune-1.png";
      desc = "... einfach gute Laune!";
      tags = [tags.schlager];
    }
    {
      stream = paloma "RP-Fresh";
      station = paloma-name "Fresh";
      logo = paloma-logo;
      tags = [tags.schlager];
    }
    {
      stream = paloma "RP-Kultschlager";
      station = paloma-name "Kultschlager";
      logo = paloma-logo;
      tags = [tags.schlager];
    }
    {
      stream = paloma "RP-Kuschelschlager";
      station = paloma-name "Kuschelschlager";
      logo = paloma-logo;
      tags = [tags.schlager];
    }
    {
      stream = paloma "RP-Partyschlager";
      station = paloma-name "Partyschlager";
      logo = paloma-logo;
      tags = [tags.schlager];
    }
    {
      stream = paloma "RP-Volksmusik";
      station = paloma-name "Volksmusik";
      logo = paloma-logo;
      tags = [tags.schlager];
    }
    {
      stream = paloma "RADIOPALOMA";
      station = paloma-name "Live";
      logo = paloma-logo;
      tags = [tags.schlager];
    }
    {
      stream = "http://91.121.59.45:8013/autodj";
      station = "Feeling Floyd | AutoDJ";
      tags = [tags.vintage];
    }
    {
      stream = "http://91.121.59.45:8013/live";
      station = "Feeling Floyd | Live";
      tags = [tags.vintage];
    }
    {
      station = "Chinese Music World";
      stream = "https://radio.chinesemusicworld.com/chinesemusic.ogg";
      tags = [tags.chinese tags.trad];
    }
    {
      stream = "https://live.hunter.fm/lofi_high";
      station = "LoFi | Hunter FM";
      tags = [tags.lofi];
    }
    {
      stream = "https://live.hunter.fm/pisadinha_high";
      station = "Pisadinha | Hunter FM";
      tags = [tags.brazilian];
    }
    {
      stream = "https://classicfm.ice.infomaniak.ch/classic-fm.mp3";
      station = "Classic FM";
      tags = [tags.classical];
    }
    {
      station = "Digital Jazz";
      stream = "http://stm01.virtualcast.com.br:8190/live";
      tags = [tags.jazz];
    }
    {
      station = "Jazz | Radio Monte Carlo";
      stream = "http://edge.radiomontecarlo.net/rmcweb019";
      tags = [tags.jazz];
    }
    {
      station = "NRK Jazz";
      stream = "http://lyd.nrk.no/nrk_radio_jazz_aac_h";
      tags = [tags.jazz];
    }
    {
      station = "Digital Impulse – Classical Channel";
      stream = "http://orion.shoutca.st:8978/stream";
      tags = [tags.classical];
    }
    {
      station = "Старое радио (детское)";
      stream = "http://195.91.237.50:8000/detskoe128";
      tags = [tags.russian tags.text tags.vintage];
    }
    {
      station = "Старое радио";
      stream = "http://195.91.237.50:8000/ices128";
      tags = [tags.russian tags.text tags.vintage];
    }
    {
      station = "Старое радио (музыка)";
      stream = "http://195.91.237.50:8000/music128";
      tags = [tags.russian tags.vintage];
    }
    {
      station = "Fango Radio";
      stream = "https://azuracast.streams.ovh/radio/8090/radio.mp3";
      tags = [tags.greek];
    }
    {
      station = "Mikis Radio";
      stream = "http://radio.hostchefs.net:8046/stream";
      tags = [tags.greek];
    }
  ]
/*
      (caster-fm "TODO" "noasrv" 10182) # https://github.com/cccruzr/albumsyoumusthear/blob/7e00baf575e4d357cd275d54d1aeb717321141a8/HLS/IBERO_90_1.m3u
      (caster-fm "TODO" "shaincast" 20866) # https://github.com/cccruzr/albumsyoumusthear/blob/7e00baf575e4d357cd275d54d1aeb717321141a8/HLS/IBERO_90_1.m3u

CNN news in morse code
http://cw.dimebank.com:8080/CNNslow
http://cw.dimebank.com:8080/CNNfast

Orchestral
http://orion.shoutca.st:8978/stream

LoFi / Chill
http://ice55.securenetsystems.net/DASH76

News background music
https://c13014-l-hls.u.core.cdn.streamfarm.net/1000153copo/hk2.m3u8

?
http://94.23.221.158:9163/stream

Greek radio
http://radio.hostchefs.net:8046/stream?1520818130148

: http://audiokrishna.com/stations/japa2.mp3
http://185.105.4.53:2339//;stream.mp3
http://cast5.servcast.net:1390/;stream.mp3

Hard rock
http://andromeda.shoutca.st:9254/stream

Rock alternative
http://icy.unitedradio.it/VirginRockAlternative.mp3

American nautical weather news
http://ca.radioboss.fm:8149/stream

Christian radio in all languages
https://jesuscomingfm.com/#
tamazight http://live.jesuscomingfm.com:8462/;
*/

