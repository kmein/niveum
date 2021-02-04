{ di-fm-key }:
# DI generated via: cat /tmp/di-fm.html| pup '.channel-tile-component json{}' | jq 'map({logo: .children[0].children[1].children[1].src | sub("^"; "http:"), station: .children[3].children[0].children[0].text, desc: .children[3].children[1].text | gsub("\\s+"; " ")})'  > /tmp/di-fm.json
# soma.fm generated via: curl https://somafm.com/ | pup '.cbshort json{}' | jq 'map({logo:.children[0].children[0].src|sub("^"; "http://soma.fm"), desc: .children[2].text, station: .children[1].text})'
let
  di-fm-name = name: "${name} | DI.FM";
  di-fm = name: "http://prem2.di.fm/${name}_hi?${di-fm-key}";

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

  radiosai-name = name: "${name} | Radiosai";
  radiosai = name: "https://stream.sssmediacentre.org/${name}";

  royal-name = name: "${name} | RoyalRadio";
  royal = name: "http://193.33.170.218:8000/${name}";

  radiorecord-name = name: "${name} | Radio Record";
  radiorecord = name: "http://air.radiorecord.ru:8102/${name}_320";
  radiorecord-logo = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 144 46'%3E%3Cpath d='M68.3 24H79c8-11.4 29.8-18.8 41.7-24l-5.4 1.6-.3.2-15.3 4.6-3.1.9-17 5.3-3.4-3.1h-5.4l-3.5 3.2L43 5 32 1.6 26.6 0c12.3 5.4 35.1 14.5 41.7 24zm5.2-11.4c.9 0 1.6.3 2.2.8.6.5.9 1.1.9 1.9 0 .8-.4 2-1.1 3-.5.7-1.2 1.2-1.9 1.3-.7 0-1.3-.5-1.8-1.2-.7-.9-1.1-2-1.1-2.9v-.2c-.2-1.5 1.1-2.7 2.8-2.7zm70.3 17.5c-.5-1.3-2.4-1.5-4.1-1.5h-15.9l-.1.4-.1.2-4.8 16-.1.2-.2.7h16.8c2.8 0 4.6-1 5.5-3.6L143 35c1-2.6 1.2-3.9.8-4.9zm-10.2 11.7h-6.3l2.7-8.9h6.3l-2.7 8.9zM94.5 29.1c-.8-.4-2-.5-3.1-.5H79.9c-2.8 0-4.6 1-5.5 3.6l-2.2 7.5c-.6 2-1.8 4.8.3 5.9.8.4 2 .5 3.1.5h11.5c2.8 0 4.6-1 5.5-3.6l2.2-7.5c.6-2 1.8-4.9-.3-5.9zm-9.2 12.7h-6.4l2.7-8.9H88l-2.7 8.9zm-39.8 0H32.8l.7-2.3h10.9l.1-.4.1-.2.9-2.9.1-.2.2-.7h-11l.7-2.3h13.8l.1-.4.1-.2.8-2.8.1-.2.2-.7H29.3l-.1.4-.2.2-4.8 16-.1.2-.1.7h21.2l.1-.4.1-.2.8-2.8.1-.2.2-.7h-1zm70.2-13.3H99.2c-1.6 5.5-3.3 11-4.9 16.6l-.1.2-.3.7h7.5l.1-.4.1-.2 1.7-5.6h2.5l2.7 5.7.1.1.1.3h8.2l-3.3-6.6c3.4-.4 4.2-1.8 4.8-4.2.2-.6.3-1.2.5-1.7 1.2-3.5-.1-4.8-3.2-4.9zm-4.3 5.1L111 35c-.1.4-.3.5-.7.6l-6.4 1.7 1.4-4.5h5.5c.6 0 .8.2.6.8zm-86.9 1.6c.2-.6.3-1.2.5-1.7 1.2-3.6-.1-4.9-3.2-4.9H5.3C3.7 34.1 2 39.6.4 45.2l-.1.2-.3.6h7.4l.1-.4.1-.2 1.7-5.6h2.5l2.7 5.7.1.1.1.3h8.2l-3.3-6.6c3.5-.3 4.3-1.7 4.9-4.1zm-7-1.6l-.4 1.4c-.1.4-.3.5-.7.6L10 37.3l1.4-4.5h5.5c.6 0 .8.2.6.8zM71.1 35l1.2-3.8c.5-1.3-.4-2.8-1.9-2.7H57c-2.8 0-4.6 1-5.5 3.6l-2.2 7.5c-.6 2-1.8 4.8.3 5.9.8.4 2 .5 3.1.5h12.5c1.5 0 3.2-1.4 3.6-2.7l1-3.7h-5.7l-.6 2.1h-7.3l2.7-8.9h7l-.8 2.2h6z' fill-rule='evenodd' clip-rule='evenodd' fill='%23fff'/%3E%3C/svg%3E";

  caster-fm = subdomain: port: "http://${subdomain}.caster.fm:${toString port}/listen.mp3?authn0b0236758bd0e178156d0787327a055d";
in [
  {
    stream = "https://radio.lassul.us/radio.ogg";
    station = "Radio lassulus";
    logo = "http://lassul.us/art/portraits/selbstportrait.jpg";
    desc = "Diminutive from lassus (“weary, faint, tired”). A programming human. Doing superior professional art.";
  }
  {
    stream = "http://radio.xn--kiern-0qa.de/meddl/listen.ogg";
    station = "Meddl";
    logo = "https://antenne-asb.ga/logo.png";
    desc = "Forked from antenne-asb.ga: Radiosender zum Youtuber Drachenlord. Hier läuft den ganzen Tag Drachenlord Musik von Haidern oder Podcasts zum Lord.";
  }
  {
    stream = "http://radio.xn--kiern-0qa.de/lyrik/listen.ogg";
    station = "Lyrik";
    desc = "Lyrik-Lesung rund um die Uhr.";
  }
  {
    stream = "http://radio.xn--kiern-0qa.de/lyrikline/listen.ogg";
    station = "Lyrikline";
    logo = "https://www.lyrikline.org/themes/lyrik/svg/Logo_lyrikline_pure.svg";
    desc = "24/7 zufällige Wiedergaben von lyrikline.org.";
  }
  {
    stream = "http://1.ice1.firststreaming.com/kkjz_fm.aac";
    station = "KJazz 88.1";
    logo = "https://cdn-radiotime-logos.tunein.com/s37062q.png";
    desc = "KKJZ 88.1 FM offers the full spectrum of jazz music, from bop to cool, Latin to straight-ahead, swing to big band, and most everything in between.";
  }
  {
    stream = "http://162.244.80.20:6948";
    station = "Cool Jazz Florida";
    logo = "https://d3kle7qwymxpcy.cloudfront.net/images/broadcasts/d1/30/24209/c300.png";
    desc = "Sit Back and Relax and Enjoy the Sophisticated Cool Jazz on Cool Jazz Florida!";
  }
  {
    stream = "https://stream1.mfm.plexpark.com/radio/8000/simulcast-berlin-sd-64.aac";
    station = "Metropol FM Berlin";
    logo = "https://www.metropolfm.de/wp-content/uploads/2017/04/berlin.png";
  }
  {
    stream = "http://rb-stream.de:8000/rrb_128.mp3";
    station = "Radio Russkij Berlin";
    logo = "http://radio-rb.de/img/site/logo.png";
    desc = "Голос нашего города ...";
  }
  {
    desc = "Your favorite dance tunes from the start of the decade. Familiar hits and overlooked classics in abundance.";
    logo = "http://cdn-images.audioaddict.com/1/4/0/3/5/b/14035b0944a3c2e77852b6d0944f381e.jpg?size=180x180";
    station = di-fm-name "00s Club Hits";
    stream = di-fm "00sclubhits";
  }
  {
    desc = "Electronic sounds and atmospheric textures create a genre to enhance your state of mind and take you deeper.";
    logo = "http://cdn-images.audioaddict.com/a/9/4/6/2/f/a9462ff46233f40fe0aa306379ae7cd8.jpg?size=180x180";
    station = di-fm-name "Ambient";
    stream = di-fm "ambient";
  }
  {
    desc = "Spaced out, melodic and full of warmth – these broken beat dance tunes will keep you dazed and amused.";
    logo = "http://cdn-images.audioaddict.com/3/9/7/3/4/3/397343ffcf5543b60bba72f393e1c3c0.jpg?size=180x180";
    station = di-fm-name "Atmospheric Breaks";
    stream = di-fm "atmosphericbreaks";
  }
  {
    desc = "From the funkiest grooves to the dirtiest beats. Hard-hitting, high energy 4/4 club cuts to move the masses.";
    logo = "http://cdn-images.audioaddict.com/4/f/e/d/8/9/4fed8974f4cfbbd0712baa8ae2cc2ed2.jpg?size=180x180";
    station = di-fm-name "Bass & Jackin' House";
    stream = di-fm "bassnjackinhouse";
  }
  {
    desc = "Blending together elements of house music, speed garage, and techno – it's all about the low end frequencies.";
    logo = "http://cdn-images.audioaddict.com/9/1/9/9/6/d/91996d1fed5b9607c625069bcb8d3b52.jpg?size=180x180";
    station = di-fm-name "Bassline";
    stream = di-fm "bassline";
  }
  {
    desc = "Heavily focused on breakbeats and dusty samples. A defining 90s musical movement still going strong today.";
    logo = "http://cdn-images.audioaddict.com/6/f/4/3/2/4/6f4324c4a776101fd91008d739ac3020.jpg?size=180x180";
    station = di-fm-name "Big Beat";
    stream = di-fm "bigbeat";
  }
  {
    desc = "Fusing together house elements from the past and the present – prime time music full of uplifting high energy.";
    logo = "http://cdn-images.audioaddict.com/0/4/1/2/5/7/0412578b5e31cf1b68f23c3cb5377139.jpg?size=180x180";
    station = di-fm-name "Big Room House";
    stream = di-fm "bigroomhouse";
  }
  {
    desc = "Inspired by hip hop and UK rave music, breaks features broken up drum loops and creative samples, synths and fx.";
    logo = "http://cdn-images.audioaddict.com/2/1/b/1/2/9/21b12909a0618017285f62a5af4b2ce5.jpg?size=180x180";
    station = di-fm-name "Breaks";
    stream = di-fm "breaks";
  }
  {
    desc = "The sounds of Chill & Tropical House are expertly made for lounging and dancing alike with its deeper house vibes.";
    logo = "http://cdn-images.audioaddict.com/2/9/0/1/e/f/2901ef5c923bb0f9ff856ac4007975f0.jpg?size=180x180";
    station = di-fm-name "Chill & Tropical House";
    stream = di-fm "chillntropicalhouse";
  }
  {
    desc = "Good EDM doesn't have to blow out your speakers and our curated selection of Chill EDM is a testament to the strength of mellow, chilled out electronic music.";
    logo = "http://cdn-images.audioaddict.com/f/0/7/5/2/e/f0752e6df86dc7a0f3ffd7a63497df4e.jpg?size=180x180";
    station = di-fm-name "Chill EDM";
    stream = di-fm "chilledm";
  }
  {
    desc = "Mellow chill beats, lofi hip-hop, trip hop, downtempo beats and jazz, blended together in a laid back style for perfect listening.";
    logo = "http://cdn-images.audioaddict.com/f/2/3/8/8/b/f2388be364717a3aa33f62411d9e3585.jpg?size=180x180";
    station = di-fm-name "ChillHop";
    stream = di-fm "chillhop";
  }
  {
    desc = "Electronic sounds, mellow mid-tempo rhythms, and a groove meant to calm the senses and ease the mind.";
    logo = "http://cdn-images.audioaddict.com/f/d/9/6/c/a/fd96ca5c52508a2755a266ebf506f162.jpg?size=180x180";
    station = di-fm-name "Chillout";
    stream = di-fm "chillout";
  }
  {
    desc = "The perfect musical soundtrack for when you want to close your eyes, get truly comfortable, and drift away.";
    logo = "http://cdn-images.audioaddict.com/1/d/4/f/3/3/1d4f3310f94769b4e2f55ee0887eead3.jpg?size=180x180";
    station = di-fm-name "Chillout Dreams";
    stream = di-fm "chilloutdreams";
  }
  {
    desc = "The brilliant combination of dubstep rhythms with the mellow grooves of chillout. A unique sound all its own.";
    logo = "http://cdn-images.audioaddict.com/c/e/9/b/d/1/ce9bd1666b49921b440ec796653b24f0.jpg?size=180x180";
    station = di-fm-name "Chillstep";
    stream = di-fm "chillstep";
  }
  {
    desc = "European pop music born in the 90s full of high energy sounds and big hooks – now heard in gyms and malls worldwide.";
    logo = "http://cdn-images.audioaddict.com/a/9/8/1/5/3/a98153e6c9fcee321fd6dff0c8a6d0ba.jpg?size=180x180";
    station = di-fm-name "Classic EuroDance";
    stream = di-fm "classiceurodance";
  }
  {
    desc = "Conceived in the European discos in the 70s, evolving through the decades into modern electronic masterpieces.";
    logo = "http://cdn-images.audioaddict.com/0/1/0/a/6/6/010a6648f8afc52654b07c07c68e9cad.jpg?size=180x180";
    station = di-fm-name "Classic EuroDisco";
    stream = di-fm "classiceurodisco";
  }
  {
    desc = "The classic melodies, the epic breakdowns and gigantic builds. Re-experience Trance music in her prime.";
    logo = "http://cdn-images.audioaddict.com/4/6/8/0/a/3/4680a3fd0e35f0b2f9bf60c9889d4343.jpg?size=180x180";
    station = di-fm-name "Classic Trance";
    stream = di-fm "classictrance";
  }
  {
    desc = "Classic sounds of Vocal Trance";
    logo = "http://cdn-images.audioaddict.com/1/e/1/8/1/a/1e181a502369be86e3f2e696723c26fe.jpg?size=180x180";
    station = di-fm-name "Classic Vocal Trance";
    stream = di-fm "classicvocaltrance";
  }
  {
    desc = "The bassbin rattling, speaker-freaking hits of Dubstep – all tried, tested and approved to work in the clubs.";
    logo = "http://cdn-images.audioaddict.com/3/6/b/5/e/7/36b5e73f328251b20120a6bc5365777f.jpg?size=180x180";
    station = di-fm-name "Club Dubstep";
    stream = di-fm "clubdubstep";
  }
  {
    desc = "The music heard in the biggest venues worldwide. From prime time pushers to deeper house shakers – the sounds of now.";
    logo = "http://cdn-images.audioaddict.com/2/e/e/f/9/8/2eef98b26e5490acc78ff6ab22e04827.jpg?size=180x180";
    station = di-fm-name "Club Sounds";
    stream = di-fm "club";
  }
  {
    desc = "From techno, deep house, progressive and trance – check out the sounds of the DJ deep in the mix.";
    logo = "http://cdn-images.audioaddict.com/1/3/a/a/0/8/13aa08b5dce2525029c6ddfb8e286dc2.jpg?size=180x180";
    station = di-fm-name "DJ Mixes";
    stream = di-fm "djmixes";
  }
  {
    desc = "Evil, gritty and twisted DnB / Drum & Bass. at 160+ BPM, hear the darkest basslines and the hardest hitting percussion.";
    logo = "http://cdn-images.audioaddict.com/6/e/4/7/c/1/6e47c1d85f09957d568c9535dda75e58.jpg?size=180x180";
    station = di-fm-name "Dark DnB";
    stream = di-fm "darkdnb";
  }
  {
    desc = "The darker form of PsyTrance, which is a sound all its own – direct from Goa to your headphones.";
    logo = "http://cdn-images.audioaddict.com/0/e/1/0/b/9/0e10b950ca2f3e828becbe3dd3c2d0b3.jpg?size=180x180";
    station = di-fm-name "Dark PsyTrance";
    stream = di-fm "darkpsytrance";
  }
  {
    desc = "House music crafted for the smaller and mid-sized rooms – deeper tracks full of silky, smooth grooves.";
    logo = "http://cdn-images.audioaddict.com/9/a/1/b/4/6/9a1b469ae251c084465096038312d506.jpg?size=180x180";
    station = di-fm-name "Deep House";
    stream = di-fm "deephouse";
  }
  {
    desc = "Elements of house, funk, and disco. Mid-tempo beats, soulful grooves and head nodding selections.";
    logo = "http://cdn-images.audioaddict.com/b/2/a/7/1/5/b2a715bb711ab06822bcaf07fde74d51.jpg?size=180x180";
    station = di-fm-name "Deep Nu-Disco";
    stream = di-fm "deepnudisco";
  }
  {
    desc = "This smooth, groove-heavy selection of deep progressive house tracks is the perfect soundtrack for smaller and mid-sized rooms.";
    logo = "http://cdn-images.audioaddict.com/b/0/e/1/9/0/b0e19018c34150a07d76224e5fcdbeda.jpg?size=180x180";
    station = di-fm-name "Deep Progressive House";
    stream = di-fm "deepprogressivehouse";
  }
  {
    desc = "A fusion of deep house & techno. Punchy grooves, spaced out sounds and forward thinking productions.";
    logo = "http://cdn-images.audioaddict.com/d/3/2/1/7/5/d32175a432f823ce84261c01d4b53f57.jpg?size=180x180";
    station = di-fm-name "Deep Tech";
    stream = di-fm "deeptech";
  }
  {
    desc = "Where would dance music be without Detroit? The city that started it all continues to inspire and educate.";
    logo = "http://cdn-images.audioaddict.com/d/7/b/3/1/d/d7b31d5973593af0043fed3f180df702.jpg?size=180x180";
    station = di-fm-name "Detroit House & Techno";
    stream = di-fm "detroithousentechno";
  }
  {
    desc = "The feel good sound inspired from 70s disco combined with the warm kick drum of modern house music.";
    logo = "http://cdn-images.audioaddict.com/2/7/9/7/0/5/279705ae85a4e0f529f6f7fbaa47a646.jpg?size=180x180";
    station = di-fm-name "Disco House";
    stream = di-fm "discohouse";
  }
  {
    desc = "Head nodding beats, chilled vocals, and lush soundscapes to bring down the sun and start the night.";
    logo = "http://cdn-images.audioaddict.com/f/8/3/3/6/f/f8336fff9bcb4b01a36f2684ecd150c6.jpg?size=180x180";
    station = di-fm-name "Downtempo Lounge";
    stream = di-fm "downtempolounge";
  }
  {
    desc = "Born in the mid 90s, Drum and Bass / DnB is all about fast breakbeats, urban vibes, and rib rattling basslines.";
    logo = "http://cdn-images.audioaddict.com/1/9/8/1/3/f/19813f3ba29ad66caa06bbee4aba558a.jpg?size=180x180";
    station = di-fm-name "Drum and Bass";
    stream = di-fm "drumandbass";
  }
  {
    desc = "A hybrid of half-time Dubstep and intense Drum and Bass / DnB.";
    logo = "http://cdn-images.audioaddict.com/1/c/6/0/b/9/1c60b9976b4d861ddc90d668f749fe6f.jpg?size=180x180";
    station = di-fm-name "Drumstep";
    stream = di-fm "drumstep";
  }
  {
    desc = "An emphasis on the bass and drums / DnB, delayed effects, sampled vocals and smokey Reggae inspired vibes.";
    logo = "http://cdn-images.audioaddict.com/e/2/c/8/6/5/e2c865e92c65cfb91ad0e2d3933234e0.jpg?size=180x180";
    station = di-fm-name "Dub";
    stream = di-fm "dub";
  }
  {
    desc = "The beloved sounds of deep techno saturated with tape delays, heavy reverb and ice cold atmospherics.";
    logo = "http://cdn-images.audioaddict.com/1/9/d/6/1/0/19d61084830ef94886b32d847fc5d29e.jpg?size=180x180";
    station = di-fm-name "Dub Techno";
    stream = di-fm "dubtechno";
  }
  {
    desc = "The wobbles of the bass, the party rocking beats, and the biggest crowd destroying drops.";
    logo = "http://cdn-images.audioaddict.com/7/7/c/1/a/e/77c1aec493eeb9e2a5d22951447fbd48.jpg?size=180x180";
    station = di-fm-name "Dubstep";
    stream = di-fm "dubstep";
  }
  {
    desc = "The sound of the largest events. From the gargantuan festivals, the huge main rooms and the biggest DJs.";
    logo = "http://cdn-images.audioaddict.com/d/d/1/1/8/f/dd118f5fe9befc191907a32d0877a13d.jpg?size=180x180";
    station = di-fm-name "EDM Festival";
    stream = di-fm "edmfestival";
  }
  {
    desc = "Where dance meets pop. Crossover favorites, stadium-sized anthems and the biggest electronic tunes in existence.";
    logo = "http://cdn-images.audioaddict.com/6/9/b/2/c/c/69b2cc01d4cbcb5a813ee6428bc4e455.jpg?size=180x180";
    station = di-fm-name "EDM Hits";
    stream = di-fm "edm";
  }
  {
    desc = "Buzzing basslines, huge kicks, party rocking drops. House music packed full of gigantic bass and massive synths.";
    logo = "http://cdn-images.audioaddict.com/7/4/2/3/8/0/742380673147770eef642532828dbc6c.jpg?size=180x180";
    station = di-fm-name "Electro House";
    stream = di-fm "electrohouse";
  }
  {
    desc = "The combination of 1920s-1940s jazz and swing music, big band horns and modern day electro house.";
    logo = "http://cdn-images.audioaddict.com/3/3/e/6/0/9/33e609d64ca20b0719d28d2193eece31.jpg?size=180x180";
    station = di-fm-name "Electro Swing";
    stream = di-fm "electroswing";
  }
  {
    desc = "The trailblazers, the renegades and the experimental musicians who gave early inspiration with electronic instruments.";
    logo = "http://cdn-images.audioaddict.com/5/7/4/a/3/3/574a3373b242bed1018c5c99e5021c3f.jpg?size=180x180";
    station = di-fm-name "Electronic Pioneers";
    stream = di-fm "electronicpioneers";
  }
  {
    desc = "Catchy pop music blended together with vintage synthesizers and electronic instrumentation.";
    logo = "http://cdn-images.audioaddict.com/2/8/c/1/e/8/28c1e8587ae6e0b2589b4d66ccd99324.jpg?size=180x180";
    station = di-fm-name "Electropop";
    stream = di-fm "electropop";
  }
  {
    desc = "Trance in its most boisterous form. Uplifting melodies on top of high energy beats create these euphoric anthems.";
    logo = "http://cdn-images.audioaddict.com/f/1/f/d/f/e/f1fdfea9ba9622ecdee020cc53126e60.jpg?size=180x180";
    station = di-fm-name "Epic Trance";
    stream = di-fm "epictrance";
  }
  {
    desc = "Pop music infused with a high energy 4/4 pulse. Heavy on the synthesizers, the melodies and the vocals.";
    logo = "http://cdn-images.audioaddict.com/4/b/b/2/7/5/4bb2756bcacce0a2bf44ebaaf743f2ab.jpg?size=180x180";
    station = di-fm-name "EuroDance";
    stream = di-fm "eurodance";
  }
  {
    desc = "Focused on the funkiest grooves, with plenty of the guitar licks and clever samples placed around a 4/4 swing.";
    logo = "http://cdn-images.audioaddict.com/3/3/7/0/b/a/3370baa3700e4578558dff57c6443a0f.jpg?size=180x180";
    station = di-fm-name "Funky House";
    stream = di-fm "funkyhouse";
  }
  {
    desc = "Hard basslines, booming beats and insatiable grooves. Inspired by Trap, Juke and Garage – molded together into a unique booming style.";
    logo = "http://cdn-images.audioaddict.com/3/9/e/5/8/b/39e58b7b675d6ec353582cd5e9f38dbe.jpg?size=180x180";
    station = di-fm-name "Future Bass";
    stream = di-fm "futurebass";
  }
  {
    desc = "2step Garage rhythms, chunky bass line driven grooves and plenty of forward thinking innovation.";
    logo = "http://cdn-images.audioaddict.com/0/e/9/0/4/2/0e904268ec98b8c0521c30da4ac321bb.jpg?size=180x180";
    station = di-fm-name "Future Garage";
    stream = di-fm "futuregarage";
  }
  {
    desc = "Finest selection of futurepop and synthpop.";
    logo = "http://cdn-images.audioaddict.com/e/e/8/5/2/3/ee8523a0d6165465961cb2d0445c4f27.jpg?size=180x180";
    station = di-fm-name "Future Synthpop";
    stream = di-fm "futuresynthpop";
  }
  {
    desc = "The hardest form of techno with punishing tracks designed to drive the crowds into a sweaty frenzy.";
    logo = "http://cdn-images.audioaddict.com/d/a/f/c/e/d/dafced37829e69724be99dc375bc5713.jpg?size=180x180";
    station = di-fm-name "Gabber";
    stream = di-fm "gabber";
  }
  {
    desc = "The sound of digital malfunctions, electric hum and bit rate distortions perfectly placed alongside laid-back hip hop beats.";
    logo = "http://cdn-images.audioaddict.com/7/3/5/a/b/b/735abb160f950c98e2bd7caf6843e255.jpg?size=180x180";
    station = di-fm-name "Glitch Hop";
    stream = di-fm "glitchhop";
  }
  {
    desc = "A very psychedelic form of trance, Goa-Psy Trance is a sound full of arpeggiated synths and trippy effects.";
    logo = "http://cdn-images.audioaddict.com/4/e/e/e/4/4/4eee448424df2768055ee7503f0251e9.jpg?size=180x180";
    station = di-fm-name "Goa-Psy Trance";
    stream = di-fm "goapsy";
  }
  {
    desc = "A channel showcasing everything from hard dance, trance and happy hardcore to lift the spirits (and the arms).";
    logo = "http://cdn-images.audioaddict.com/d/f/7/7/8/e/df778eed294536f4e858093124aa2be5.jpg?size=180x180";
    station = di-fm-name "Hands Up";
    stream = di-fm "handsup";
  }
  {
    desc = "Concrete kicks and punching rhythms, hard dance is a tougher side of music with sharp edges and aggressive power.";
    logo = "http://cdn-images.audioaddict.com/5/9/d/a/e/5/59dae529a51feb3f90b542f3d33808ec.jpg?size=180x180";
    station = di-fm-name "Hard Dance";
    stream = di-fm "harddance";
  }
  {
    desc = "Tough as nails warehouse jams full of cold aggression, sinister structures and pounding rhythms that hit hard.";
    logo = "http://cdn-images.audioaddict.com/5/e/5/1/1/d/5e511d67198b404149e7eaed9858e5d4.jpg?size=180x180";
    station = di-fm-name "Hard Techno";
    stream = di-fm "hardtechno";
  }
  {
    desc = "Strictly for the hardcore. These are the biggest and boldest bangers, and the hardest hitting tracks.";
    logo = "http://cdn-images.audioaddict.com/e/0/5/3/8/5/e05385cf844ef4884a26c19ed254efbf.jpg?size=180x180";
    station = di-fm-name "Hardcore";
    stream = di-fm "hardcore";
  }
  {
    desc = "Hard techno & hardcore. A global phenomenon with powerful kicks, distorted effects and infectious melodies.";
    logo = "http://cdn-images.audioaddict.com/8/b/1/0/3/c/8b103c03fbf78becf042085552e9ef41.jpg?size=180x180";
    station = di-fm-name "Hardstyle";
    stream = di-fm "hardstyle";
  }
  {
    desc = "Born in Chicago and now global, house music is always evolving but remains true to its pure 4/4 structure.";
    logo = "http://cdn-images.audioaddict.com/5/3/0/b/6/9/530b699645ddff8d66a2333ae09bb06a.jpg?size=180x180";
    station = di-fm-name "House";
    stream = di-fm "house";
  }
  {
    desc = "Smooth, groovy and full of cutting-edge, fresh ideas – beats to kick back and enjoy far from the club setting.";
    logo = "http://cdn-images.audioaddict.com/9/1/8/4/2/b/91842b0ec15b8b69e50315dbd3afe03e.jpg?size=180x180";
    station = di-fm-name "Indie Beats";
    stream = di-fm "indiebeats";
  }
  {
    desc = "The spirit of Rock & Roll with an electronic soul. Club culture and live music combined.";
    logo = "http://cdn-images.audioaddict.com/9/f/7/0/a/d/9f70ad76ec13a6123405c6d7a03325f3.jpg?size=180x180";
    station = di-fm-name "Indie Dance";
    stream = di-fm "indiedance";
  }
  {
    desc = "One of the biggest cultural soundtracks with the infectious thump of house music. Expect sultry saxophones, trumpets, and finger snapping grooves.";
    logo = "http://cdn-images.audioaddict.com/f/4/b/3/3/e/f4b33e8cbd9f0b1776e64f17b9c35abd.jpg?size=180x180";
    station = di-fm-name "Jazz House";
    stream = di-fm "jazzhouse";
  }
  {
    desc = "Jungle keeps the breakbeat tempos high and celebrates the diverse ideas found within urban and rave music.";
    logo = "http://cdn-images.audioaddict.com/4/1/e/d/e/7/41ede7b8b43cffcc42876e6e319b7ef3.jpg?size=180x180";
    station = di-fm-name "Jungle";
    stream = di-fm "jungle";
  }
  {
    desc = "The sounds of Salsa, Brazilian beats and Latin Jazz with the steady grooves of modern East Coast dance music.";
    logo = "http://cdn-images.audioaddict.com/6/0/1/7/6/e/60176ec05138ad9b621cb92de7f0c8c8.jpg?size=180x180";
    station = di-fm-name "Latin House";
    stream = di-fm "latinhouse";
  }
  {
    desc = "Smooth as water, with the fast paced rhythms, Liquid DNB / Drum and Bass flows with rolling ease without losing momentum.";
    logo = "http://cdn-images.audioaddict.com/6/7/5/9/9/a/67599a0b4f3575ea5f6a74fe633899d7.jpg?size=180x180";
    station = di-fm-name "Liquid DnB";
    stream = di-fm "liquiddnb";
  }
  {
    desc = "Smooth, rolling and steady – this fresh formation of Dubstep keeps the sounds you love with a flowing Drum and Bass groove.";
    logo = "http://cdn-images.audioaddict.com/6/e/f/6/c/e/6ef6ced4249faec61c8fbc58b10b6343.jpg?size=180x180";
    station = di-fm-name "Liquid Dubstep";
    stream = di-fm "liquiddubstep";
  }
  {
    desc = "The smoother side of Trap but still packed with mechanical grooves and hip hop moods.";
    logo = "http://cdn-images.audioaddict.com/6/b/5/b/d/6/6b5bd66a99e46fa1258cb565d988ea7c.jpg?size=180x180";
    station = di-fm-name "Liquid Trap";
    stream = di-fm "liquidtrap";
  }
  {
    desc = "Tastefully selected LoFi Hip-Hop tunes with textured atmospheres & laid back beats – with a dash of chillhop and perfectly designed to chill your ears.";
    logo = "http://cdn-images.audioaddict.com/c/f/7/0/d/a/cf70daf883c01687f321d72376227493.jpg?size=180x180";
    station = di-fm-name "LoFi Hip-Hop";
    stream = di-fm "lofihiphop";
  }
  {
    desc = "Punch your one-way ticket to peace of mind and mental clarity with this curated selection of LoFi Lounge & Chill tracks today.";
    logo = "http://cdn-images.audioaddict.com/0/2/7/2/1/f/02721f4098dbeae07a8956d832f59cd8.jpg?size=180x180";
    station = di-fm-name "LoFi Lounge & Chill";
    stream = di-fm "lofiloungenchill";
  }
  {
    desc = "Music to chill to. Music made for when it's all about kicking off your shoes, laying back, and totally relaxing.";
    logo = "http://cdn-images.audioaddict.com/f/7/a/5/f/b/f7a5fbb67a1a0f0992f131506917c2e6.jpg?size=180x180";
    station = di-fm-name "Lounge";
    stream = di-fm "lounge";
  }
  {
    desc = "The melodic side of progressive house, packed with driving rhythms and forward thinking sounds.";
    logo = "http://cdn-images.audioaddict.com/4/4/8/c/f/6/448cf62c2d3c68ede713008015da9bfc.jpg?size=180x180";
    station = di-fm-name "Melodic Progressive";
    stream = di-fm "melodicprogressive";
  }
  {
    desc = "Minimal fuses elements of house, techno and electronica and strips it back to focus on the spaces between the sound.";
    logo = "http://cdn-images.audioaddict.com/a/9/1/b/1/4/a91b1414a8712794672e3ea1324ffe92.jpg?size=180x180";
    station = di-fm-name "Minimal";
    stream = di-fm "minimal";
  }
  {
    desc = "Pitched up vocals, happy hardcore beats, and high energy music non-stop.";
    logo = "http://cdn-images.audioaddict.com/e/f/8/f/b/e/ef8fbe63f86e496f0ce514ee2e85c30a.jpg?size=180x180";
    station = di-fm-name "Nightcore";
    stream = di-fm "nightcore";
  }
  {
    desc = "Modern disco music blending the familiar funk of the 70s and 80s with futuristic beats and up to date grooves.";
    logo = "http://cdn-images.audioaddict.com/f/2/3/7/8/a/f2378a19d61a3063a2a6271a29fb1595.jpg?size=180x180";
    station = di-fm-name "Nu Disco";
    stream = di-fm "nudisco";
  }
  {
    desc = "Acid, one of the characteristics of the TB-303, is celebrated here with the best tracks from house, techno and trance.";
    logo = "http://cdn-images.audioaddict.com/7/0/a/7/8/3/70a7830bce8d22eb1a7c868773f443e8.jpg?size=180x180";
    station = di-fm-name "Oldschool Acid";
    stream = di-fm "oldschoolacid";
  }
  {
    desc = "The biggest classics and secret weapons – this is a true treasure chest of house tracks from back in the day.";
    logo = "http://cdn-images.audioaddict.com/7/d/0/c/e/f/7d0cefbcb479ce257b24471f3d600eba.jpg?size=180x180";
    station = di-fm-name "Oldschool House";
    stream = di-fm "oldschoolhouse";
  }
  {
    desc = "Grab your whistles, white gloves and reach for the laser beams. This is the sound of raving when raving was new.";
    logo = "http://cdn-images.audioaddict.com/b/1/0/c/2/6/b10c2672a6a54a8ae5356f3ddb49b4c1.jpg?size=180x180";
    station = di-fm-name "Oldschool Rave";
    stream = di-fm "oldschoolrave";
  }
  {
    desc = "Go back in time and hear the biggest and best tracks within techno and trance that defined a decade of dance culture.";
    logo = "http://cdn-images.audioaddict.com/2/4/9/d/1/8/249d182058ac9e5631557eb309efe80f.jpg?size=180x180";
    station = di-fm-name "Oldschool Techno & Trance";
    stream = di-fm "oldschool";
  }
  {
    desc = "Always moving forward, progressive continues to reinvent itself into new sounds and styles made for the floor.";
    logo = "http://cdn-images.audioaddict.com/3/3/5/5/3/1/3355314492d633a5330c659cfe98fc1b.jpg?size=180x180";
    station = di-fm-name "Progressive";
    stream = di-fm "progressive";
  }
  {
    desc = "Progress your mind to undiscovered psychedelic dimensions.";
    logo = "http://cdn-images.audioaddict.com/5/f/a/5/6/5/5fa5659badbaf1a4ff817323ee5e998a.jpg?size=180x180";
    station = di-fm-name "Progressive Psy";
    stream = di-fm "progressivepsy";
  }
  {
    desc = "Downtempo psychedelic dub grooves, goa ambient, and world beats.";
    logo = "http://cdn-images.audioaddict.com/4/5/2/d/1/4/452d14ab72381941b1bd5f94af15678b.jpg?size=180x180";
    station = di-fm-name "PsyChill";
    stream = di-fm "psychill";
  }
  {
    desc = "Dub, ambient, and psychedelic trance, fused together in atmospheric harmony.";
    logo = "http://cdn-images.audioaddict.com/4/0/0/d/5/3/400d53d541322f0883a807c9b79d5540.jpg?size=180x180";
    station = di-fm-name "PsyDub";
    stream = di-fm "psydub";
  }
  {
    desc = "The psychedelic side of ambient.";
    logo = "http://cdn-images.audioaddict.com/a/7/b/0/d/c/a7b0dcee2110e6d01fec96758a639ea3.jpg?size=180x180";
    station = di-fm-name "Psybient";
    stream = di-fm "psybient";
  }
  {
    desc = "Russia's hottest club hits.";
    logo = "http://cdn-images.audioaddict.com/5/8/2/6/2/e/58262e338e405ab82ab92aa55f719f86.jpg?size=180x180";
    station = di-fm-name "Russian Club Hits";
    stream = di-fm "russianclubhits";
  }
  {
    desc = "House music saturated with feeling – full of melodies, vocals and true soul. Steady warm 4/4 vibes.";
    logo = "http://cdn-images.audioaddict.com/e/6/5/5/c/d/e655cd614bd4a1c981273a555081c309.jpg?size=180x180";
    station = di-fm-name "Soulful House";
    stream = di-fm "soulfulhouse";
  }
  {
    desc = "Ambient space music for expanding minds.";
    logo = "http://cdn-images.audioaddict.com/b/e/c/b/e/0/becbe0cb9c3002fc21f97f9e65cf9da1.jpg?size=180x180";
    station = di-fm-name "Space Dreams";
    stream = di-fm "spacemusic";
  }
  {
    desc = "This selection of summer chill house classics has been handpicked to elicit that special summer feeling year-round.";
    logo = "http://cdn-images.audioaddict.com/3/d/8/b/6/1/3d8b619652faa1969274e0d51c8bd59d.jpg?size=180x180";
    station = di-fm-name "Summer Chill House";
    stream = di-fm "summerchillhouse";
  }
  {
    desc = "Influenced by video games and movie soundtracks of the 80s, Synthwave's mission continues today with great new music keeping things future retro.";
    logo = "http://cdn-images.audioaddict.com/d/8/e/a/6/4/d8ea647113c8cdec87b4751f20b3360b.jpg?size=180x180";
    station = di-fm-name "Synthwave";
    stream = di-fm "synthwave";
  }
  {
    desc = "Blending the warmth of house music with the cold structural precision of techno, tech house bridges the divide.";
    logo = "http://cdn-images.audioaddict.com/2/7/b/a/7/0/27ba70234566a810d9dd33745195f088.jpg?size=180x180";
    station = di-fm-name "Tech House";
    stream = di-fm "techhouse";
  }
  {
    desc = "Techno is a true musical force full of structure and style. Robotic, mechanical and full of soul, always facing the future.";
    logo = "http://cdn-images.audioaddict.com/7/a/3/1/4/a/7a314a3ff87e31013172e9099d9aa843.jpg?size=180x180";
    station = di-fm-name "Techno";
    stream = di-fm "techno";
  }
  {
    desc = "Emotive dance music which embraces incredible melodies, future-facing production and energetic anthems heard worldwide.";
    logo = "http://cdn-images.audioaddict.com/b/1/0/5/6/7/b10567777ad265dcc63816fa32396654.jpg?size=180x180";
    station = di-fm-name "Trance";
    stream = di-fm "trance";
  }
  {
    desc = "Born out of Southern Hip-Hop and influenced by techno, trap is analog drum machines / DnB & with hip-hop aesthetics.";
    logo = "http://cdn-images.audioaddict.com/8/8/6/e/b/2/886eb22e09893237ff5a851991543e4b.jpg?size=180x180";
    station = di-fm-name "Trap";
    stream = di-fm "trap";
  }
  {
    desc = "The percussive side of the house and tech house scene, tribal house takes drums and puts them in the forefront.";
    logo = "http://cdn-images.audioaddict.com/5/9/d/5/b/0/59d5b064c1bc9e165850a8d9371d32a5.jpg?size=180x180";
    station = di-fm-name "Tribal House";
    stream = di-fm "tribalhouse";
  }
  {
    desc = "UMF Radio 24/7";
    logo = "http://cdn-images.audioaddict.com/b/4/5/7/b/9/b457b9fe45596e28a7c4b7b004c82ce4.jpg?size=180x180";
    station = di-fm-name "UMF Radio";
    stream = di-fm "umfradio";
  }
  {
    desc = "From gritty Berlin streets to dark corners of Brooklyn, this is techno made by artists pushing the genre further.";
    logo = "http://cdn-images.audioaddict.com/9/f/7/1/0/f/9f710f7f4c2a663bab3ff5551f8669eb.jpg?size=180x180";
    station = di-fm-name "Underground Techno";
    stream = di-fm "undergroundtechno";
  }
  {
    desc = "Relaxing vibes and a collection of vocal songs providing the laid back soundtrack to your day.";
    logo = "http://cdn-images.audioaddict.com/5/7/4/8/8/c/57488ced7c732709c72764636b0065dc.jpg?size=180x180";
    station = di-fm-name "Vocal Chillout";
    stream = di-fm "vocalchillout";
  }
  {
    desc = "The glorious 4/4 thump of House music paired perfectly with the human voice. Sultry, soulful, sexy sounds.";
    logo = "http://cdn-images.audioaddict.com/8/5/a/1/3/b/85a13bb220281bf22bf04d4f1f778b59.jpg?size=180x180";
    station = di-fm-name "Vocal House";
    stream = di-fm "vocalhouse";
  }
  {
    desc = "Laid back grooves and a collection of smooth vocals soothe the ears and relax the mind.";
    logo = "http://cdn-images.audioaddict.com/8/4/3/6/f/9/8436f9d3338ba7c8de0e88dbe45ea4d5.jpg?size=180x180";
    station = di-fm-name "Vocal Lounge";
    stream = di-fm "vocallounge";
  }
  {
    desc = "Lush vocals paired together with emotive dance music. Beautiful melodies and endless energy.";
    logo = "http://cdn-images.audioaddict.com/3/0/9/f/2/4/309f243a8a181ad83e8c5e15cd4b24c3.jpg?size=180x180";
    station = di-fm-name "Vocal Trance";
    stream = di-fm "vocaltrance";
  }
  {
    desc = "All Vaporwave. All the time.";
    logo = "http://soma.fm/img/vaporwaves120.jpg";
    station = soma-fm-name "Vaporwaves";
    stream = soma-fm "vaporwaves";
  }
  {
    desc = "Emotional Experiments in Music: Ambient, modern composition, post-rock, & experimental electronic music";
    logo = "http://soma.fm/img/n5md120.png";
    station = soma-fm-name "n5MD Radio";
    stream = soma-fm "n5md";
  }
  {
    desc = "A nicely chilled plate of ambient/downtempo beats and grooves.";
    logo = "http://soma.fm/img/groovesalad120.png";
    station = soma-fm-name "Groove Salad";
    stream = soma-fm "groovesalad";
  }
  {
    desc = "The classic (early 2000s) version of a nicely chilled plate of ambient/downtempo beats and grooves.";
    logo = "http://soma.fm/img3/gsclassic120.jpg";
    station = soma-fm-name "Groove Salad Classic";
    stream = soma-fm "gsclassic";
  }
  {
    desc = "Deep ambient electronic, experimental and space music. For inner and outer space exploration.";
    logo = "http://soma.fm/img/deepspaceone120.gif";
    station = soma-fm-name "Deep Space One";
    stream = soma-fm "deepspaceone";
  }
  {
    desc = "Tune in, turn on, space out. Spaced-out ambient and mid-tempo electronica.";
    logo = "http://soma.fm/img/sss.jpg";
    station = soma-fm-name "Space Station Soma";
    stream = soma-fm "spacestation";
  }
  {
    desc = "Served best chilled, safe with most medications. Atmospheric textures with minimal beats.";
    logo = "http://soma.fm/img/dronezone120.jpg";
    station = soma-fm-name "Drone Zone";
    stream = soma-fm "dronezone";
  }
  {
    desc = "Progressive house / trance. Tip top tunes.";
    logo = "http://soma.fm/img/thetrip120.jpg";
    station = soma-fm-name "The Trip";
    stream = soma-fm "thetrip";
  }
  {
    desc = "Music for Hacking. The DEF CON Year-Round Channel.";
    logo = "http://soma.fm/img/defcon120.png";
    station = soma-fm-name "DEF CON Radio";
    stream = soma-fm "defcon";
  }
  {
    desc = "Transcending the world of jazz with eclectic, avant-garde takes on tradition.";
    logo = "http://soma.fm/img/sonicuniverse120.jpg";
    station = soma-fm-name "Sonic Universe";
    stream = soma-fm "sonicuniverse";
  }
  {
    desc = "NEW! Reggae, Ska, Rocksteady classic and deep tracks.";
    logo = "http://soma.fm/img3/reggae120.png";
    station = soma-fm-name "Heavyweight Reggae";
    stream = soma-fm "reggae";
  }
  {
    desc = "Vintage soul tracks from the original 45 RPM vinyl.";
    logo = "http://soma.fm/img/7soul120.png";
    station = soma-fm-name "Seven Inch Soul";
    stream = soma-fm "7soul";
  }
  {
    desc = "Mellow album rock from the Seventies. Yacht not required.";
    logo = "http://soma.fm/img/seventies120.jpg";
    station = soma-fm-name "Left Coast 70s";
    stream = soma-fm "seventies";
  }
  {
    desc = "Early 80s UK Synthpop and a bit of New Wave.";
    logo = "http://soma.fm/img/u80s-120.png";
    station = soma-fm-name "Underground 80s";
    stream = soma-fm "u80s";
  }
  {
    desc = "The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!";
    logo = "http://soma.fm/img/secretagent120.jpg";
    station = soma-fm-name "Secret Agent";
    stream = soma-fm "secretagent";
  }
  {
    desc = "Sensuous and mellow vocals, mostly female, with an electronic influence.";
    logo = "http://soma.fm/img/lush120.jpg";
    station = soma-fm-name "Lush";
    stream = soma-fm "lush";
  }
  {
    desc = "Exploring music from Celtic roots and branches";
    logo = "http://soma.fm/img/thistle120.png";
    station = soma-fm-name "ThistleRadio";
    stream = soma-fm "thistle";
  }
  {
    desc = "Drown in the electronic sound of instrumental hiphop, future soul and liquid trap.";
    logo = "http://soma.fm/img/fluid120.jpg";
    station = soma-fm-name "Fluid";
    stream = soma-fm "fluid";
  }
  {
    desc = "Electropop and indie dance rock with sparkle and pop.";
    logo = "http://soma.fm/img/poptron120.png";
    station = soma-fm-name "PopTron";
    stream = soma-fm "poptron";
  }
  {
    desc = "A late night blend of deep-house and downtempo chill.";
    logo = "http://soma.fm/img/blender120.png";
    station = soma-fm-name "Beat Blender";
    stream = soma-fm "beatblender";
  }
  {
    desc = "Americana Roots music for Cowhands, Cowpokes and Cowtippers";
    logo = "http://soma.fm/img/bootliquor120.jpg";
    station = soma-fm-name "Boot Liquor";
    stream = soma-fm "bootliquor";
  }
  {
    desc = "Classic bachelor pad, playful exotica and vintage music of tomorrow.";
    logo = "http://soma.fm/img/illstreet.jpg";
    station = soma-fm-name "Illinois Street Lounge";
    stream = soma-fm "illstreet";
  }
  {
    desc = "What alternative rock radio should sound like. [explicit]";
    logo = "http://soma.fm/img/bagel120.png";
    station = soma-fm-name "BAGeL Radio";
    stream = soma-fm "bagel";
  }
  {
    desc = "New and classic favorite indie pop tracks.";
    logo = "http://soma.fm/img/indychick.jpg";
    station = soma-fm-name "Indie Pop Rocks!";
    stream = soma-fm "indiepop";
  }
  {
    desc = "Digitally affected analog rock to calm the agitated heart.";
    logo = "http://soma.fm/img/digitalis120.png";
    station = soma-fm-name "Digitalis";
    stream = soma-fm "digitalis";
  }
  {
    desc = "Indie Folk, Alt-folk and the occasional folk classics.";
    logo = "http://soma.fm/img/folkfwd120.jpg";
    station = soma-fm-name "Folk Forward";
    stream = soma-fm "folkfwd";
  }
  {
    desc = "Blips'n'beeps backed mostly w/beats. Intelligent Dance Music.";
    logo = "http://soma.fm/img/cliqhop120.png";
    station = soma-fm-name "cliqhop idm";
    stream = soma-fm "cliqhop";
  }
  {
    desc = "Dubstep, Dub and Deep Bass. May damage speakers at high volume.";
    logo = "http://soma.fm/img/dubstep120.png";
    station = soma-fm-name "Dub Step Beyond";
    stream = soma-fm "dubstep";
  }
  {
    desc = "Desi-influenced Asian world beats and beyond.";
    logo = "http://soma.fm/img/sog120.jpg";
    station = soma-fm-name "Suburbs of Goa";
    stream = soma-fm "suburbsofgoa";
  }
  {
    desc = "Ambient music mixed with the sounds of San Francisco public safety radio traffic.";
    logo = "http://soma.fm/img/sf1033120.png";
    station = soma-fm-name "SF 10-33";
    stream = soma-fm "sf1033";
  }
  {
    desc = "San Francisco Public Safety Scanner Feed";
    logo = "http://soma.fm/img/sf1033120.png";
    station = soma-fm-name "SF Police Scanner";
    stream = soma-fm "scanner";
  }
  {
    desc = "Celebrating NASA and Space Explorers everywhere.";
    logo = "http://soma.fm/img/missioncontrol120.jpg";
    station = soma-fm-name "Mission Control";
    stream = soma-fm "missioncontrol";
  }
  {
    desc = "From black to doom, prog to sludge, thrash to post, stoner to crossover, punk to industrial.";
    logo = "http://soma.fm/img3/metal120.png";
    station = soma-fm-name "Metal Detector";
    stream = soma-fm "metal";
  }
  {
    desc = "Just covers. Songs you know by artists you don't. We've got you covered.";
    logo = "http://soma.fm/img/covers120.jpg";
    station = soma-fm-name "Covers";
    stream = soma-fm "covers";
  }
  {
    desc = "From the Playa to the world, for the annual Burning Man festival.";
    logo = "http://soma.fm/img/1023brc.jpg";
    station = soma-fm-name "Black Rock FM";
    stream = soma-fm "brfm";
  }
  {
    desc = "Special Live Events and rebroadcasts of past live events";
    logo = "http://soma.fm/img/SomaFMDJSquare120.jpg";
    station = soma-fm-name "SomaFM Live";
    stream = soma-fm "live";
  }
  {
    desc = "SomaFM's wacky and eclectic holiday mix. Not for the easily offended.";
    logo = "http://soma.fm/img/xmasinfrisco120.jpg";
    station = soma-fm-name "Xmas in Frisko";
    stream = soma-fm "xmasinfrisko";
  }
  {
    desc = "Chilled holiday grooves and classic winter lounge tracks. (Kid and Parent safe!)";
    logo = "http://soma.fm/img/christmaslounge120.png";
    station = soma-fm-name "Christmas Lounge";
    stream = soma-fm "christmas";
  }
  {
    desc = "Have your self an indie/alternative holiday season!";
    logo = "http://soma.fm/img/xmasrocks120.png";
    station = soma-fm-name "Christmas Rocks!";
    stream = soma-fm "xmasrocks";
  }
  {
    desc = "Where we cut right to the soul of the season.";
    logo = "http://soma.fm/img/jollysoul120.png";
    station = soma-fm-name "Jolly Ol' Soul";
    stream = soma-fm "jollysoul";
  }
  {
    desc = "Department Store Christmas (extended through Jan 31)";
    logo = "http://soma.fm/img/SomaFMDJSquare120.jpg";
    station = soma-fm-name "SomaFM Specials";
    stream = soma-fm "specials";
  }
  {
    desc = "HandsUp / Dance";
    logo = "https://www.technobase.fm/content/images/site/logo-technobase.fm.png";
    station = we-are-one-name "TechnoBase.FM";
    stream = we-are-one "technobase";
  }
  {
    desc = "Dance & Pop";
    logo = "https://www.housetime.fm/content/images/site/logo-housetime.fm.png";
    station = we-are-one-name "HouseTime.FM";
    stream = we-are-one "housetime";
  }
  {
    desc = "Hardstyle";
    logo = "https://www.hardbase.fm/content/images/site/logo-hardbase.fm.png";
    station = we-are-one-name "HardBase.FM";
    stream = we-are-one "hardbase";
  }
  {
    desc = "Vocal & Uplifting Trance";
    logo = "https://www.trancebase.fm/content/images/site/logo-trancebase.fm.png";
    station = we-are-one-name "TranceBase.FM";
    stream = we-are-one "trancebase";
  }
  {
    desc = "Hardcore";
    logo = "https://www.coretime.fm/content/images/site/logo-coretime.fm.png";
    station = we-are-one-name "CoreTime.FM";
    stream = we-are-one "coretime";
  }
  {
    desc = "Techno / Minimal";
    logo = "https://www.clubtime.fm/content/images/site/logo-clubtime.fm.png";
    station = we-are-one-name "ClubTime.FM";
    stream = we-are-one "clubtime";
  }
  {
    desc = "Happy Hardcore / DnB";
    logo = "https://www.teatime.fm/content/images/site/logo-teatime.fm.png";
    station = we-are-one-name "TeaTime.FM";
    stream = we-are-one "teatime";
  }
  {
    desc = "90s / 00s";
    logo = "https://www.replay.fm/content/images/site/logo-replay.fm.png";
    station = we-are-one-name "Replay.FM";
    stream = we-are-one "replay";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Bigg-G-Quadrat-Webstream-final-klein.jpg";
    station = big-fm-name "Dancehall – Reggae – Afrobeat";
    stream = big-fm "reggaevibes";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigBALKAN_0.jpg";
    station = big-fm-name "bigBALKAN";
    stream = big-fm "balkan";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigSES_0.jpg";
    station = big-fm-name "bigSES (Türkei)";
    stream = big-fm "turkey";
  }
  # { logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigRUSSIA_0.jpg"; station = big-fm-name "bigRUSSIA"; stream = big-fm "russia"; }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_CHARTS.jpg";
    station = big-fm-name "Charts";
    stream = big-fm "charts";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Rock_am_Ring_720_Webradio_Crowd_2.jpg";
    station = big-fm-name "Rock am Ring";
    stream = big-fm "rockamring";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_MASHUP_0.jpg";
    station = big-fm-name "Mashup";
    stream = big-fm "mashup";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigFM_US_Rap_Hip-Hop.jpg";
    station = big-fm-name "US Rap & Hip Hop";
    stream = big-fm "usrap";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_HIP-HOP_0.jpg";
    station = big-fm-name "Hip-Hop";
    stream = big-fm "hiphop";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Oldschool_Rap_Hip_Hop_720x720.jpg";
    station = big-fm-name "Oldschool Rap & Hip Hop";
    stream = big-fm "oldschool";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Hip_Hop_Charts_DJ_blau.jpg";
    station = big-fm-name "Deutscher Hip-Hop Charts";
    stream = big-fm "dhiphopcharts";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/bigFM_DEUTSCHRAP.jpg";
    station = big-fm-name "Deutschrap";
    stream = big-fm "deutschrap";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Markus-Spiske-unsplash-quadrat-berlin.jpg";
    station = big-fm-name "Neu Berlin Deutschrap";
    stream = big-fm "rapfeature";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/Oldschool_Deutsch_720x720.jpeg";
    station = big-fm-name "Oldschool Deutschrap";
    stream = big-fm "oldschooldeutsch";
  }
  {
    logo = "https://cdn.bigfm.de/sites/default/files/styles/small_webstream_200/public/I_love_bigFM_MASHUP_0.jpg";
    station = big-fm-name "Mashup";
    stream = big-fm "mashup";
  }
  # { station = rautemusik-name "Workout"; stream = rautemusik "workout"; desc = "Dance, HipHop, Pop"; }
  # { station = rautemusik-name "Wacken Radio"; desc = "Metal, Heavy Metal"; stream = rautemusik "wackenradio"; }
  {
    station = rautemusik-name "Volksmusik";
    desc = "Volksmusik, Blasmusik, Schlager";
    stream = rautemusik "volksmusik";
  }
  # { station = rautemusik-name "Trance"; stream = rautemusik "trance"; desc = "Trance, Vocal Trance, Uplifting"; }
  {
    station = rautemusik-name "Study";
    stream = rautemusik "study";
    desc = "Lo-Fi, Chillout, Easy Listening";
  }
  {
    station = rautemusik-name "TechHouse";
    stream = rautemusik "techhouse";
    desc = "Techhouse, Deephouse, Techno, Minimal";
  }
  {
    station = rautemusik-name "Goldies";
    stream = rautemusik "goldies";
    desc = "Oldies, 60s, 70s, 80s";
  }
  {
    station = rautemusik-name "90s";
    stream = rautemusik "90s";
    desc = "90s, Eurodance, Pop, HipHop";
  }
  {
    station = rautemusik-name "Schlager";
    stream = rautemusik "schlager";
    desc = "Schlager, Discofox, Deutsch, Pop";
  }
  {
    station = rautemusik-name "Country";
    stream = rautemusik "country";
    desc = "Country, Western, Americana";
  }
  {
    station = rautemusik-name "Sex";
    stream = rautemusik "sex";
    desc = "RnB, Pop, Easy Listening";
  }
  {
    station = rautemusik-name "LoveHits";
    stream = rautemusik "lovehits";
    desc = "Lovesongs, Balladen, RnB, Pop";
  }
  {
    station = rautemusik-name "Klassik";
    stream = rautemusik "klassik";
    desc = "Symphonie, Orchester, Klassik";
  }
  {
    station = rautemusik-name "Traurig";
    stream = rautemusik "traurig";
    desc = "Balladen, Pop, Easy Listening";
  }
  {
    station = rautemusik-name "Happy";
    stream = rautemusik "happy";
    desc = "Pop, Dance, Charts";
  }
  {
    station = rautemusik-name "Solo Piano";
    stream = rautemusik "solopiano";
    desc = "Klavier, Instrumental, Easy Listening";
  }
  {
    station = rautemusik-name "HappyHardcore";
    stream = rautemusik "happyhardcore";
    desc = "UK Core, Happy Hardcore, Dance";
  }
  {
    station = rautemusik-name "HardeR";
    stream = rautemusik "harder";
    desc = "Hardstyle, Hardcore, Jumpstyle";
  }
  {
    station = rautemusik-name "BigCityBeats";
    stream = rautemusik "bigcitybeats";
    desc = "EDM, Dance, House, Electro, Star DJs";
  }
  {
    station = rautemusik-name "Lounge";
    stream = rautemusik "lounge";
    desc = "Ambient, Jazz, Chillout, Easy Listening";
  }
  {
    station = rautemusik-name "Oriental";
    stream = rautemusik "oriental";
    desc = "Arabisch, Oriental, HipHop";
  }
  {
    station = rautemusik-name "Salsa";
    stream = rautemusik "salsa";
    desc = "Salsa, Latina, Tropical";
  }
  {
    station = rautemusik-name "Christmas";
    stream = rautemusik "christmas";
    desc = "Weihnachtslieder, Balladen, Schlager";
  }
  {
    station = rautemusik-name "Christmas Chor";
    stream = rautemusik "christmas-chor";
    desc = "Chor, Weihnachtslieder, Gesang";
  }
  {
    station = rautemusik-name "Christmas Schlager";
    stream = rautemusik "christmas-schlager";
    desc = "Schlager, Weihnachtslieder";
  }
  {
    station = rautemusik-name "Weihnachten.FM";
    stream = rautemusik "weihnachten";
    desc = "Weihnachtslieder, Pop";
  }
  {
    station = rautemusik-name "Top40";
    stream = rautemusik "top40";
    desc = "Charts, Top40, Dance, Hiphop";
  }
  {
    station = rautemusik-name "Rock";
    desc = "Rock, Alternative, Punk";
    stream = rautemusik "rock";
  }
  {
    station = rautemusik-name "PartyHits";
    desc = "Karneval, Mallorca, Après Ski, Schlager";
    stream = rautemusik "partyhits";
  }
  # { station = rautemusik-name "Main"; stream = rautemusik "main"; desc = "Pop, Rock, Charts, 80s"; }
  # { station = rautemusik-name "JaM"; stream = rautemusik "jam"; desc = "HipHop, RnB, Rap, Soul, Urban"; }
  # { station = rautemusik-name "House"; stream = rautemusik "house"; desc = "House, Dance, Electro, EDM"; }
  {
    station = rautemusik-name "Deutschrap Charts";
    stream = rautemusik "deutschrap-charts";
    desc = "Deutschrap, HipHop, Rap, Charts";
  }
  {
    station = rautemusik-name "Deutschrap Classic";
    stream = rautemusik "deutschrap-classic";
    desc = "Oldschool, Rap, HipHop, Deutschrap";
  }
  # { station = rautemusik-name "Club"; stream = rautemusik "club"; desc = "HandsUp, Dance, Hard Dance"; }
  {
    station = rautemusik-name "ChartHits";
    stream = rautemusik "ChartHits";
    desc = "House, RnB, Dance, Electro";
  }
  {
    station = rautemusik-name "BreakZ.FM";
    stream = rautemusik "breakz";
    desc = "RnB, House, HipHop, Dance, Mixtapes";
  }
  {
    station = rautemusik-name "Bass";
    stream = rautemusik "bass";
    desc = "DnB, Dubstep, Trap & Bass House";
  }
  {
    station = rautemusik-name "12punks";
    stream = rautemusik "12punks";
    desc = "Punk, Punk Rock, Ska, Hardcore";
  }
  {
    logo = "https://d3kle7qwymxpcy.cloudfront.net/images/broadcasts/77/a4/13931/1/c175.png";
    station = "Raidió Rírá";
    stream = "http://185.80.220.12:8166/stream";
    desc = "Is cairt-staisiún ceoil é Raidió Rí-Rá a bhíonn ag craoladh go hiomlán trí Ghaeilge! Bíonn an ceol ar fad ó na cairteacha le cloisteáil ar an stáisiún, mar aon leis an bpopnuacht, an nuacht spóirt agus an nuacht scannánaíochta is déanaí!";
  }
  {
    stream = "http://188.247.86.67:8008";
    station = "Rotana Tarab";
    logo = "https://liveonlineradio.net/wp-content/uploads/2017/11/Rotana-Tarab-100x47.jpg";
  }
  {
    stream = "http://iphone.live24.gr/derty1000";
    station = "Derti FM";
    desc = "Μόνο λαϊκά";
    logo = "https://cdn-radiotime-logos.tunein.com/s87063q.png";
  }
  {
    logo = "http://www.stoxosfm.gr/images/stoxosfm-logo-small.png";
    desc = "Ο Στόχος FM μετράει σχεδόν 25 χρόνια ηχηρής παρουσίας στα ερτζιανά. Ιδρύθηκε το 1990 και έκτοτε έχει διαγράψει μια δυναμική και άκρως επιτυχημένη πορεία, κατακτώντας την προτίμηση ενός σταθερού και ολοένα αυξανόμενου ακροατηρίου.";
    station = "Stoxos FM";
    stream = "http://s3.onweb.gr:8016/;";
  }
  {
    stream = "http://62.210.24.124:8379/;stream.mp3";
    station = "Gjirokastër Albania";
    logo = "https://cdn-radiotime-logos.tunein.com/s151734q.png";
  }
  {
    stream = "http://95.173.188.170:9306/";
    station = "Arabesk Damar FM";
    logo = "https://www.arabeskdamarfm.com/uploads/logo/1.jpg";
    desc = "türkiyenin en cok dinlenen internet radyosu";
  }
  {
    station = "Vahon Hindustani Radio";
    stream = "http://94.23.148.11:8058/";
  }
  {
    logo = "https://www.liveradio.ie/files/images/115732/resized/180x172c/rte_raidio_na_gaeltachta.jpg";
    station = rte-name "Raidió Na Gaeltachta";
    desc = "Stáisiún Náisiúnta na Gaeltachta agus na Gaeilge, ag craoladh as Gaeilge.";
    stream = rte "rnag";
  }
  # { logo = "https://www.liveradio.ie/files/images/114812/resized/180x172c/rte_2fm.jpg"; station = rte-name "2fm"; stream = rte "2fm"; desc = "RTÉ 2fm is a music and entertainment service for 25-44's. 2fm is the national platform for new Irish talent, great festivals and live music."; }
  # { logo = "https://www.liveradio.ie/files/images/115626/resized/180x172c/rte_2xm.jpg"; stream = rte "2xm"; station = rte-name "2XM"; desc = "The station plays a broad range of music from rock, indie, metal, electronica, alternative and nu metal and also selection of live music with a particular focus on music content from festivals across Europe."; }
  {
    logo = "https://www.liveradio.ie/files/images/115762/resized/180x172c/rte_gold.jpg";
    stream = rte "gold";
    station = rte-name "Gold";
    desc = "RTÉ Gold is a play list service offering a carefully chosen selection of classic hits as well as album tracks from top selling artists spanning the '50s, '60s, ‘70s and ‘80s.";
  }
  {
    logo = "https://www.liveradio.ie/files/images/101096/resized/180x172c/rte_lyric_fm.png";
    stream = rte "lyric";
    station = rte-name "Lyric FM";
    desc = "“Without doubt RTÉ lyric fm is for listeners that are looking for a real alternative. With an increase in people tuning to the station ‘where life sounds better’ across weekends, there is something to satisfy everyone’s taste for specialist or more mainstream music.”";
  }
  {
    logo = "https://www.liveradio.ie/files/images/329303/resized/180x172c/rte_pulse.jpg";
    station = rte-name "Pulse";
    stream = rte "pulse";
    desc = "RTÉ Pulse is an electronic dance music station from Raidió Teilifís Éireann (RTÉ), Ireland's national broadcaster.";
  }
  {
    logo = "https://www.liveradio.ie/files/images/338090/resized/180x172c/rte_radio_1.jpg";
    station = rte-name "Radio 1";
    stream = rte "radio1";
    desc = "RTÉ Radio 1 is the principal radio channel of Irish public-service broadcaster Raidió Teilifís Éireann. The station is a rare modern example of a mixed radio network, broadcasting a mixture of music and speech programming.";
  }
  {
    logo = "https://www.liveradio.ie/files/images/115731/resized/180x172c/rte_radio_1_extra.jpg";
    station = rte-name "Radio 1 Extra";
    stream = rte "radio1extra";
    desc = "RTE Radio 1 Extra (aka RTE Radio 1xtra) – Quality speech radio from home and abroad.";
  }
  {
    logo = "https://cdn-profiles.tunein.com/s96877/images/logoq.png";
    station = "DWG Radio";
    desc = "Bibeltreues Radio im Internet";
    stream = "http://server25531.streamplus.de/;stream.mp3";
  }
  {
    station = "Wake News";
    logo = "https://stream.wakenews.net/wakenews-radio-200px.jpg";
    stream = "https://stream.wakenews.net/radio-high.ogg";
    desc = "Ohne Blatt vor dem Mund! Für alle, die aufwachen wollen.";
  }
  {
    logo = "http://www.beatlesradio.com/content/images/thumbs/0000587.gif";
    station = "Beatles Radio";
    stream = "http://www.beatlesradio.com:8000/stream";
    desc = "";
  }
  {
    logo = "https://knr.gl/sites/knr/themes/knr/gfx/sprite.png";
    station = "Kalaallit Nunaata Radioa";
    stream = "https://knr.gl/radiolivestream";
  }
  {
    logo = "http://sithafm.lk/sithafm.jpg";
    station = "Sitha.FM";
    stream = caster-fm "shaincast" 48148;
  }
  {
    logo = "https://media.radiosai.org/journals/vol_18/01JAN20/images/11_h2h_special/95th-Birthday-Logo-for-White-Background.jpg";
    station = radiosai-name "Bhajan Stream";
    stream = radiosai "bhajan";
    desc = "The voice of pure love.";
  }
  {
    logo = "https://media.radiosai.org/journals/vol_18/01JAN20/images/11_h2h_special/95th-Birthday-Logo-for-White-Background.jpg";
    station = radiosai-name "Asia Stream";
    stream = radiosai "asia";
    desc = "The voice of pure love.";
  }
  {
    logo = "https://media.radiosai.org/journals/vol_18/01JAN20/images/11_h2h_special/95th-Birthday-Logo-for-White-Background.jpg";
    station = radiosai-name "Ameri Stream";
    stream = radiosai "ameri";
    desc = "The voice of pure love.";
  }
  {
    stream = royal "RoyalPopsa";
    station = royal-name "Popsa";
    desc = "из Санкт-Петербурга";
  }
  {
    stream = royal "RoyalTrance";
    station = royal-name "Trance";
    desc = "из Санкт-Петербурга";
  }
  {
    stream = royal "RoyalDrum";
    station = royal-name "Drum";
    desc = "из Санкт-Петербурга";
  }
  {
    stream = royal "RoyalTrap";
    station = royal-name "Trap";
    desc = "из Санкт-Петербурга";
  }
  {
    stream = royal "RoyalRock";
    station = royal-name "Rock";
    desc = "из Санкт-Петербурга";
  }
  {
    stream = royal "RoyalLounge";
    station = royal-name "Lounge";
    desc = "из Санкт-Петербурга";
  }
  {
    stream = royal "RoyaLove";
    station = royal-name "Love";
    desc = "из Санкт-Петербурга";
  }
  # { station = "Rock | Kamchatka Live"; stream = "https://radio.kamchatkalive.ru:8103/rock"; logo = "https://kamchatkalive.ru/image/rock.png?1612248138"; }
  {
    station = "Rap Français | Mouv";
    stream = "http://icecast.radiofrance.fr/mouvrapfr-midfi.mp3";
    logo = "https://cdn.radiofrance.fr/s3/cruiser-production/2019/01/3c4dc967-ed2c-4ce5-a998-9437a64e05d5/300x300_rapfr.jpg";
  }
  {
    stream = "http://66.45.232.131:9994/;stream.mp3";
    station = "ERTU Al Quran Al Kareem";
  }
  {
    stream = "http://direct.radioarmenie.com:9029/stream";
    station = "Radio Arménie";
  }
  {
    stream = "http://onair15.xdevel.com:7064/1/";
    station = "Radio Mozart Italia";
    logo = "https://www.lafenicepubblicita.it/rmi/wp-content/uploads/2020/12/360x180.jpg";
    desc = "Emittente ufficiale delle Associazioni Mozart Italia nel mondo";
  }
  {
    stream = "http://onair7.xdevel.com:7126/1/";
    station = "Opera Radio Budapest";
    logo = "https://www.opera.hu/static/default/asset/img/common/opera-logo.svg";
  }
  {
    stream = "http://peacefulpiano.stream.publicradio.org/peacefulpiano.mp3";
    station = "Peaceful Piano";
  }
  {
    logo = "https://cdn.promodj.com/afs/11a5f0be108d5f48084aac34ec54da9f11:e598f2";
    stream = "https://radio.promodj.com/fullmoon";
    station = "Trance | PromoDJ";
  }
  {
    stream = "http://worship.lobpreisradio.de:8000/anbetung-lobpreis-radio-37k-ogg-stereo";
    station = "Lobpreisradio";
    desc = "Aufladen mit Lobpreis und Anbetung über Lobpreisradio, täglich auch Evangelium und Predigten sowie die Worship-Hits. Wir wollen Herzen von Gott berühren vor allem mit deutschen aber auch englischem Lobpreis";
  }
  {
    stream = radiorecord "1970";
    station = radiorecord-name "1970";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "1980";
    station = radiorecord-name "1980";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "2step";
    station = radiorecord-name "2step";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "cadillac";
    station = radiorecord-name "Cadillac";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "darkside";
    station = radiorecord-name "Darkside";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "dc";
    station = radiorecord-name "Dancecore";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "dream";
    station = radiorecord-name "Dream Dance";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "fbass";
    station = radiorecord-name "Future Bass";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "gast";
    station = radiorecord-name "Gastarbaiter FM";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "hbass";
    station = radiorecord-name "Hardbass";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "mf";
    station = radiorecord-name "FuckO";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "rap";
    station = radiorecord-name "Rap";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "rock";
    station = radiorecord-name "Rock";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "rus";
    station = radiorecord-name "Russian Mix";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "russianhits";
    station = radiorecord-name "Russian Hits";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "rv";
    station = radiorecord-name "Ruki Vverh Hits";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "symph";
    station = radiorecord-name "Symphony";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "synth";
    station = radiorecord-name "Synthwave";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "tm";
    station = radiorecord-name "Trancemission";
    logo = radiorecord-logo;
  }
  {
    stream = radiorecord "uplift";
    station = radiorecord-name "Uplifting";
    logo = radiorecord-logo;
  }
  {
    stream = "http://play2.organlive.com:7000/320";
    station = "Organ Live";
  }
  {
    stream = "https://listen1.outpostradio.com/omagic";
    station = "Organ Magic";
    desc = "Pipe Organ music 24/7. An Outpost Radio station.";
    logo = "https://outpostradio.com/organmagic/organ-magic-1-web.jpg";
  }
]
/*
      (caster-fm "TODO" "noasrv" 10182) # https://github.com/cccruzr/albumsyoumusthear/blob/7e00baf575e4d357cd275d54d1aeb717321141a8/HLS/IBERO_90_1.m3u
      (caster-fm "TODO" "shaincast" 20866) # https://github.com/cccruzr/albumsyoumusthear/blob/7e00baf575e4d357cd275d54d1aeb717321141a8/HLS/IBERO_90_1.m3u
*/

/*

CNN news in morse code
http://cw.dimebank.com:8080/CNNslow
http://cw.dimebank.com:8080/CNNfast



*/
