{ runCommand, di-fm-key, lib, writeText, zip }:
let
  streams = import ../lib/streams.nix {inherit di-fm-key;};
  object = rec {
    modificationDate = "6/25/24 09:09 AM";
    version = 0;
    stations = map (stream: {
      bitrate = 0;
      codec = "";
      homepage = "";
      image = if stream ? "logo" then stream.logo else "";
      imageColor = -1;
      imageManuallySet = stream ? "logo";
      isPlaying = false;
      modificationDate = modificationDate;
      name = stream.station;
      nameManuallySet = true;
      radioBrowserChangeUuid = "";
      radioBrowserStationUuid = "";
      remoteImageLocation = "";
      remoteStationLocation = "";
      smallImage = "";
      starred = false;
      stream = 0;
      streamContent = "audio/mpeg";
      streamUris = [stream.stream];
      uuid = stream.station; # hacky hack
    }) streams;
  };
  json = writeText "collection.json" (builtins.toJSON object);
  m3u = writeText "collection.m3u" ''
    #EXTM3U
    ${lib.concatMapStringsSep "\n" (stream: ''
      #EXTIF:-1,${stream.station}
      ${stream.stream}
    '') streams}
  '';
in runCommand "transistor.zip" {} ''
  mkdir collection
  cp ${json} collection/collection.json
  cp ${m3u} collection/collection.m3u
  ${zip}/bin/zip -r $out collection/
''
