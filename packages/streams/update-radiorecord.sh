curl 'https://www.radiorecord.ru/api/stations/' --compressed \
| jq '
    .result.stations
    | map({
      station: .title,
      desc: .tooltip,
      stream: .stream_320,
      tags: .genre|map(.name)
    }) ' \
> radiorecord.json
