# Berlin BVG transit disruption checker
{
  writers,
  curl,
  jq,
}:
writers.writeDashBin "bvg" ''
  interesting="U6 N6 140 M46 184 N84"

  ${curl}/bin/curl -sSL 'https://www.bvg.de/disruption-reports/q' \
    --data-raw '{"variables":{},"query":"{
      allDisruptions {
        disruptions {
          meldungsId
          linie
          verkehrsmittel
          __typename
          ... on Traffic {
            datum
            gueltigVonDatum
            gueltigVonZeit
            gueltigBisDatum
            gueltigBisZeit
            richtungName
            richtungHafasId
            beginnAbschnittName
            beginnAbschnittHafasId
            endeAbschnittName
            endeAbschnittHafasId
            textIntUrsache
            sev
            textIntAuswirkung
            umfahrung
            textWAPSMSUrsache
            textWAPSMSAuswirkung
            prioritaet
            __typename
          }
        }
        __typename
      }
    }"}' \
    | ${jq}/bin/jq --arg interesting "$interesting" '
      .data.allDisruptions.disruptions
      | map(select(
        (.linie as $linie
        | $interesting
        | split(" ")
        | index($linie))
        and (.["__typename"] == "Traffic")
      ))
    '
''
