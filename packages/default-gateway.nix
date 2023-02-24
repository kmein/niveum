{
  writers,
  iproute2,
  jq,
}:
writers.writeDashBin "default-gateway" ''
  ${iproute2}/bin/ip -json route | ${jq}/bin/jq --raw-output '.[0].gateway'
''
