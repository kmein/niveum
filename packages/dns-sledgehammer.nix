{
  writers,
  coreutils,
}:
writers.writeDashBin "dns-sledgehammer" ''
  ${coreutils}/bin/printf '%s\n' 'nameserver 1.1.1.1' 'options edns0' > /etc/resolv.conf
''
