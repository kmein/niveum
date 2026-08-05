{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (pkgs.lib.niveum) domain externalNetwork;

  githubPages = map (x: "185.199.${x}.153") [
    108
    109
    110
    111
  ];

  # hosting.de slaves our zones: ns1 and ns3 pull via AXFR from the addresses
  # below, ns2 is their anycast server, which syncs from those two and rejects
  # NOTIFY. https://www.hosting.de/helpdesk/produkte/dns/dns-master-ips/
  secondaries = {
    ns1 = [
      "134.0.30.178"
      "2a03:2900:3:1::2"
    ];
    ns3 = [
      "194.126.196.2"
      "2a03:2902:3:1::2"
    ];
  };

  # The NS RRset has to match what is delegated at DENIC, which wants at least
  # two nameservers on distinct addresses — hosting.de's three cover that on
  # their own, so makanek being down never takes the zones offline.
  nameservers = [
    "ns.${domain}."
    "ns1.hosting.de."
    "ns2.hosting.de."
    "ns3.hosting.de."
  ];

  # Renders a zone from nix: records is name -> type -> rdata (or list of rdata).
  # The serial is fixed at 1 on purpose — knot is configured with
  # zonefile-load = "difference-no-serial" below, so it keeps its own serial in
  # the journal and we never have to bump anything by hand.
  mkZone =
    origin:
    {
      records,
      primary ? builtins.head nameservers,
      hostmaster ? "hostmaster.${domain}.",
      ttl ? 3600,
      refresh ? 86400,
      # DENIC's predelegation check wants retry between 1/8 and 1/3 of refresh.
      retry ? 14400,
      expire ? 3600000,
      minimum ? 3600,
    }:
    pkgs.writeTextFile {
      name = "${origin}.zone";
      text = ''
        $ORIGIN ${origin}.
        $TTL ${toString ttl}
        @ IN SOA ${primary} ${hostmaster} 1 ${toString refresh} ${toString retry} ${toString expire} ${toString minimum}
      ''
      + lib.concatStrings (
        lib.mapAttrsToList (
          name: types:
          lib.concatStrings (
            lib.mapAttrsToList (
              type: rdata: lib.concatMapStrings (datum: "${name} IN ${type} ${datum}\n") (lib.toList rdata)
            ) types
          )
        ) records
      );
      checkPhase = ''
        ${lib.getExe' config.services.knot.package "kzonecheck"} -o ${origin}. $out
      '';
    };

  zones = {
    ${domain}.records = {
      "@".NS = nameservers;
      # Glue: this address also has to be registered at hosting.de.
      "ns".A = externalNetwork.makanek;
    };

    # kierän.de — out of bailiwick for ns.kmein.de, so no glue here.
    "xn--kiern-0qa.de".records = {
      "@".NS = nameservers;
    };
  };
in
{
  services.knot = {
    enable = true;

    settings = {
      # Binding the wildcard would race systemd-resolved's stub listener.
      server.listen = [ "${externalNetwork.makanek}@53" ];

      log.syslog.any = "info";

      remote = lib.mapAttrs (_: addresses: {
        address = map (address: "${address}@53") addresses;
      }) secondaries;

      acl.secondaries = {
        address = lib.concatLists (lib.attrValues secondaries);
        action = "transfer";
      };

      template.default = {
        # Zone files live in the store, so knot must never write back to them:
        # it takes the records from the file and keeps serial bumps in its journal.
        zonefile-load = "difference-no-serial";
        zonefile-sync = "-1";
        journal-content = "all";

        notify = lib.attrNames secondaries;
        acl = "secondaries";
      };

      zone = lib.mapAttrs (origin: zone: { file = mkZone origin zone; }) zones;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };
}
