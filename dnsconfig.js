// @ts-check
/// <reference path="types-dnscontrol.d.ts" />

var REG_HOSTINGDE = NewRegistrar("hostingde_primary");
var DNS_HOSTINGDE = NewDnsProvider("hostingde_primary");

var GITHUB_USER = "kmein";
var makanek = "88.99.83.173";
var ful = "130.61.217.114";

/**
 * Helper to generate GitHub Pages apex IP records and an optional CNAME.
 * @param {string} [subdomain] - Optional subdomain for CNAME (defaults to "www", pass null/false to disable)
 */
function GITHUB_PAGES(subdomain) {
  if (subdomain === undefined) {
    subdomain = "www";
  }

  var records = [
    A("@", "185.199.108.153"),
    A("@", "185.199.109.153"),
    A("@", "185.199.110.153"),
    A("@", "185.199.111.153"),
    AAAA("@", "2606:50c0:8000::153"),
    AAAA("@", "2606:50c0:8001::153"),
    AAAA("@", "2606:50c0:8002::153"),
    AAAA("@", "2606:50c0:8003::153")
  ];

  if (subdomain) {
    records.push(CNAME(subdomain, GITHUB_USER + ".github.io."));
  }

  return records;
}

D("irideszenz.de", REG_HOSTINGDE, DnsProvider(DNS_HOSTINGDE),
  DefaultTTL(600),
  GITHUB_PAGES(),
);

D("kieranmeinhardt.de", REG_HOSTINGDE, DnsProvider(DNS_HOSTINGDE),
    DefaultTTL(86400),
    GITHUB_PAGES(),
    TXT("@", "google-site-verification=fsDy9oQ697HHALdnyTZ0A6Xqohp_bpcorHQ9re4_w2g"),
);

D("kmein.de", REG_HOSTINGDE, DnsProvider(DNS_HOSTINGDE),
    DefaultTTL(600),
    A("@",ful, TTL(86400)),
    A("alew",ful, TTL(86400)),
    A("cloud",makanek),
    A("code",makanek),
    A("dichtungsring",ful, TTL(120)),
    A("feed",makanek),
    A("ical-ephemeris",ful),
    A("iching",makanek),
    A("ledger",ful, TTL(86400)),
    A("matomo",ful),
    A("onlyoffice",makanek),
    A("openapiaiapi",ful),
    A("pad",makanek),
    A("pocket",ful),
    A("pr",ful),
    A("pun-sort",ful),
    A("radio",ful),
    A("scrabble",makanek, TTL(86400)),
    A("tarot",makanek),
    A("www",makanek),
);

D("ptoros.de", REG_HOSTINGDE, DnsProvider(DNS_HOSTINGDE),
    DefaultTTL(86400),
    GITHUB_PAGES(),
);

D("schilfpalast.de", REG_HOSTINGDE, DnsProvider(DNS_HOSTINGDE),
    DefaultTTL(86400),
    GITHUB_PAGES(),
    TXT("@", "google-site-verification=FDQzjnqLsMaOajiy8IlAbWC7ARXJrvqEOhHcA2DiHNc"),
    TXT("@", "google-site-verification=Y9tNPaP6VcKZXq6R-a3G3_An1R680AxO0bqBUAymZIA"),
);

D("xn--kiern-0qa.de", REG_HOSTINGDE, DnsProvider(DNS_HOSTINGDE),
    DefaultTTL(600),
    GITHUB_PAGES("logotheca"),
    TXT("@", "google-site-verification=uoey13jSjqFvRljDsQF5bn6x_DI2r80jS2bZc1cVvXU", TTL(86400)),
    A("meteora",ful),
);
