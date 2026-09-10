# niveum

> I must Create a System, or be enslav'd by another Man's. —William Blake

> [nĭvĕus](https://logeion.uchicago.edu/niveus), a, um, adj. [nix], _of_ or _from snow, snowy, snow-_ (poet.)
>
> 1. Lit.: aggeribus niveis informis, Verg. G. 3, 354: aqua, _cooled with snow_, Mart. 12, 17, 6; cf. id. 14, 104 and 117: mons, _covered with snow_, Cat. 64, 240.—
> 2. Transf., _snow-white, snowy_ (mostly poet.): a similitudine sic: Corpore niveum candorem, aspectu igneum ardorem assequebatur, Auct. Her. 4, 33, 44: lacerti, Verg. A. 8, 387: lac, id. E. 2, 20: hanc si capite niveae agnae exorari judicas, Sen. Q. N. 2, 36: Briseis niveo colore, Hor. C. 2, 4, 3: vestis, Ov. M. 10, 432: candidior nivei folio, Galatea, ligustri, id. ib. 13, 789: dens, id. H. 18, 18: quā notam duxit niveus videri, Hor. C. 4, 2, 59: panis, Juv. 5, 70: flumen, _clear, pellucid_, Sen. Hippol. 504: undae, Mart. 7, 32, 11: tribuni, _clothed in white togas_, Calp. Ecl. 7, 29; so, Quirites, Juv. 10, 45.

## Pressestimmen

> das ist ja pure poesie —[riotbib](https://github.com/riotbib/)

> Deine Configs sind wunderschön <3 —[flxai](https://github.com/flxai/)

## Deployment

### Hosts

Every entry in `nixosConfigurations` has a matching app:

```sh
nix run .#deploy-ful
```

It probes the machine's direct addresses (internal IP, external IP, hyprspace, retiolum) in parallel and takes the first to answer, falling back to the `.onion` via Tor; then it runs `nixos-rebuild switch --flake .?submodules=1#<host> --target-host root@…` on the SSH port from `lib/machines.nix`. When the host's architecture differs from the local one — `ful` and `khall` are aarch64 — it builds on the target as well.

Check a host evaluates before deploying:

```sh
nix eval --raw '.?submodules=1#nixosConfigurations.ful.config.system.build.toplevel.drvPath'
```

`?submodules=1` is not optional: the `secrets` submodule has to be checked out, and new files must be `git add`ed or the flake won't see them. After adding or removing a secret, refresh the manifest with `nix run .#mock-secrets`.

### DNS

All zones live in `dnsconfig.js` and are managed with [dnscontrol](https://dnscontrol.org) at hosting.de:

```sh
nix shell nixpkgs#dnscontrol --command dnscontrol check    # validate, no network
nix shell nixpkgs#dnscontrol --command dnscontrol preview  # diff against the live zones
nix shell nixpkgs#dnscontrol --command dnscontrol push     # apply
```

Credentials are read from `creds.json` (untracked — the hosting.de API token). `dnscontrol write-types` regenerates `types-dnscontrol.d.ts` for editor completion.

## To do

🦗
