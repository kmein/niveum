# Adding a machine

There are two kinds of machine:

- **Laptop**: `profiles.default ++ profiles.desktop`, importing `configs/default.nix`, like fatteh.
- **Server**: `profiles.default ++ profiles.server`, like makanek.

A machine's identity is its entry in `lib/machines.nix`. Several things are derived from that entry: ssh host key pinning (`configs/ssh.nix`), `try-connect` and the deploy apps, hyprspace peers and `*.h` names, and `ownAddresses`. Other places list hosts by hand; those are covered below.

Every key is made before installing. agenix decrypts with the host key on first boot, so that key has to exist, and every secret has to be encrypted to it, before then. Make the private keys in `/dev/shm`, where they never touch a disk:

```sh
h=<host>; k=/dev/shm/$h; mkdir -m 700 $k
```

## Both kinds

**Host key.** Its public half goes into `hostKey` in `lib/machines.nix` and into `system.<host>` in `secrets/secrets.nix`.

```sh
ssh-keygen -t ed25519 -N '' -C "root@$h" -f $k/ssh_host_ed25519_key
```

**Hyprspace.** The command prints the peer id; put it in `hyprspace.id`. The host's hyprspace addresses only exist after its first boot.

```sh
nix run github:hyprspace/hyprspace -- keygen --private-key-file $k/hyprspace
```

**Retiolum.** retiolum's hosts are listed in krebs' [kartei](https://github.com/krebs/kartei):

1. In a fork of kartei, run `nix run .#add-host` with namespace `kmein`. Move the private key it reports to `$k/retiolum`.
2. Add `<host>.r` to `kmein/hosts/<host>/retiolum/aliases`. Everything here reaches hosts by that name.
3. Ask in #krebs for a free `10.243.x.y`. kartei treats IPv4 as optional, but this repo doesn't: `configs/ssh.nix` pins `retiolum.ipv4`.
4. Open the PR. Once it's merged, run `nix flake update kartei`.

If the host is an x86 CPU without AVX2, tincd dies there with SIGILL. Use `tincd-compat`, as zaatar and manakish do.

**Secrets.** Add rules to `secrets/secrets.nix` for `<host>-retiolum-privateKey-ed25519.age` and `<host>-hyprspace-privateKey.age`, both `users ++ [system.<host>]`. Then encrypt the keys, and rekey so the secrets every host shares (`users ++ systems`) include the new one:

```sh
cd secrets
nix run github:ryantm/agenix -- -e $h-retiolum-privateKey-ed25519.age < $k/retiolum
nix run github:ryantm/agenix -- -e $h-hyprspace-privateKey.age < $k/hyprspace
nix run github:ryantm/agenix -- -r -i <kmein's key or the paper key>
```

**Configuration:**

- `lib/machines.nix`: `hostKey`, `system`, `inherit sshPort`, `retiolum = { ipv4; ipv6; }` from the kartei files, and `hyprspace.id`.
- `systems/<host>/configuration.nix`:
  - `networking.hostName`
  - `networking.retiolum = pkgs.lib.niveum.retiolumAddresses.<host>`
  - `system.stateVersion` set to the installer's release
  - `hardware-configuration.nix`, generated on the target (see Installing)
- `flake.nix`: a `nixosConfigurations.<host>` built from the profiles of its kind.
- `configs/ssh.nix`: a short alias in `matchBlocks`, if wanted. `<host>.niveum` is derived.
- `.github/workflows/niveum.yml`: a build matrix entry. aarch64 hosts need the arm runner.
- `AGENTS.md`: the machines table.

Then run `nix run .#mock-secrets`, `git add` everything, and check that `nix eval --raw '.?submodules=1#nixosConfigurations.<host>.config.system.build.toplevel.drvPath'` evaluates.

## Laptop only

- **Modules:**
  - in `flake.nix`, add the host's `nixos-hardware` module;
  - in `configuration.nix`, import `configs/default.nix` and whichever of `networkmanager.nix`, `home-wifi.nix` and `tlp.nix` apply, and set `niveum.{batteryName,wirelessInterface,promptColours}`.
- **Syncthing:**
  1. Run `nix shell nixpkgs#syncthing --command syncthing generate --home $k/syncthing`. It logs the device ID; put it in `syncthingId`.
  2. Encrypt `cert.pem` and `key.pem` as `<host>-syncthing-cert.age` and `<host>-syncthing-key.age`.
  3. In `configuration.nix`, declare them as `age.secrets.syncthing-{cert,key}`.
  4. Add the host to the folders' `devices` in `configs/cloud.nix`.
- **User key:** the public half of the laptop's own ssh key goes into `sshKey`. Add `machines.<host>.sshKey` to `kieran.sshKeys` in `lib/default.nix`, which is what root, kfm and the service users everywhere accept.
- **Shared laptop secrets:** add `system.<host>` to `laptops` and to the recipients of `wifi.age`, `restic-personal.age` and `restic-personal-ssh.age`. Set `lib.niveum.restic.group.<host> = "personal"`.
- **`internalIp`:** reserve the address in the router. Its configuration isn't in this repo.

## Server only

- **Modules:**
  - import `configs/restic-client.nix` and `configs/server-packages.nix`;
  - if it serves the web, also import `configs/nginx.nix`.
- **`externalIp`** in `lib/machines.nix`.
- **Backups:**
  - set `lib.niveum.restic.group.<host> = "server"`;
  - add `system.<host>` to the recipients of `restic-server.age` and `restic-server-ssh.age`.
- **Monitoring:** `profiles.server` brings the node exporter and log shipping. Prometheus on makanek only scrapes what it's told about, so in `systems/makanek/monitoring/default.nix`:
  - add a scrape job for `<host>.h:<node exporter port>`. Without it, `BackupStale` and the disk alerts never see the host;
  - add the host to `probedHosts`.

  The scrape runs over hyprspace, so it needs the host's `hyprspace.ipv6`, which only exists after the first boot.
- **DNS:** public names go in `dnsconfig.js` (see `README.md`).

## Installing

1. Boot a NixOS installer on the target. For a server, use the provider's rescue system.
2. Partition the disk and mount it at `/mnt`.
3. Generate `systems/<host>/hardware-configuration.nix` with `nixos-generate-config --root /mnt --show-hardware-config`.
4. Copy the host key pair to `/mnt/etc/ssh/` (mode 600 for the private half).
5. Push this repo and the secrets.
6. On the installer:
   - clone this repo;
   - get the secrets submodule over https, as kfm with the code password (the installer has no ssh key): `git -c submodule.secrets.url=https://private.code.kmein.de/niveum-secrets.git submodule update --init`;
   - run `nixos-install --flake '.?submodules=1#<host>' --no-root-passwd`, then reboot.
7. On the new host, read off the addresses that only exist now, and put them into `lib/machines.nix`:
   - `hyprspace.ipv4` and `hyprspace.ipv6`: `ip -br addr show hyprspace`
   - `torAddress`: `/var/lib/tor/onion/ssh/hostname`
8. Deploy every machine, not only the new one. The others learn its hyprspace peering, host key pins and names only from their own configuration. From now on the host deploys like the rest: `nix run '.?submodules=1#deploy-<host>'`.
9. Delete `$k`.
