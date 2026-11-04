# Disaster recovery

The case planned for: everything at home is gone at once. That means the desktops, zaatar, khall and every key on them, including `kmein`'s ssh key and the desktops' host keys, which are the only other recipients of the personal backup's secrets.

What survives elsewhere:

- **the storagebox**: the `personal` and `server` restic repositories, plus the storagebox's own snapshots (Hetzner Console)
- **makanek and ful**, with the git repositories on makanek, among them the private `niveum-secrets`
- **this repository**, public on GitHub
- **the paper key**: an age key that exists only on paper, kept off-site. It is `paper` in `secrets/secrets.nix` and a member of `users`, so every secret is encrypted to it.
- **the old disk**: the restic repository from before the storagebox, plus a mirror of `niveum-secrets`, kept off-site in a different place from the paper

The paper key plus the ciphertexts is all it takes. The ciphertexts come either from private.code.kmein.de (over https with the code password, since no ssh key is left) or from the old disk.

## Getting the secrets

```sh
git clone https://github.com/kmein/niveum && cd niveum
# from private.code.kmein.de, as kfm with the code password:
git -c submodule.secrets.url=https://private.code.kmein.de/niveum-secrets.git submodule update --init
# or from the old disk, mounted at /media/disk (/mnt is for the new system); it belongs to another uid
git -c protocol.file.allow=always -c safe.directory='*' -c submodule.secrets.url=/media/disk/niveum-secrets.git submodule update --init

nix shell nixpkgs#age nixpkgs#restic
umask 077; cat > /dev/shm/paper            # type the key from the paper, Ctrl-D
age -d -i /dev/shm/paper secrets/restic-personal.age     > /dev/shm/pw
age -d -i /dev/shm/paper secrets/restic-personal-ssh.age > /dev/shm/sbkey
```

`/dev/shm` keeps the keys in RAM. The storagebox's ssh key can also be replaced through Hetzner Console, but the restic password exists nowhere except in these secrets.

## Opening the repository

Repository, host and the pinned host key come from `lib.niveum.restic` (`lib/default.nix`). For the `server` repository, use `repositories.server` and `restic-server*.age` instead.

```sh
r=".#nixosConfigurations.fatteh.pkgs.lib.niveum.restic"
nix eval --raw "$r.knownHost" --apply 'k: builtins.concatStringsSep "," k.hostNames + " " + k.publicKey + "\n"' >> ~/.ssh/known_hosts
repo=$(nix eval --raw "$r.repositories.personal.repository")
sftp=$(nix eval --raw "$r.repositories.personal" --apply 'p: p.sftpCommand "/dev/shm/sbkey"')
restic() { eval command restic -r "$repo" -o "$sftp" -p /dev/shm/pw '"$@"'; }
restic snapshots --latest 1 --host fatteh
```

Both desktops back up the same paths (`configs/backup.nix`) into this repository, so take whichever snapshot is newest. If the repository was wiped or tampered with, restore a storagebox snapshot in Hetzner Console first.

## Reinstalling a machine

The secrets are encrypted to the old host keys, so the new machine's key has to be made before it is installed:

1. Boot the NixOS installer, partition the disk and mount it at `/mnt`.
2. `ssh-keygen -t ed25519 -N '' -f /mnt/etc/ssh/ssh_host_ed25519_key`
3. Put its public key into `system.<host>` in `secrets/secrets.nix` and `hostKey` in `lib/machines.nix`. Replace `systems/<host>/hardware-configuration.nix` with the output of `nixos-generate-config --show-hardware-config`.
4. `(cd secrets && nix run github:ryantm/agenix -- -r -i /dev/shm/paper)`
5. `nixos-install --flake '.?submodules=1#<host>'`
6. Before rebooting, while the keys are still in `/dev/shm`: `restic restore <snapshot> --target /mnt`. This brings back the home directory, including `~/.ssh` and `~/.gnupg`.
7. Reboot. With `~/.ssh` back, commit and push both repositories. Re-sign the commits if needed.

## The paper key

Generate it on a machine you trust, outside any agent session:

```sh
age-keygen      # prints to stdout, writes no file; copy AGE-SECRET-KEY-1… by hand
age-keygen -y   # type it back from the paper, Ctrl-D: must print the same age1… recipient
```

Write it by hand rather than printing it, because the print spool keeps a copy. The key carries a checksum, so `age-keygen -y` catches typos. Put the `age1…` recipient into `paper` in `secrets/secrets.nix`, then run `agenix -r` and refresh the old disk. A new paper key always means rekeying and refreshing the disk's mirror of the secrets.

## The old disk

It holds the restic repository from before the storagebox (`restic/`) and a mirror of `niveum-secrets`. The repository opens with `secrets/restic.age`, which is also encrypted to the paper key. Restic encrypts the repository and the mirror is ciphertext, so the disk alone reveals nothing. Keep it away from the paper all the same: the paper decrypts both.

It is a second copy of the data that does not depend on Hetzner, frozen at its last refresh. To refresh it, attach it to a desktop and mount it at `/mnt`. `$KEY` is `kmein`'s ssh key:

```sh
r() { sudo restic -r /mnt/restic --password-command "age -d -i $KEY $PWD/secrets/restic.age" "$@"; }
r backup --tag offsite <paths and excludes from configs/backup.nix>
sudo rm -rf /mnt/niveum-secrets.git && sudo install -d -o "$USER" /mnt/niveum-secrets.git
git clone -q --mirror secrets /mnt/niveum-secrets.git
r check --read-data
```

## Keeping it working

- `paper` stays in `users` in `secrets/secrets.nix`, and every secret stays encrypted to `users`.
- The paper and the disk are kept in different places, and neither of them is at home.
- Access to the Hetzner account (password, 2FA recovery codes) and the code password must not depend only on devices at home. The storagebox snapshots, the rescue systems and the second way to the secrets all sit behind them. `code-basicAuth.age` holds only its hash.
- Once a year, test it: type the paper key, decrypt `restic-personal.age`, list the snapshots. Plug in the disk and run `r check`.
