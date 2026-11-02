# Retiring a machine

A retired machine's keys keep working wherever they are still listed:

- **kartei:** retiolum nodes accept its tinc key.
- **`secrets/secrets.nix`:** every secret encrypted to its host key opens with that key.
- **`kieran.sshKeys`:** its user key logs in everywhere.

Removing a machine from this repo only stops encrypting to it from now on. Whatever the machine could decrypt before, its disk still can, and git history keeps every old ciphertext. So wipe the disk. If you can't, because it was lost, stolen or broke beyond repair, also follow [Lost or stolen](#lost-or-stolen).

`docs/new-machine.md` lists everywhere a machine gets added; retiring undoes each of them. To find what's left over: `git grep -i -w <host>` here and in the secrets repo, plus `ls secrets | grep <host>`.

## Steps

1. **kartei.** In a fork of [kartei](https://github.com/krebs/kartei), remove `kmein/hosts/<host>` (`nix run .#remove-host`, or `git rm -r`) and open a PR. Once it's merged, run `nix flake update kartei` here. Other krebs members' nodes keep accepting the key until they update their own kartei. That delay can't be shortened.
2. **The repo.** Remove:
   - its `nixosConfigurations` entry, `systems/<host>/`, and its entry in `lib/machines.nix`;
   - its scrape job, `probedHosts` entry and DNS records;
   - its syncthing devices and `kieran.sshKeys` entry;
   - the alias in `configs/ssh.nix`, the CI matrix entry and the row in `AGENTS.md`.
3. **Restic.** Remove it from `lib.niveum.restic.group`. If it was its group's `maintainer`, give that role to another host, or nobody prunes or checks the repository any more. Its snapshots stay; `restic forget --host <host> --prune` drops them once they aren't wanted.
4. **Secrets.** Remove `system.<host>` and the host's own rules from `secrets/secrets.nix`, and delete `<host>-*.age`. Then run `agenix -r`, so the shared secrets are no longer encrypted to the host key, and `nix run .#mock-secrets`.
5. **Deploy every remaining machine.** That is how they drop the host as a hyprspace peer, stop pinning its host key and stop accepting its user key.

## Lost or stolen

Rekeying doesn't help against a disk that is already gone: it holds the private keys, and git history holds the ciphertexts they open. Everything the machine could decrypt has to be changed where it comes from: a new password or token at the service, then encrypted again. The rules in `secrets/secrets.nix` say what that is:

- **A server:** its host key opens the shared secrets (`systems`), the restic secrets of its group, and its own.
- **A laptop:** much worse. Treat everything in the home directory as disclosed: the ssh keys, `~/.gnupg` (revoke the key) and `~/.electrum` (move the funds). `~/.ssh` is synced between the laptops, so if `kmein`'s key lives there, every secret is disclosed too: `kmein` is a recipient of all of them.
- **`kmein`'s key itself:** make a new one, put it into `users` and register it wherever the old one was registered outside this repo.

restic can't change a repository's master key. `restic key add` and `restic key remove` only replace the password that unlocks it. Someone who had the password and the storagebox key may already have the master key, and only a new repository locks them out: `restic init --from-repo <old> --copy-chunker-params`, then `restic copy --from-repo <old>`, then delete the old one. Give the storagebox sub-account a new ssh key in Hetzner Console as well.
