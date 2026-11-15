# adyton

The air-gapped Raspberry Pi 3 of `threat-model.md`. The R-ids below refer to it.

The SD card holds everything: the Pi firmware, U-Boot and the kernels on a FAT partition (`ADYTONBOOT`), and a LUKS partition (`adyton`) with LVM inside, holding `/nix` and `/persist`. `/` is tmpfs. The board itself stores nothing, so the card goes with you when adyton is not in use (R12).

Never plug in a network cable: the kernel has no network stack, but U-Boot falls back to network boot when the card does not boot.

## Update key (R2)

Kept on its own encrypted stick (KEY), plugged into fatteh only to sign:

```sh
nix key generate-secret --key-name adyton-update-1 > /media/KEY/adyton-update.sec
nix key convert-secret-to-public < /media/KEY/adyton-update.sec > systems/adyton/update-key.pub
git add systems/adyton/update-key.pub
```

adyton evaluates only once `update-key.pub` exists.

## Sticks (R3, R5)

IN and OUT are vfat sticks labelled `IN` and `OUT`. Before a stick goes to adyton, overwrite it on fatteh:

```sh
sudo $(nix build --no-link --print-out-paths .#adyton-wipe-stick)/bin/adyton-wipe-stick /dev/sdX OUT
```

Inspect the OUT stick on fatteh after every session, then wipe it again. BACKUP sticks are LUKS, made by `adyton-backup --init`.

## Installing

1. Build the installer on ful, and on fatteh, and check that both give the same store path (R2):

   ```sh
   nix build '.?submodules=1#nixosConfigurations.adyton-installer.config.system.build.sdImage'
   ```

   The kernel is custom, so nothing comes from a cache. Keep adyton out of CI and cachix, or the two builds stop being independent.
2. Write it to card A: `zstdcat result/sd-image/*.img.zst | sudo dd of=/dev/sdX bs=4M conv=fsync status=progress`.
3. Boot the Pi from card A, with the future adyton card (card B) in a USB card reader. It logs in as root.
4. `adyton-install /dev/sdX` (card B). It asks for the disk passphrase and the passwords of `admin` and `kfm`. When it asks, attach the keyboard and the sticks adyton should accept; usbguard allows exactly those from then on.
5. Power off, overwrite card A, boot from card B.
6. Generate the keys and do the R13 paper backups.

## Every session

adyton asks for the UTC date and time first: the Pi has no clock, and GPG writes the date into keys and signatures.

`kfm` mounts sticks with `mount /media/in` and `mount /media/out`. Power off by pulling the power cable (R14).

## Updating

On fatteh, with IN and KEY mounted:

```sh
nix run .#adyton-publish -- /media/IN /media/KEY/adyton-update.sec ssh-ng://root@ful
```

It builds adyton on fatteh and ful, compares every path of the closure, and only then signs onto the IN stick. Paths both take from cache.nixos.org were built once, by Hydra.

On adyton, as `admin`: `mount /media/in`, `sudo adyton-update`, then power off and on.

## Backups (R13)

As `admin`, with a BACKUP stick attached: `sudo adyton-backup /dev/sdX` (`--init` the first time). It copies `/persist` and a LUKS header backup.

Restore test, quarterly: on a spare card, run the installer, then `cryptsetup open` the BACKUP stick and copy its `persist/` over the new `/persist`.

## Admitting another USB device

As `admin`: `sudo usbguard list-devices`, then `sudo usbguard allow-device -p <id>`.
