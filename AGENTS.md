# niveum — Agent Notes

## What This Is

A NixOS flake managing 10 machines (desktops, servers, family laptops, a Raspberry Pi) for one user (kmein/kfm).
Levantine food-themed hostnames: fatteh, kabsa, khall, kibbeh, makanek, manakish, tabula, tahina, zaatar, ful.

## Repository Structure

```
flake.nix          # ~770 lines — inputs, overlay, nixosConfigurations, apps, packages output
configs/           # ~50 NixOS config fragments imported by systems
  default.nix      # 200+ line mega-module for desktop machines (user, shell, gnupg, i18n, etc.)
  graphical/       # Hyprland + home-manager config (415 lines in home-manager.nix)
  packages.nix     # ~250 lines of environment.systemPackages
  bots/            # Telegram/Mastodon/Matrix bot configs
  keyboard/        # XKB layouts (Coptic, Avestan, Gothic, etc.)
configs/*.nix      # Individual concerns: bluetooth, sound, printing, ssh, fonts, etc.
modules/           # Proper NixOS modules with options (retiolum, telegram-bot, passport, power-action, etc.)
packages/          # ~107 package files (scripts, wrappers, small tools)
systems/<name>/    # Per-machine: configuration.nix + hardware-configuration.nix + extras
lib/               # default.nix (niveum helpers + constants), machines.nix (IP/key inventory)
secrets/           # agenix-encrypted .age files (empty dir in checkout, tracked via secrets.txt)
```

## Key Relationships

- **niphas** (input): Provides shared "how I like things" config — nixosModules (shell, editor, git, desktop, nix, udiskie) and overlay (niphas-\* packages). Used in `profiles.default` and `profiles.desktop`.
- **configs/default.nix**: The "big desktop profile" — imported by fatteh, kabsa, manakish (the main desktop machines). NOT imported by servers or family laptops.
- **profiles** (in flake.nix): `profiles.default`, `profiles.desktop`, `profiles.server` — lists of modules composed per machine.
- **lib.niveum**: Custom lib injected via overlay (`pkgs.lib.niveum`) — used everywhere for machine addresses, SSH port, helper functions.

## Coding Conventions

- Packages use `writers.writeDashBin`, `writers.writeBashBin`, or `writers.writePython3Bin`
- Dependencies are referenced via `lib.getExe pkg` (main executable) or `lib.getExe' pkg "name"` (specific binary)
- For packages needing many commands via PATH, use `lib.makeBinPath` instead (see `packages/prospekte.nix`)
- Overlay entries use `prev.callPackage packages/foo.nix { }` pattern
- Packages are exported via `inherit (pkgs) ...` in the `packages` output
- The base web domain is `pkgs.lib.niveum.domain` (`kmein.de`); build subdomains as `"pad.${pkgs.lib.niveum.domain}"`, never as literals
- Server-shared fragments live in configs/ and are imported from a host's configuration.nix: `nginx.nix` (recommended settings + ACME), `restic-client.nix` (backup repo/timer/secret), `oci-containers.nix` (podman + weekly image pull), `server-packages.nix`
- Per-service backup paths are added via `services.restic.backups.niveum.paths = [ ... ]` in the service's own file; retiolum key secrets are derived from `networking.hostName` in configs/retiolum.nix
- To verify a change, eval the affected hosts: `nix eval --raw '.?submodules=1#nixosConfigurations.<host>.config.system.build.toplevel.drvPath'` — new files must be `git add`ed first or flake eval won't see them; after adding/removing secrets run `nix run .#mock-secrets` to refresh secrets.txt

## Known Bugs / Broken References

None currently known. No NIX_PATH (`<...>`) lookups remain; the flake evaluates purely (with `?submodules=1`).

## Architectural Issues

### 1. configs/default.nix is a grab-bag (200+ lines, ~15 inline anonymous modules)

It's a list of `imports` mixing inline `{ ... }` blocks with file imports. Hard to find what's defined where.

### 2. The `pkgs.lib.niveum` pattern

Custom lib injected via overlay into `pkgs.lib`. Unconventional — only available where overlay is applied. A `specialArgs` approach or standalone lib would be cleaner.

### 3. Adding a package requires touching three places

`packages/<name>.nix`, the overlay in flake.nix, and the ~90-name `inherit (pkgs) ...` list in the `packages` output. The output list also inherits a few names (`pi`, `rfc`, `timer`) that are not defined in the overlay, so its provenance is murky. Deriving the output from the overlay's attr names would collapse this.

### 4. configs/ vs modules/ distinction blurry

`configs/` has both stateless config fragments (spacetime.nix = timezone) and stateful ones (backup.nix, cloud.nix). `modules/` has proper option-declaring modules. Some configs/ files import from modules/.

## Machines Overview

| Machine  | Role          | Profile         | Arch    | Notes                               |
| -------- | ------------- | --------------- | ------- | ----------------------------------- |
| fatteh   | Desktop       | default+desktop | x86_64  | ThinkPad T480, CUDA, main daily     |
| kabsa    | Desktop       | default+desktop | x86_64  | ThinkPad X220, constrained (2 jobs) |
| manakish | Desktop       | default+desktop | x86_64  | ThinkPad X230                       |
| kibbeh   | Desktop       | default+desktop | x86_64  | Pantheon DE, travel laptop          |
| ful      | Server        | default+server  | aarch64 | Oracle/Hetzner, nginx, web services |
| makanek  | Server        | default+server  | x86_64  | Hetzner, gitea, nextcloud, weechat  |
| zaatar   | Server/Home   | default+server  | x86_64  | Home assistant, backup server       |
| tabula   | Family laptop | (none)          | x86_64  | LXQt, user "xenos"                  |
| tahina   | Family laptop | default         | x86_64  | Pantheon, user "xenos", German      |
| khall    | Raspberry Pi  | minimal         | aarch64 | RPi 3, SD-card image build          |

## Remaining Improvement Ideas

1. **Break up configs/default.nix** into proper named files
2. **Derive the `packages` output from the overlay** so new packages are registered in one place
3. **Comment-disabled files** (configs/i3.nix, configs/bots/tlg-wotd.nix, makanek's menstruation/names/onlyoffice.nix) — decide to reenable or delete
