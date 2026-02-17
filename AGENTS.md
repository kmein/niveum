# niveum — Agent Notes

## What This Is

A NixOS flake managing ~9 machines (desktops, servers, family laptops) for one user (kmein/kfm).
Levantine food-themed hostnames: fatteh, kabsa, kibbeh, makanek, manakish, tabula, tahina, zaatar, ful.

## Repository Structure

```
flake.nix          # ~670 lines — inputs, overlay, nixosConfigurations, apps, packages output
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
lib/               # default.nix (niveum helpers), machines.nix (IP/key inventory), panoptikon.nix
secrets/           # agenix-encrypted .age files (empty dir in checkout, tracked via secrets.txt)
```

## Key Relationships

- **niphas** (input): Provides shared "how I like things" config — nixosModules (shell, editor, git, desktop, nix, udiskie) and overlay (niphas-* packages). Used in `profiles.default` and `profiles.desktop`.
- **configs/default.nix**: The "big desktop profile" — imported by fatteh, kabsa, manakish (the main desktop machines). NOT imported by servers or family laptops.
- **profiles** (in flake.nix): `profiles.default`, `profiles.desktop`, `profiles.server` — lists of modules composed per machine.
- **lib.niveum**: Custom lib injected via overlay (`pkgs.lib.niveum`) — used everywhere for machine addresses, SSH port, helper functions.

## Coding Conventions

- Packages use `writers.writeDashBin`, `writers.writeBashBin`, or `writers.writePython3Bin`
- Dependencies are referenced via `lib.getExe pkg` (main executable) or `lib.getExe' pkg "name"` (specific binary)
- For packages needing many commands via PATH, use `lib.makeBinPath` instead (see `packages/prospekte.nix`)
- Overlay entries use `prev.callPackage packages/foo.nix { }` pattern
- Packages are exported via `inherit (pkgs) ...` in the `packages` output

## Known Bugs / Broken References

All previously broken references have been fixed (see commits `36132b04`, `e67d6d7d`).

Remaining issues:
- `modules/retiolum.nix` uses `<retiolum/hosts>` and `<system-secrets/...>` NIX_PATH lookups — breaks flake purity but works with current `NIX_PATH` setup

## Architectural Issues

### 1. configs/default.nix is a grab-bag (200+ lines, ~15 inline anonymous modules)
It's a list of `imports` mixing inline `{ ... }` blocks with file imports. Hard to find what's defined where.

### 2. Retiolum secret boilerplate repeated 9 times
Every system has a near-identical block:
```nix
age.secrets.retiolum-rsa = { file = ../../secrets/${hostname}-retiolum-privateKey-rsa.age; mode = "400"; owner = "tinc-retiolum"; ... };
age.secrets.retiolum-ed25519 = { ... same ... };
```
Could be a function or module parameterized by hostname.

### 3. Nginx + ACME boilerplate duplicated
ful and makanek have identical nginx recommended settings + ACME config.

### 4. niveum-* overlay aliases
`niveum-terminal`, `niveum-browser`, `niveum-filemanager` are aliases to niphas equivalents. Could be removed by updating ~6 references in configs/ to use niphas-* names directly.

### 5. The `pkgs.lib.niveum` pattern
Custom lib injected via overlay into `pkgs.lib`. Unconventional — only available where overlay is applied. A `specialArgs` approach or standalone lib would be cleaner.

### 6. Restic backup config scattered
`services.restic.backups.niveum` is configured in configs/backup.nix, configs/applicative.nix, and extended in 5+ system files. Hard to see what a given machine backs up.

### 7. configs/ vs modules/ distinction blurry
`configs/` has both stateless config fragments (spacetime.nix = timezone) and stateful ones (backup.nix, cloud.nix). `modules/` has proper option-declaring modules. Some configs/ files import from modules/.

## Machines Overview

| Machine   | Role         | Profile          | Arch    | Notes                               |
|-----------|--------------|------------------|---------|---------------------------------------|
| fatteh    | Desktop      | default+desktop  | x86_64  | ThinkPad T480, CUDA, main daily       |
| kabsa     | Desktop      | default+desktop  | x86_64  | ThinkPad X220, constrained (2 jobs)   |
| manakish  | Desktop      | default+desktop  | x86_64  | ThinkPad X230                         |
| kibbeh    | Desktop      | default+desktop  | x86_64  | Pantheon DE, travel laptop            |
| ful       | Server       | default+server   | aarch64 | Oracle/Hetzner, nginx, web services   |
| makanek   | Server       | default+server   | x86_64  | Hetzner, gitea, nextcloud, weechat    |
| zaatar    | Server/Home  | default+server   | x86_64  | Home assistant, backup server         |
| tabula    | Family laptop| default           | x86_64  | LXQt, user "xenos"                   |
| tahina    | Family laptop| default           | x86_64  | Pantheon, user "xenos", German        |

## Remaining Improvement Ideas

1. **Extract retiolum secret boilerplate** into a function/module
2. **Break up configs/default.nix** into proper named files
3. **Extract nginx+ACME server profile**
4. **Replace niveum-* aliases** with direct niphas-* references
5. **Fix modules/retiolum.nix** NIX_PATH usage for flake purity
