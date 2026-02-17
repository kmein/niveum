# niveum — Agent Notes

## What This Is

A NixOS flake managing ~9 machines (desktops, servers, family laptops) for one user (kmein/kfm).
Levantine food-themed hostnames: fatteh, kabsa, kibbeh, makanek, manakish, tabula, tahina, zaatar, ful.

## Repository Structure

```
flake.nix          # ~650 lines — everything: inputs, overlay, nixosConfigurations, apps, packages
configs/           # ~50 NixOS config fragments imported by systems (desktop defaults, programs, services)
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

## Known Bugs / Broken References

All previously broken references have been fixed. See git log for details.
The deleted files (`configs/admin-essentials.nix`, `configs/nix.nix`, `configs/zsh.nix`,
`configs/tmux.nix`) were superseded by niphas modules already present in `profiles.default`.

## Architectural Issues

### 1. flake.nix is a monolith (636 lines)
The overlay alone is ~180 lines. The nixosConfigurations block is ~170 lines. Overlay, apps, packages output, nixosConfigurations, profiles — all in one file.

### 2. configs/default.nix is a grab-bag (200+ lines, ~15 inline anonymous modules)
It's a list of `imports` mixing inline `{ ... }` blocks with file imports. Hard to find what's defined where. Many of these inline blocks should be their own files (user config, i18n, gnupg, dconf, xdg dirs, etc.).

### 3. Retiolum secret boilerplate repeated 9 times
Every system has a near-identical block:
```nix
age.secrets.retiolum-rsa = { file = ../../secrets/${hostname}-retiolum-privateKey-rsa.age; mode = "400"; owner = "tinc-retiolum"; group = "tinc-retiolum"; };
age.secrets.retiolum-ed25519 = { ... same ... };
```
This could be a function or module parameterized by hostname.

### 4. Nginx + ACME boilerplate duplicated
ful and makanek have identical nginx recommended settings + ACME config. Should be a shared server profile.

### 5. modules/retiolum.nix uses NIX_PATH lookups (`<retiolum/hosts>`, `<system-secrets/...>`)
This breaks flake purity. The `configs/retiolum.nix` partially overrides the key paths to use agenix, but the hosts file and extraHosts still use `<retiolum/...>`.

### 6. Orphaned packages
- `packages/gpt.nix`, `packages/hora.nix`, `packages/k-lock.nix` — not in overlay, not referenced anywhere
- `wallpapers` flake input — declared but never referenced in outputs
- `naersk` and `fenix` inputs — only used transitively via `follows`, not directly

### 7. niveum-* overlay aliases
After our cleanup, `niveum-terminal`, `niveum-browser`, `niveum-filemanager` are aliases to niphas equivalents. These could be removed by updating the ~6 references in configs/i3.nix, configs/graphical/home-manager.nix, configs/default.nix to use niphas-* names directly.

### 8. The `pkgs.lib.niveum` pattern
Custom lib is injected via overlay into `pkgs.lib`. This works but is unconventional — it means the lib is only available where the overlay is applied, and it pollutes the pkgs namespace. A `specialArgs` approach or a standalone lib would be cleaner.

### 9. Restic backup config scattered
`services.restic.backups.niveum` is configured in configs/backup.nix, configs/applicative.nix, and extended in 5+ system files. The module system merges these, but it's hard to see what a given machine backs up without reading multiple files.

### 10. configs/ vs modules/ distinction is unclear
`configs/` has both stateless config fragments (spacetime.nix = timezone) and stateful ones (backup.nix, cloud.nix). `modules/` has proper option-declaring modules. Some configs/ files (like power-action.nix) are actually imported from modules/. The boundary is blurry.

## Machines Overview

| Machine   | Role         | Profile          | Arch         | Notes                              |
|-----------|--------------|------------------|--------------|------------------------------------|
| fatteh    | Desktop      | default+desktop  | x86_64       | ThinkPad T480, CUDA, main daily    |
| kabsa     | Desktop      | default+desktop  | x86_64       | ThinkPad X220, constrained (2 jobs)|
| manakish  | Desktop      | default+desktop  | x86_64       | ThinkPad X230                      |
| kibbeh    | Desktop      | custom (broken!) | x86_64       | Pantheon DE, travel laptop          |
| ful       | Server       | default+server   | aarch64      | Oracle/Hetzner, nginx, web services|
| makanek   | Server       | default+server   | x86_64       | Hetzner, gitea, nextcloud, weechat |
| zaatar    | Server/Home  | custom (broken!) | x86_64       | Home assistant, backup server       |
| tabula    | Family laptop| custom (broken!) | x86_64       | LXQt, user "xenos"                 |
| tahina    | Family laptop| custom (broken!) | x86_64       | Pantheon, user "xenos", German      |

## Improvement Ideas (Prioritized)

1. **Fix broken references** (5 files reference deleted configs)
2. **Extract retiolum secret boilerplate** into a function/module
3. **Split flake.nix** — at minimum extract the overlay to `overlay.nix`
4. **Break up configs/default.nix** into proper named files
5. **Remove orphaned packages** (gpt.nix, hora.nix, k-lock.nix)
6. **Remove unused flake inputs** (wallpapers)
7. **Clean up nixosModules.zsh-kmein** (references deleted file)
8. **Extract nginx+ACME server profile**
9. **Replace niveum-* aliases** with direct niphas-* references
10. **Fix modules/retiolum.nix** NIX_PATH usage for flake purity
