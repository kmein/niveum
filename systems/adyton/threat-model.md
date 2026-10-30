# adyton — threat model

Status: written before any adyton configuration exists; every adyton design choice must cite the R-ids it serves.

## 1. System

- An air-gapped NixOS machine on a spare ThinkPad X230. It stays at home and is powered off whenever it is not in use.
- Purposes: (a) cryptographic master keys: GPG certify key, age identities; (b) sensitive offline document work; (c) offline signing for a cold crypto wallet.
- The only channels in or out are the human (keyboard, display) and USB mass-storage sticks.
  - **Inbound:** signed NixOS closures, documents, unsigned transactions.
  - **Outbound:** public keys, signatures, signed transactions, exported documents.
- The build station (fatteh) builds and signs closures. It plays the role of the "Datenschleuse" in BSI SYS.4.5.A16.

```mermaid
flowchart LR
  inputs["nixpkgs / flake inputs"] --> build["fatteh build"]
  build --> compare["second builder compare"]
  compare --> sign["sign with adyton update key (offline token)"]
  sign --> instick["IN stick"]
  instick --> adyton["adyton"]
  adyton --> outstick["OUT stick"]
  outstick --> inspect["fatteh (inspect, wipe)"]
```

## 2. Assets and protection needs

Scale: normal / high / very high (BSI).

| ID | Asset | Confidentiality | Integrity | Availability |
| --- | --- | --- | --- | --- |
| A1 | Master keys (GPG certify key, age identities) | very high | very high | high |
| A2 | Wallet seed and private keys | very high | very high | very high |
| A3 | Sensitive documents | very high | high | high |
| A4 | adyton Nix update signing key (held on the build side) | very high | very high | normal |
| A5 | Boot integrity anchors (firmware image, TPM-sealed secrets, /boot signing key) | high | very high | high |
| A6 | Disk passphrase and LUKS header | very high | high | very high |

Availability "very high" means loss cannot be recovered without a backup (→ R13).

## 3. Adversaries

### T1 Remote attacker via the USB channel

- **Goal:** exfiltrate A1–A3; get adyton to sign attacker-chosen data (integrity of A2); persist on adyton.
- **Capabilities:** controls fatteh, a nixpkgs/flake input, or a stick's contents and its controller firmware (BadUSB).
- **Cannot:** reach adyton over any network; touch it physically.

### T2 Thief/burglar

- **Goal:** resale or opportunistic data theft.
- **Capabilities:** takes the laptop, sticks and papers found nearby; has unlimited offline time.
- **Cannot:** find the device running — it is found powered off.

### T3 Evil maid

- **Goal:** capture passphrases or keys at the next use.
- **Capabilities:** repeated unsupervised access while the owner is away; can open the case, reflash SPI with a SOIC-8 clip, plant a keylogger/implant, swap in a lookalike laptop, tamper with sticks.
- **Cannot:** be present during use; break tamper-evident seals without it being noticed.

### T4 Seizure / compelled access

- **Goal:** get A1–A3.
- **Capabilities:** confiscates every device and paper in the home at once (backups included); runs forensics on disks and sticks; may seize the machine while it is running (cold-boot attack); applies legal pressure to disclose the passphrase.
- **Cannot:** reach copies stored outside the home without separate action (see R13).

## 4. Attack scenarios

| ID | Adversary | Path | Countered by |
| --- | --- | --- | --- |
| S1 | T1 | Malicious closure (compromised fatteh or input) installed by an update | R2, R9, R10 |
| S2 | T1 | Crafted filesystem/USB stack exploit or BadUSB keystroke injection from a stick | R3, R8 |
| S3 | T1 | Malware on adyton hides secrets in outbound data (slack space, metadata, steganography) | R5 |
| S4 | T1 | Malware alters a transaction before signing (address/amount) | R6 |
| S5 | T1 | Malware persists across reboots | R7, R10 |
| S6 | T2 | Theft of the powered-off laptop | R11 |
| S7 | T2, T4 | Backup sticks/paper found in the home | R11, R13 |
| S8 | T3 | Reflashed firmware captures the passphrase | R12 |
| S9 | T3 | Hardware keylogger, implant, or lookalike laptop | R12 |
| S10 | T3 | Tampered /boot, kernel or initrd | R12 |
| S11 | T4 | Machine seized while running; cold-boot memory extraction | R7, R14 |
| S12 | T4 | Passphrase disclosure is compelled | Accepted risk (§6), bounded by R7 |
| S13 | any | Radio or management-engine path (Wi-Fi, Bluetooth, ME/AMT) | R1 |
| S14 | — | Own failure: disk dies, passphrase forgotten, firmware bricked | R13 |

## 5. Requirements

| ID | Requirement (testable) | Counters | Candidate implementation (non-binding) |
| --- | --- | --- | --- |
| **R1** | No network capability. The kernel is built without `INET`, `PACKET`, `NETDEVICES`, `WIRELESS`, `WLAN`, `CFG80211`, `BT`, `NFC`; the system build fails if `CONFIG_INET=y`; the Wi-Fi card is physically removed; Intel ME is neutered. Note: `CONFIG_NET`/`UNIX` stay enabled because systemd needs `AF_UNIX` and netlink. | S13 | Custom kernel config + `system.checks` grep of the kernel `.config`; remove the mini-PCIe Wi-Fi card; `me_cleaner` in the coreboot build. |
| **R2** | Only closures signed by the adyton update key enter `/nix/store`. `substituters` empty, `require-sigs = true`, `trusted-public-keys` contains only the adyton key; the update key never sits on fatteh's normal disk; a closure is signed only if two independent builds produce identical outputs. | S1 | `nix store sign` from an offline token; `nix build --rebuild` / a second builder comparison. |
| **R3** | Inbound media are controlled. Only allowlisted USB devices are authorised; sticks are mounted manually `ro,noexec,nosuid,nodev`; no automount; only ext4/vfat filesystem drivers; dedicated labelled sticks IN / OUT / BACKUP; a stick that was ever lost is retired (BSI SYS.2.3.A6, SYS.4.5.A2/A6). | S2 | `services.usbguard` with an explicit allowlist; no udiskie. |
| R4 | — merged into R3/R8 | — | — |
| **R5** | Outbound data is minimised. The OUT stick is fully overwritten on fatteh before each use and its contents inspected there afterwards; outbound files are limited to ASCII-armored public keys/signatures, PSBTs/signed transactions, and documents cleaned by `mat2` (BSI SYS.4.5.A7). | S3 | `mat2` in the closure; a wipe script on fatteh. |
| **R6** | Transactions are verified twice. Destination and amount are checked on adyton before signing, and checked again on the online side against the original intent before broadcast. | S4 | Manual procedure. |
| **R7** | Amnesic by default. Root on tmpfs; only explicitly listed paths persist; no swap; `init_on_free=1`; no suspend or hibernate (power off after every session). RAM: only `/` is tmpfs; `/nix` and the persisted paths live on the LUKS disk, so the tmpfs holds only runtime state (`/etc`, `/var`, `/tmp`, an empty `$HOME`). Cap it with `size=1G` (tmpfs consumes RAM only for what is actually written). With no swap, the RAM requirement is driven by the applications (office suite, wallet), not by amnesia: 4 GB is the floor, and 8 GB or more (X230 maximum 16 GB DDR3) is comfortable. Fallback if RAM is below 4 GB: a btrfs root subvolume restored from a blank snapshot at every boot — same amnesic behaviour, no RAM cost, but deleted data stays recoverable on the (encrypted) disk until it is overwritten; this weakens the bound on S12. | S5, S11, S12 | Impermanence-style bind mounts; `swapDevices = [ ]`; `systemd.sleep` disabled. |
| **R8** | Kernel hardening. Kernel parameters `slab_nomerge slub_debug=FZ vsyscall=none mce=0 page_alloc.shuffle=1 mds=full,nosmt randomize_kstack_offset=on spec_store_bypass_disable=on init_on_free=1 intel_iommu=on`; sysctls `kernel.kptr_restrict=2`, `kernel.kexec_load_disabled=1`, `kernel.sysrq=0`, `net.core.bpf_jit_harden=2`, `vm.mmap_rnd_bits` at its maximum; module loading locked after boot; FireWire/Thunderbolt not built (Tails kernel hardening; BSI SYS.2.3.A14/A18/A20). | S2 | `boot.kernelParams`, `boot.kernel.sysctl`, `security.lockKernelModules`, `security.protectKernelImage`. |
| **R9** | Account separation. No root login; the daily user has no sudo; a separate admin account is used only for updates; users are immutable (BSI SYS.2.3.A1). | S1 | `users.mutableUsers = false`. |
| **R10** | Executable code comes only from the read-only store. `/home`, `/tmp` and persistent data mounts are `noexec,nosuid,nodev` (BSI SYS.2.3.A15). | S1, S5 | `fileSystems.<path>.options`. |
| **R11** | Full-disk encryption with a passphrase that is not stored on the device. LUKS2 with argon2id; no TPM-only unlock; every backup is encrypted (BSI SYS.2.3.A19). | S6, S7 | `boot.initrd.luks.devices`. |
| **R12** | Firmware and boot tampering is detectable before any secret is typed. Open firmware (coreboot); measured boot with an attestation the user can check on a hardware token; SPI flash write-protected from the OS; /boot contents signed; tamper-evident seals (photographed glitter nail polish) on case screws and on the RAM/flash covers, checked against the photos before every use. | S8, S9, S10 | See D1. |
| **R13** | Availability. A `paperkey` printout of A1 and a wallet-seed backup on paper/metal, stored outside the home; two encrypted backup sticks (one at home, one off-site); a LUKS header backup; a quarterly restore test (BSI SYS.4.5.A17); a recovery path for a bricked firmware: external SOIC-8 flash from khall (Raspberry Pi) using a known-good ROM image kept on the backup sticks. | S7, S14 | Procedure document. |
| **R14** | Emergency power-off. A single physical action powers the machine off within seconds, so memory is erased via R7. | S11 | A udev rule that runs `poweroff` when a designated USB token is removed (modelled on Tails' boot-medium watchdog). |

## 6. Accepted risks

- Compelled disclosure (S12): if the passphrase is disclosed, everything persisted is disclosed. There is no plausible-deniability scheme; the only mitigation is persisting as little as possible (R7).
- Covert channels from a compromised adyton through outbound data that passes R5, and acoustic/electromagnetic emanation (these need a receiver near the machine).
- Hardware supply chain (CPU, embedded controller, residual ME code).
- An attacker who is present during use (shoulder surfing, cameras in the room).
- Simultaneous compromise of both builders and the holder of the update key.

## 7. Consequences for open design decisions

- **D1 Firmware (R12).** The X230 has no Intel Boot Guard, so anyone with a clip can replace the firmware, including any UEFI Secure Boot key database.
  - coreboot + edk2 (Secure Boot) + lanzaboote enforces signatures, but gives the user no attestation: firmware reflashed with attacker keys boots silently. It fails R12 against T3.
  - Heads (coreboot with a Linux payload) measures the firmware into the TPM. Via TOTP/reverse HOTP on a Nitrokey/Librem Key, it shows the user that the firmware is unchanged before the passphrase is entered. It also GPG-verifies every /boot file against a key held on an OpenPGP smartcard ([Heads security model](https://github.com/linuxboot/heads/blob/master/doc/security-model.md)). It satisfies R12.
  - **Recommendation: Heads.** It replaces lanzaboote, because Heads does not use UEFI Secure Boot. Costs: an OpenPGP+HOTP token is required, and every new NixOS generation requires re-signing /boot in the Heads menu.
  - The owner explicitly asked for lanzaboote, so this needs owner confirmation before implementation.
- **D2 Update model (R2, R7, R10).** Signed `nix copy` into the store with a tmpfs root, vs. an immutable dm-verity image.
  - **Recommendation: signed `nix copy` with a tmpfs root.** It already meets R2/R7/R10. Verity would add protection only against an attacker who already has root and wants to persist in the store, a path R7 and R9 already narrow.
  - Revisit if the S5 risk is rated higher later.
- **D3 Hardware.** ThinkPad X230, fixed. Physical preparation before installation: remove the Wi-Fi card (R1); flash the firmware externally for the first time (R12); seal and photograph the case (R12). Whether Bluetooth is a separate module on the X230 is unverified — confirm when the case is open, and remove it if present.

## 8. Sources

- [Tails design](https://tails.net/contribute/design/)
- [BSI SYS.2.3 Clients unter Linux und Unix (Edition 2023)](https://www.bsi.bund.de/SharedDocs/Downloads/DE/BSI/Grundschutz/IT-GS-Kompendium_Einzel_PDFs_2023/07_SYS_IT_Systeme/SYS_2_3_Clients_unter_Linux_und_Unix_Edition_2023.pdf)
- [BSI SYS.4.5 Wechseldatenträger (Edition 2023)](https://www.bsi.bund.de/SharedDocs/Downloads/DE/BSI/Grundschutz/IT-GS-Kompendium_Einzel_PDFs_2023/07_SYS_IT_Systeme/SYS_4_5_Wechseldatentraeger_Edition_2023.pdf)
- [Grundschutz++](https://www.bsi.bund.de/dok/grundschutz-plus-plus)
- [Heads security model](https://github.com/linuxboot/heads/blob/master/doc/security-model.md)
- [Lanzaboote automatic provisioning](https://nix-community.github.io/lanzaboote/explanation/automatic-provisioning.html)
