{
  lib,
  passage,
  coreutils,
  oath-toolkit,
  openssh,
  systemd,
  replaceVars,
  # how long `passage unlock`'s identity lives without use, as a systemd time span
  lockAfter ? "10min",
}:
let
  systemctl = lib.getExe' systemd "systemctl";
in
passage.overrideAttrs (old: {
  postPatch = (old.postPatch or "") + ''
    substituteInPlace src/password-store.sh --replace-fail \
      'IDENTITIES_FILE="''${PASSAGE_IDENTITIES_FILE:-$HOME/.passage/identities}"' \
      'IDENTITIES_FILE="''${PASSAGE_IDENTITIES_FILE:-$HOME/.passage/identities}"
    source ${replaceVars ./unlocked.bash { inherit systemctl; }}'
  '';
  postInstall = old.postInstall + ''
    install -Dm755 ${
      replaceVars ./otp.bash { oathtool = lib.getExe' oath-toolkit "oathtool"; }
    } $out/lib/passage/extensions/otp.bash
    install -Dm755 ${
      replaceVars ./unlock.bash {
        inherit systemctl lockAfter;
        sshKeygen = lib.getExe' openssh "ssh-keygen";
        systemdRun = lib.getExe' systemd "systemd-run";
        rm = lib.getExe' coreutils "rm";
      }
    } $out/lib/passage/extensions/unlock.bash
    install -Dm755 ${
      replaceVars ./lock.bash { inherit systemctl; }
    } $out/lib/passage/extensions/lock.bash
  '';
})
