{
  lib,
  passage,
  oath-toolkit,
  openssh,
  replaceVars,
}:
passage.overrideAttrs (old: {
  # an identity `passage unlock` left in memory wins over the passphrase-protected one
  postPatch = (old.postPatch or "") + ''
    substituteInPlace src/password-store.sh --replace-fail \
      'IDENTITIES_FILE="''${PASSAGE_IDENTITIES_FILE:-$HOME/.passage/identities}"' \
      'IDENTITIES_FILE="''${PASSAGE_IDENTITIES_FILE:-$HOME/.passage/identities}"
    UNLOCKED_IDENTITIES_FILE="''${XDG_RUNTIME_DIR-}/passage/identities"
    [[ -z ''${PASSAGE_IDENTITIES_FILE-} && -f $UNLOCKED_IDENTITIES_FILE ]] && IDENTITIES_FILE="$UNLOCKED_IDENTITIES_FILE"'
  '';
  postInstall = old.postInstall + ''
    install -Dm755 ${
      replaceVars ./otp.bash { oathtool = lib.getExe' oath-toolkit "oathtool"; }
    } $out/lib/passage/extensions/otp.bash
    install -Dm755 ${
      replaceVars ./unlock.bash { sshKeygen = lib.getExe' openssh "ssh-keygen"; }
    } $out/lib/passage/extensions/unlock.bash
  '';
})
