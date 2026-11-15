{
  lib,
  passage,
  oath-toolkit,
  replaceVars,
}:
passage.overrideAttrs (old: {
  postInstall = old.postInstall + ''
    install -Dm755 ${
      replaceVars ./otp.bash { oathtool = lib.getExe' oath-toolkit "oathtool"; }
    } $out/lib/passage/extensions/otp.bash
  '';
})
