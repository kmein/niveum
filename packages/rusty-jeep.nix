{
  lib,
  fetchFromGitHub,
  rustPlatform,
  pkg-config,
  alsa-lib,
}:
rustPlatform.buildRustPackage {
  pname = "rusty-jeep";
  version = "1.0.0";

  src = let
    repo = fetchFromGitHub {
      owner = "kmein";
      repo = "scripts";
      rev = "c8e80b34c08e427f83b6af19a361e8c0711a4e6c";
      sha256 = "18jlf9zkhni4jsvzrlkkllqvv5dkhjmilggcchbs32hr1km51q84";
    };
  in "${repo}/rusty-jeep";

  nativeBuildInputs = [pkg-config];
  buildInputs = [alsa-lib];

  cargoHash = "sha256-8qbYTqRk+4InJNX6xK95VxISamDb5KID+wbmUDJYJ94=";

  meta = with lib; {
    description = "A beeping program inspired by K_belwagen";
    license = licenses.wtfpl;
    maintainers = [maintainers.kmein];
  };
}
