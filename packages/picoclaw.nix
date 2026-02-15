{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule (finalAttrs: {
  pname = "picoclaw";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "sipeed";
    repo = "picoclaw";
    rev = "v${finalAttrs.version}";
    hash = "sha256-nx/D8ir4/l0pTnMNORby2FNtU+ouKT0DUjP2vpJLmPk=";
  };
  postPatch = ''
    substituteInPlace go.mod --replace "go 1.25.7" "go 1.25.5"
  '';
  proxyVendor = true;

  # Set to lib.fakeHash or empty initially, then update with the actual hash Nix reports.
  vendorHash = "sha256-XKwYmbMyf4yg/E4Yv0uMS9v0oAuMZJwvoaAPCL/1AAY=";

  subPackages = [ "cmd/picoclaw" ];

  ldflags = [
    "-s" "-w"
    "-X main.version=${finalAttrs.version}"
  ];

  meta = with lib; {
    description = "Ultra-efficient AI Assistant in Go for $10 hardware";
    homepage = "https://github.com/sipeed/picoclaw";
    license = licenses.mit; # Verify license in the repo
    maintainers = [];
  };
})
