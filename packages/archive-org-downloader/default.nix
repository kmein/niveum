{
  lib,
  python3Packages,
  fetchFromGitHub,
}:

python3Packages.buildPythonApplication {
  pname = "archive-org-downloader";
  version = "unstable-2026-08-19";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "MiniGlome";
    repo = "Archive.org-Downloader";
    rev = "ea0d8072bab449ee199b02d037c5b26a24b9576a";
    hash = "sha256-+LkAoXU5sGKMkiDxHFCFvspe8RT5aWBxDmaeJzK3DIs=";
  };

  # Upstream ships a bare script (hyphenated filename, no pyproject.toml, no
  # importable entry point), so wrap its argparse block in a main() and
  # supply packaging metadata ourselves.
  patches = [ ./main-function.patch ];
  postPatch = ''
    mv archive-org-downloader.py archive_org_downloader.py
    cp ${./pyproject.toml} pyproject.toml
  '';

  build-system = [ python3Packages.setuptools ];

  dependencies = with python3Packages; [
    requests
    tqdm
    img2pdf
    pycryptodome
  ];

  # No test suite upstream.
  doCheck = false;

  meta = {
    description = "Download archive.org books as PDF";
    homepage = "https://github.com/MiniGlome/Archive.org-Downloader";
    license = {
      fullName = "PolyForm Noncommercial License 1.0.0";
      url = "https://polyformproject.org/licenses/noncommercial/1.0.0";
      free = false;
      redistributable = true;
    };
    mainProgram = "archive-org-downloader";
  };
}
