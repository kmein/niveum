# OCR a PDF file to text using tesseract
{
  lib,
  writers,
  poppler_utils,
  tesseract,
  coreutils,
}:
writers.writeDashBin "pdf-ocr" ''
  set -efu

  pdf_path="$(${lib.getExe' coreutils "realpath"} "$1")"

  [ -f "$pdf_path" ] || {
    echo "Usage: pdf-ocr FILE.pdf" >&2
    exit 1
  }

  tmpdir="$(${lib.getExe' coreutils "mktemp"} -d)"
  trap 'rm -rf $tmpdir' EXIT

  cd "$tmpdir"

  ${lib.getExe' poppler_utils "pdftoppm"} -png "$pdf_path" pdf-ocr
  for png in pdf-ocr*.png; do
    ${lib.getExe tesseract} "$png" "$png.txt" 2>/dev/null
  done

  cat pdf-ocr-*.txt
''
