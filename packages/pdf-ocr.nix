# OCR a PDF file to text using tesseract
{
  writers,
  poppler_utils,
  tesseract,
  coreutils,
}:
writers.writeDashBin "pdf-ocr" ''
  set -efu

  pdf_path="$(${coreutils}/bin/realpath "$1")"

  [ -f "$pdf_path" ] || {
    echo "Usage: pdf-ocr FILE.pdf" >&2
    exit 1
  }

  tmpdir="$(${coreutils}/bin/mktemp -d)"
  trap 'rm -rf $tmpdir' EXIT

  cd "$tmpdir"

  ${poppler_utils}/bin/pdftoppm -png "$pdf_path" pdf-ocr
  for png in pdf-ocr*.png; do
    ${tesseract}/bin/tesseract "$png" "$png.txt" 2>/dev/null
  done

  cat pdf-ocr-*.txt
''
