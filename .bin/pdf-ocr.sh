#!/usr/bin/env nix-shell
#! nix-shell -i bash -p poppler_utils tesseract4
set -eu

pdf_path="$(realpath "$1")"

[ -f "$pdf_path" ] || {
  echo "Usage: $0 FILE.pdf" >&2
  exit 1
}


tmpdir="$(mktemp -d)"
trap 'rm -rf $tmpdir' EXIT

cd "$tmpdir"

pdftoppm -png "$pdf_path" pdf-ocr
for png in pdf-ocr*.png; do
  tesseract "$png" "$png.txt" 2>/dev/null
done

cat pdf-ocr-*.txt
