# Split a PDF into chunks of N pages
{
  writers,
  pdftk,
  gnugrep,
  coreutils,
}:
writers.writeDashBin "chunk-pdf" ''
  set -efu

  INPUT_FILE="''${2:?Pass the PDF path as second argument.}"
  PAGES_PER_REPORT="''${1:?Pass the chunk size as first argument.}"

  if [ ! -f "$INPUT_FILE" ]; then
    echo >&2 "File $INPUT_FILE does not exist."
    exit 1
  fi

  TOTAL_PAGES="$(${pdftk}/bin/pdftk "$INPUT_FILE" dump_data | ${gnugrep}/bin/grep NumberOfPages | ${coreutils}/bin/cut -f2 -d' ')"

  RUNS=$((TOTAL_PAGES/PAGES_PER_REPORT))

  for run in $(${coreutils}/bin/seq 0 "$((RUNS-1))"); do
    start_page=$((run*PAGES_PER_REPORT+1))
    end_page=$(((run+1)*PAGES_PER_REPORT))
    output_file="chunk_$((run+1)).pdf"
    echo "splitting $INPUT_FILE from $start_page to $end_page into $output_file"
    ${pdftk}/bin/pdftk "$INPUT_FILE" cat "$start_page-$end_page" output "$output_file"
  done
''
