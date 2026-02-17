# Convert CSV to JSON
{
  writers,
  python3,
}:
writers.writePython3Bin "csv2json" {
  flakeIgnore = [ "E501" ];
} ''
  import csv
  import json
  import sys
  import argparse

  parser = argparse.ArgumentParser()
  parser.add_argument("--delimiter", "-d", default=",", help="CSV field separator")

  args = parser.parse_args()

  if __name__ == "__main__":
      json.dump(list(csv.DictReader(sys.stdin, delimiter=args.delimiter)), sys.stdout)
''
