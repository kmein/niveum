# Print Unicode character info for each character on stdin
{
  writers,
  python3,
}:
writers.writePython3Bin "charinfo" {
  flakeIgnore = [ "E501" "E722" ];
} ''
  import sys
  import unicodedata

  for index, character in enumerate(sys.stdin.read().strip()):
      try:
          print(index, character, hex(ord(character)), unicodedata.category(character), unicodedata.name(character))
      except Exception:
          print(index, character, hex(ord(character)))
''
