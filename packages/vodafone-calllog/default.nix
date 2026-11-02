{
  writers,
  python3,
}:

# python for the AES-CCM the router's login needs
(writers.writePython3Bin "vodafone-calllog"
  {
    libraries = [ python3.pkgs.cryptography ];
    flakeIgnore = [ "E501" ];
  }
  ./vodafone-calllog.py
).overrideAttrs
  (old: {
    meta = (old.meta or { }) // {
      description = "Print the call log of the Vodafone Station (Arris TG3442DE)";
      mainProgram = "vodafone-calllog";
    };
  })
