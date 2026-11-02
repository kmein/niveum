{
  lib,
  writers,
  python3,
  pass,
  # null leaves only VODAFONE_STATION_PASSWORD and the prompt
  passwordCommand ? "${lib.getExe pass} show web/192.168.0.1",
}:

# python for the AES-CCM the router's login needs
(writers.writePython3Bin "vodafone-calllog"
  {
    libraries = [ python3.pkgs.cryptography ];
    flakeIgnore = [ "E501" ];
    # a script path, because the writer word-splits makeWrapperArgs
    makeWrapperArgs = lib.optionals (passwordCommand != null) [
      "--set-default"
      "VODAFONE_STATION_PASSWORD_COMMAND"
      (writers.writeDash "vodafone-station-password" passwordCommand)
    ];
  }
  ./vodafone-calllog.py
).overrideAttrs
  (old: {
    meta = (old.meta or { }) // {
      description = "Print the call log of the Vodafone Station (Arris TG3442DE)";
      mainProgram = "vodafone-calllog";
    };
  })
