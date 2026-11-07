{
  lib,
  pkgs,
  ...
}:
let
  model = pkgs.fetchurl {
    url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-small.bin";
    sha256 = "1be3a9b2063867b937e64e2ec7483364a79917e157fa98c5d94b5c1fffea987b";
  };

  configFile = (pkgs.formats.toml { }).generate "voxtype.toml" {
    # required by `voxtype record toggle`
    state_file = "auto";
    # the niri bind drives recording; the evdev hotkey would need the input group, which the agent sandbox shares
    hotkey.enabled = false;
    # no serde defaults for these, the section must be complete
    audio = {
      device = "default";
      sample_rate = 16000;
      max_duration_secs = 60;
    };
    whisper = {
      model = "${model}";
      language = [
        "de"
        "en"
      ];
      # encodes short clips with a proportionally smaller window instead of the full 30s; the
      # repetition loops it can cause are reported for large-v3, not small
      context_window_optimization = true;
    };
    output = {
      mode = "type";
      # only used by the dotool driver
      dotool_xkb_layout = "de";
      dotool_xkb_variant = "T3";
      # toggle mode gives no other sign that it is listening
      notification = {
        on_recording_start = true;
        on_recording_stop = false;
        on_transcription = false;
      };
    };
  };

  voxtype = pkgs.writers.writeDashBin "voxtype" ''
    # mx150 next to the intel igpu; whisper.cpp would otherwise pick device 0
    export VOXTYPE_VULKAN_DEVICE=nvidia
    exec ${
      lib.getExe (pkgs.voxtype.override { vulkanSupport = true; })
    } --config ${configFile} "$@"
  '';
in
{
  environment.systemPackages = [ voxtype ];

  # mkDefault: niphas sets its startup list at mkDefault, a normal-priority definition would replace it
  niphas.niri.settings.spawn-at-startup = lib.mkDefault [
    [
      (lib.getExe voxtype)
      "daemon"
    ]
  ];

  niphas.niri.settings.binds."Mod+S".spawn-sh = "${lib.getExe voxtype} record toggle";
}
