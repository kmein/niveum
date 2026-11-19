{
  lib,
  pkgs,
  ...
}:
let
  # int8 onnx export of nvidia's parakeet-tdt-0.6b-v3: one model for de and en without a
  # language-detection pass, and fast enough on the cpu to leave the mx150 asleep
  model =
    let
      rev = "8f23f0c03c8761650bdb5b40aaf3e40d2c15f1ce";
      file = name: hash: {
        inherit name;
        path = pkgs.fetchurl {
          url = "https://huggingface.co/istupakov/parakeet-tdt-0.6b-v3-onnx/resolve/${rev}/${name}";
          inherit hash;
        };
      };
    in
    pkgs.linkFarm "parakeet-tdt-0.6b-v3-int8" [
      (file "encoder-model.int8.onnx" "sha256-YTnS+n4bCGCXsnfHFJcl7bq4nMfHrmSyPHQb5AVa/wk=")
      (file "decoder_joint-model.int8.onnx" "sha256-7qdIPuPRowN12u3I7YPjlgyRsJiBISeg2Z0ciXdmenA=")
      (file "vocab.txt" "sha256:0pf3wcvps76wq7iadw37lk7xcjs7gpmlbxficg2nmg54krkl91fm")
    ];

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
      # beeps on recording start and stop
      feedback.enabled = true;
    };
    engine = "parakeet";
    parakeet = {
      model = "${model}";
      # auto-detection only recognises the fp32 file names
      model_type = "tdt";
    };
    output = {
      mode = "type";
      # only used by the dotool driver
      dotool_xkb_layout = "de";
      dotool_xkb_variant = "T3";
      # visible sign that it is listening, beside the beep
      notification = {
        on_recording_start = true;
        on_recording_stop = false;
        on_transcription = false;
      };
    };
  };

  voxtype = pkgs.writers.writeDashBin "voxtype" ''
    exec ${
      lib.getExe (pkgs.voxtype.override { onnxSupport = true; })
    } --config ${configFile} "$@"
  '';
in
{
  environment.systemPackages = [ voxtype ];

  # recognition is local; no network, so a config or upstream change cannot send audio away.
  # IPAddressDeny= would be ignored: a user manager may not attach bpf firewalls
  systemd.user.services.voxtype = {
    description = "voxtype dictation daemon";
    # niri imports WAYLAND_DISPLAY into the user manager before it reports ready
    wantedBy = [ "niri.service" ];
    partOf = [ "niri.service" ];
    after = [ "niri.service" ];
    serviceConfig = {
      ExecStart = "${lib.getExe voxtype} daemon";
      Restart = "on-failure";
      # wayland, pipewire and the session bus are path sockets, which a network namespace keeps
      PrivateNetwork = true;
      RestrictAddressFamilies = "AF_UNIX";
    };
  };

  niphas.niri.settings.binds."Mod+S".spawn-sh = "${lib.getExe voxtype} record toggle";
}
