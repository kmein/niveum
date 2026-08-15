# packaged from flake inputs
{
  opencrow,
  llm-agents,
  wetter,
  agenix,
  scripts,
  telebots,
  autorenkalender,
  tinc-graph,
  ephemeris-service,
}:
final: prev:
let
  inherit (prev.stdenv.hostPlatform) system;
in
{
  opencrow = opencrow.packages.${system}.opencrow.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      # omp emits fractional delayMs in auto_retry_start events, which
      # opencrow's int field rejects as malformed JSON
      ../packages/opencrow/delayms-float.patch
    ];
  });
  omp = llm-agents.packages.${system}.omp;
  claude-code = llm-agents.packages.${system}.claude-code;
  wetter = wetter.packages.${system}.wetter;
  agenix = agenix.packages.${system}.default;
  pun-sort-api = scripts.packages.${system}.pun-sort-api;
  alarm = scripts.packages.${system}.alarm;
  telebots = telebots.packages.${system}.telebots;
  hesychius = scripts.packages.${system}.hesychius;
  autorenkalender = autorenkalender.packages.${system}.default;
  onomap = scripts.packages.${system}.onomap;
  tinc-graph = tinc-graph.packages.${system}.tinc-graph;
  ephemeris-service = ephemeris-service.packages.${system}.ephemeris-service;
  ephemeris-tray = ephemeris-service.packages.${system}.ephemeris-tray;
}
