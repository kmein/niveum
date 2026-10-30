# packaged from flake inputs
{
  llm-agents,
  wetter,
  sis,
  agenix,
  scripts,
  telebots,
  autorenkalender,
  tinc-graph,
}:
final: prev:
let
  inherit (prev.stdenv.hostPlatform) system;
in
{
  claude-code = llm-agents.packages.${system}.claude-code;
  omp = llm-agents.packages.${system}.omp;
  wetter = wetter.packages.${system}.wetter;
  agenix = agenix.packages.${system}.default;
  pun-sort-api = scripts.packages.${system}.pun-sort-api;
  alarm = scripts.packages.${system}.alarm;
  sis = sis.packages.${system}.default;
  telebots = telebots.packages.${system}.telebots;
  hesychius = scripts.packages.${system}.hesychius;
  autorenkalender = autorenkalender.packages.${system}.default;
  onomap = scripts.packages.${system}.onomap;
  tinc-graph = tinc-graph.packages.${system}.tinc-graph;
}
