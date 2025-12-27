let
  lib = import <nixpkgs/lib>;
in
rec {
  inherit lib;

  input = [
    {
      x = [
        "pool"
        "zfs"
      ];
      y = [
        "mdadm"
        "raid1"
      ];
    }
    {
      x = [
        "pool"
        "zfs"
      ];
      y = [
        "disk"
        "sda"
      ];
    }
    {
      x = [
        "mdadm"
        "raid1"
      ];
      y = [
        "disk"
        "sdb"
      ];
    }
    {
      x = [
        "mdadm"
        "raid1"
      ];
      y = [
        "disk"
        "sdc"
      ];
    }
  ];

  outNodes = node: graph: lib.unique (builtins.map (e: e.y) (builtins.filter (v: v.x == node) graph));

  vertices = graph: lib.unique (builtins.map (x: x.y) graph ++ builtins.map (x: x.x) graph);

  deleteVertex = node: graph: (builtins.filter (v: v.x != node && v.y != node) graph);

  findSink =
    graph:
    lib.findFirst (v: outNodes v graph == [ ]) (lib.trace graph (builtins.abort "No sink found")) (
      vertices graph
    );

  topSort =
    graph:
    if graph == [ ] then
      [ ]
    else if builtins.length graph == 1 then
      let
        only = builtins.head graph;
      in
      [
        only.y
        only.x
      ]
    else
      let
        sink = findSink graph;
      in
      [ sink ] ++ topSort (deleteVertex sink graph);

  output = topSort input;
}
