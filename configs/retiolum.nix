{
  config,
  ...
}:
{
  networking.hosts = {
    # "42:0:ca48:f98f:63d7:31ce:922b:245d" = [ "go" ];
    # prism is down; cyberlocker (c.r), paste (p.r) and wallpaper.r moved to
    # neoprism, but kartei still lists them as prism aliases
    # (lass/hosts/prism/retiolum/aliases).
    # paste.r is deliberately absent: neoprism has no vhost for it.
    "42:0:ce16::99" = [
      "c.r"
      "p.r"
      "wallpaper.r"
    ];
    "10.243.0.99" = [
      "c.r"
      "p.r"
      "wallpaper.r"
    ];
    # onomap (names.kmein.r) moved from makanek to ful, but kartei still
    # lists the alias under makanek; override until that entry is updated.
    "42:0:3c46:2c8b:a564:1213:9fb4:1bc4" = [ "names.kmein.r" ];
    "10.243.2.107" = [ "names.kmein.r" ];
  };

  # kartei's connectTo still names prism, which no longer speaks tinc;
  # neoprism took over as hub, and yellow (behind nat) hangs off it
  services.tincr.networks.retiolum = {
    connectTo = [ "neoprism" ];
    hosts.neoprism = ''
      Address = 95.217.192.59 655
      Address = 2a01:4f9:4a:4f1a::2 655
    '';
  };

  # tincr speaks only sptps, so the ed25519 key is the only one it reads
  age.secrets.retiolum-ed25519 = {
    file = ../secrets/${config.networking.hostName}-retiolum-privateKey-ed25519.age;
    mode = "400";
  };

  networking.retiolum.ed25519PrivateKeyFile = config.age.secrets.retiolum-ed25519.path;
}
