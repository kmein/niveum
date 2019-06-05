{
  imports = [ modules/git.nix ];

  niveum.git = {
    enable = true;
    repositories.niveum = {
      enable = true;
      location = "/home/kfm/prog/git/niveum";
      remotes.origin = https://github.com/kmein/niveum;
      branches = [ "master" ];
      autoFetch = [ { remote = "origin"; branch = "master"; } ];
    };
  };
}
