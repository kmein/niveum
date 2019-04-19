{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.python3.withPackages
      (py: [
        py.black
        py.python-language-server
        py.pyls-mypy
        py.flake8
      ])
    )
  ];

  home-manager.users.me.xdg.configFile."pycodestyle".text = ''
    [pycodestyle]
    max-line-length = 110
  '';
}
