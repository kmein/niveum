{ pkgs, ... }: {
  environment.systemPackages = [
    (pkgs.python3.withPackages (py: [
      py.black
      # py.python-language-server
      # py.pyls-mypy
      # py.pyls-black
      # py.pyls-isort
      py.flake8
      py.pygments
      py.schema
    ]))
    pkgs.python3Packages.poetry
  ];

  home-manager.users.me.xdg.configFile."pycodestyle".text = ''
    [pycodestyle]
    max-line-length = 110
  '';
}
