{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.python3.withPackages
      (py: [
        py.black
        py.python-language-server
        py.pyls-mypy
        py.pyls-black
        py.pyls-isort
        py.flake8
        py.flask
        py.pygments
        py.docopt
        py.schema
        py.ansicolors
        py.virtualenv
        py.spacy
        py.spacy_models
      ])
    )
  ];

  home-manager.users.me.xdg.configFile."pycodestyle".text = ''
    [pycodestyle]
    max-line-length = 110
  '';
}
