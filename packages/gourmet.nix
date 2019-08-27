# { stdenv, python }:
{ pkgs ? import <nixpkgs> {} }:
with pkgs; with pkgs.python2Packages;

buildPythonApplication rec {
  pname = "gourmet";
  version = "0.17.4";
  src = builtins.fetchTarball {
    url = "https://github.com/thinkle/gourmet/archive/${version}.tar.gz";
  };
  buildInputs = [ distutils_extra intltool ];
  propagatedBuildInputs = [ sqlalchemy reportlab lxml ];
  meta = with stenv.lib; {
    maintainers = with maintainers; [ kmein ];
  };
}
