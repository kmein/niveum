with import <nixpkgs> {};
buildPythonPackage {
  name = "gdom";
  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/76/79/1ccbf38c32576dbb29efdc35819f96a99768266cdf6dc1586aef0e9fbe73/gdom-1.0.0.tar.gz";
    md5 = "f1ec05032cefc74d023fcdf8a24177f6";
  };
  propagatedBuildInputs = with pkgs.python36Packages; [ graphene flask-graphql pyquery requests ];
  meta = with stdenv.lib; {
    description = "GDOM";
    homepage = http://github.com/syrusakbary/gdom;
    license = licenses.mit;
  };
}
