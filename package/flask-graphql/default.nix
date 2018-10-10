with import <nixpkgs> {};
buildPythonPackage {
  name = "flask-graphql";
  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/d6/8c/043bd1deec422d33c31961dd93d952236326b1e5bbca6d3479383360ac42/Flask-GraphQL-2.0.0.tar.gz";
    md5 = "72dacc65c7879d4d84d871fff5d5cf0c";
  };
  propagatedBuildInputs = with pkgs.python36Packages; [ graphql-core flask graphql-server-core ];
  meta = with stdenv.lib; {
    description = "Flask-GraphQL";
    homepage = https://github.com/graphql-python/flask-graphql;
    license = licenses.mit;
  };
}
