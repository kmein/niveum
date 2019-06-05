{ stdenv, buildPythonApplication, fetchPypi, argparse, colour, numpy, pillow, scipy, tqdm, opencv, pycairo, pydub }:
buildPythonApplication rec {
  pname = "manimlib";
  version = "0.1.5";
  src = fetchPypi {
    inherit pname version;
    sha256 = "1hrb94zawngn6pm31185sdg91a66y4lwagwcry3k9a3rk8w81f7m";
  };
  propagatedBuildInputs = [ argparse colour numpy pillow scipy tqdm opencv pycairo pydub ];
  doCheck = false;

  meta = with stdenv.lib; {
    description = "Animation engine for explanatory math videos";
    homepage = "https://github.com/3b1b/manim";
    license = license.mit;
    maintainers = [ maintainers.kmein ];
  };
}
