{ stdenv, makeWrapper, pandoc, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "daybook";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "daybook";
    rev = "370c62bc19d514efc55451fca19d6aa26ba5e893";
    sha256 = "0dqah4ml561xbizkbah0s7n4mqn7y5dcpwbp3x7cj5ypr7y225gp";
  };
  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ pandoc ];
  buildPhase = ''
    pandoc --standalone --to man daybook.1.md -o daybook.1
  '';
  installPhase = ''
    mkdir -p $out/{bin,share/man/man1}
    install daybook.1 $out/share/man/man1
    install daybook $out/bin
    wrapProgram $out/bin/daybook --prefix PATH ":" ${pandoc}/bin ;
  '';
  meta = with stdenv.lib; {
    homepage = https://github.com/kmein/daybook;
    description = "A diary writing utility in sh";
    license = licenses.mit;
  };
}
