{ jq, runCommand, fetchFromGitHub }:
let
  online-radio = fetchFromGitHub {
    owner = "josiahmokob0";
    repo = "online-radio";
    rev = "04d16421355196194a05e5504ba8f66a9c07ee7b";
    sha256 = "1q0iagx7df0sd6vl5anvpzyiw4jdwa6c67z45rx622a6cr6m4zzl";
  };
in runCommand "worldradio.m3u" {} ''
  ${jq}/bin/jq --raw-output --slurp 'flatten | map(.url_resolved) | .[]' ${online-radio}/src/data/countries/*.json \
    | sort \
    | uniq \
    > $out
''

# anthoer method for running a world radio using Icecast Directory
# curl http://dir.xiph.org/ | pup 'a[href^=http]:contains("Play") attr{href}'
