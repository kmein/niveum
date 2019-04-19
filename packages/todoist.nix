{ buildGoModule, fetchFromGitHub, lib }:
buildGoModule rec {
  name = "todoist-${version}";
  version = "master";

  src = fetchFromGitHub {
    owner = "sachaos";
    repo = "todoist";
    rev = "8c17d7ebf9be78cce24414b5aa3d9c8f1098e506";
    sha256 = "1r5kn5sx9g9rvx78cqfh19mqwf7370vk102x1n062whryk1j3562";
  };

  modSha256 = "09n6abyaqwz4zcdz8934rvpbxhp4v2nmm5v739kkcc98c3h93i64";

  meta = with lib; {
    description = "Todoist CLI Client";
    homepage = "https://github.com/sachaos/todoist";
    license = licenses.mit;
    platforms = platforms.linux ++ platforms.darwin;
  };
}
