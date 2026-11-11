# iamb against the homeserver on ful (systems/ful/matrix.nix)
{
  pkgs,
  self,
  ...
}:
let
  iamb =
    (self.wrapperModules.iamb.apply {
      inherit pkgs;
      settings = {
        default_profile = "kmein";
        profiles.kmein = {
          user_id = "@kmein:chat.kmein.r";
          url = "https://chat.kmein.r";
        };
        # newest message first, like the phone apps; chats and dms follow rooms
        settings.sort.rooms = [
          "favorite"
          "lowpriority"
          "recent"
        ];
      };
    }).wrapper;
in
{
  environment.systemPackages = [ iamb ];
}
