# senpai against the soju bouncer on makanek (systems/makanek/soju.nix)
{
  config,
  pkgs,
  self,
  ...
}:
let
  senpai =
    (self.wrapperModules.senpai.apply {
      inherit pkgs;
      settings = {
        # soju listens on retiolum only
        address = "ircs://makanek.kmein.r";
        nickname = "kmein";
        # soju keeps a read marker and history position per client name
        username = "kmein@${config.networking.hostName}";
        password-cmd = [
          "${pkgs.coreutils}/bin/cat"
          config.age.secrets.soju-password.path
        ];
        colors.nicks = "extended";
      };
    }).wrapper;
in
{
  age.secrets.soju-password = {
    file = ../secrets/soju-password.age;
    owner = config.users.users.me.name;
    group = config.users.users.me.group;
    mode = "400";
  };

  environment.systemPackages = [
    senpai
    (pkgs.writers.writeDashBin "chats" ''
      ${pkgs.openssh}/bin/ssh makanek "cd /var/lib && grep --ignore-case --color=always --recursive $@ soju/logs weechat/logs" | ${pkgs.less}/bin/less --raw-control-chars
    '')
  ];

  environment.shellAliases.im = "senpai";
}
