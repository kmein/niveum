{ token }: {
  general = {
    RemoteNickFormat = "[{NOPINGNICK}] ";
    Charset = "utf-8";
  };
  telegram.kmein.Token = token;
  irc.freenode = {
    Server = "irc.freenode.net:6667";
    Nick = "tg_bridge";
  };
  gateway = [
    {
      name = "krebs-bridge";
      enable = true;
      inout = [
        {
          account = "irc.freenode";
          channel = "#krebs";
        }
        {
          account = "telegram.kmein";
          channel = "-330372458";
        }
      ];
    }
    {
      name = "myengadin-bridge";
      enable = true;
      inout = [
        {
          account = "irc.freenode";
          channel = "##myengadin";
        }
        {
          account = "telegram.kmein";
          channel = "-425759153";
        }
      ];
    }
  ];
}