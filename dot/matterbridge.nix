{ token }: ''
  [general]
  RemoteNickFormat = "[{NOPINGNICK}] "

  [telegram]
    [telegram.kmein]
    Token = "${token}"

  [irc]
    [irc.freenode]
    Server = "irc.freenode.net:6667"
    Nick = "tg_bridge"

  [[gateway]]
  name = "krebs-bridge"
  enable = true

    [[gateway.inout]]
    account = "irc.freenode"
    channel = "#krebs"

    [[gateway.inout]]
    account = "telegram.kmein"
    channel = "-330372458"
''
