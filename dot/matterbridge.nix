{ token }: ''
  [general]
  RemoteNickFormat = "{NICK}"

  [telegram]
    [telegram.kmein]
    Token = "${token}"
    MessageFormat = "HTMLNick"

  [irc]
    [irc.freenode]
    Server = "irc.freenode.net:6667"
    Nick = "kmein_bridge"

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
