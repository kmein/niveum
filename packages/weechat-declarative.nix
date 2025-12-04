{
  pkgs,
  lib,
  ...
} @ args: let
  # config cannot be declared in the input attribute set because that would
  # cause callPackage to inject the wrong config.  Instead, get it from ...
  # via args.
  config = args.config or {};

  lib =
    args.lib
    // rec {
      attrPaths = let
        recurse = path: value:
          if builtins.isAttrs value
          then lib.mapAttrsToList (name: recurse (path ++ [name])) value
          else [(lib.nameValuePair path value)];
      in
        attrs: lib.flatten (recurse [] attrs);

      attrPathsSep = sep: attrs: lib.listToAttrs (map (x: x // {name = lib.concatStringsSep sep x.name;}) (attrPaths attrs));

      toWeechatValue = x:
        {
          bool = builtins.toJSON x;
          string = x;
          list = lib.concatMapStringsSep "," toWeechatValue x;
          int = toString x;
        }
        .${builtins.typeOf x};

      setCommand = name: value: "/set ${name} \"${toWeechatValue value}\"";

      filterAddreplace = name: filter: "/filter addreplace ${name} ${filter.buffer} ${toWeechatValue filter.tags} ${filter.regex}";
    };

  cfg = eval.config;

  eval = lib.evalModules {
    modules = lib.singleton {
      _file = toString ./default.nix;
      imports = lib.singleton config;
      options = {
        scripts = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          default = [];
          description = ''
            some stuff from pkgs.weechatScripts
          '';
        };
        settings = lib.mkOption {
          type = (pkgs.formats.json {}).type;
          description = ''
            your weechat config in nix-style syntax.
            secrets can be defined with \''${my.secret.value}
          '';
          default = {};
          example = {
            irc.server_default.nicks = "rick_\\\${sec.data.foo}";
            irc.server_default.msg_part = "ciao kakao";
            irc.server_default.msg_quit = "tschö mit \\\${sec.data.foo}";
            irc.look.color_nicks_in_nicklist = true;
            matrix.server.nibbana = {
              address = "nibbana.jp";
            };
            irc.server.hackint = {
              address = "irc.hackint.org/6697";
              ssl = true;
              autoconnect = true;
              autojoin = ["#krebs"];
            };
            weechat.bar.buflist.hidden = true;
            irc.server.hackint.command = lib.concatStringsSep "\\;" [
              "/msg nickserv IDENTIFY \\\${sec.data.hackint_password}"
              "/msg nickserv SET CLOAK ON"
            ];
            filters.playlist_topic = {
              buffer = "irc.*.#the_playlist";
              tags = ["irc_topic"];
              regex = "*";
            };
            relay = {
              port.weechat = 9000;
              network.password = "hunter2";
            };
            alias.cmd.mod = "quote omode $channel +o $nick";
            secure.test.passphrase_command = "echo lol1234123124";
          };
        };
        extraCommands = lib.mkOption {
          type = lib.types.lines;
          default = "";
        };
        files = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = {};
          example = lib.literalExpression ''
            {
              "sec.conf" = toString (pkgs.writeText "sec.conf" '''
                [crypt]
                cipher = aes256
                hash_algo = sha256
                passphrase_command = ""
                salt = on

                [data]
                __passphrase__ = off
                foo = "bar"
              ''');
            }
          '';
        };
      };
    };
  };

  setFile = pkgs.writeText "weechat.set" (
    lib.optionalString (cfg.settings != {})
    (lib.concatStringsSep "\n" (
      lib.optionals
      (cfg.settings.irc or {} != {})
      (lib.mapAttrsToList
        (name: server: "/server add ${name} ${lib.toWeechatValue server.addresses}")
        cfg.settings.irc.server)
      ++ lib.optionals
      (cfg.settings.matrix or {} != {})
      (lib.mapAttrsToList
        (name: server: "/matrix server add ${name} ${server.address}")
        cfg.settings.matrix.server)
      ++ lib.mapAttrsToList lib.setCommand (lib.attrPathsSep "." cfg.settings)
      ++ lib.optionals
      (cfg.settings.filters or {} != {})
      (lib.mapAttrsToList lib.filterAddreplace cfg.settings.filters)
      ++ lib.singleton cfg.extraCommands
    ))
  );

  weechatPkg = pkgs.weechat.override {
    configure = _: {
      init = "/exec -oc cat ${setFile}";

      scripts = cfg.scripts;
    };
  };

  wrapper = pkgs.writers.writeDashBin "weechat" ''
    CONFDIR=''${XDG_CONFIG_HOME:-$HOME/.config}/weechat
    ${pkgs.coreutils}/bin/mkdir -p "$CONFDIR"
    ${
      lib.concatStringsSep "\n"
      (
        lib.mapAttrsToList
        (name: target:
          /*
          sh
          */
          ''
            ${pkgs.coreutils}/bin/cp ${lib.escapeShellArg target} "$CONFDIR"/${lib.escapeShellArg name}
            ${pkgs.coreutils}/bin/chmod -w "$CONFDIR"/${lib.escapeShellArg name}
          '')
        cfg.files
      )
    }
    exec ${weechatPkg}/bin/weechat "$@"
  '';
in
  pkgs.symlinkJoin {
    name = "weechat-configured";
    paths = [
      wrapper
      weechatPkg
    ];
    postBuild = ''
      ln -s ${setFile} $out/weechat.set
    '';
  }
