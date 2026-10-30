# msmtpq: msmtp behind a queue. Mail that can't be sent right away stays in
# the queue until `msmtp-queue -r` (or the next msmtpq call) gets it out.
{
  config,
  lib,
  wlib,
  ...
}:
let
  inherit (lib) mkOption types;
  value = types.oneOf [
    types.str
    types.int
    types.bool
  ];
  render = v: if lib.isBool v then (if v then "on" else "off") else toString v;
  section =
    header: settings:
    "${header}\n" + lib.concatStrings (lib.mapAttrsToList (k: v: "${k} ${render v}\n") settings);
in
{
  _class = "wrapper";
  options = {
    settings = {
      defaults = mkOption {
        type = types.attrsOf value;
        default = { };
        description = "Settings for all accounts, see msmtp(1).";
      };
      accounts = mkOption {
        type = types.attrsOf (types.attrsOf value);
        default = { };
        description = "Accounts by name; `--read-envelope-from` picks the one whose `from` matches.";
      };
    };
    defaultAccount = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Account used when no `from` matches.";
    };
    msmtprc = mkOption {
      type = wlib.types.file config.pkgs;
      description = ''
        The path ends up in every queued mail's msmtp arguments, so a queue
        that may outlive a garbage collection needs it at a stable location
        (`msmtprc.path = "/etc/…"`) instead of the store.
      '';
    };
    queue = {
      dir = mkOption {
        type = types.str;
        default = "$HOME/.local/state/msmtp/queue";
      };
      log = mkOption {
        type = types.str;
        default = "$HOME/.local/state/msmtp/queue.log";
      };
    };
  };
  config = {
    package = lib.mkDefault config.pkgs.msmtp;
    exePath = "${config.package}/bin/msmtpq";
    # mkDefault, so that setting `msmtprc.content` replaces it
    msmtprc.content = lib.mkDefault (
      lib.concatStringsSep "\n" (
        lib.optional (config.settings.defaults != { }) (section "defaults" config.settings.defaults)
        ++ lib.mapAttrsToList (name: section "account ${name}") config.settings.accounts
        ++ lib.optional (config.defaultAccount != null) "account default : ${config.defaultAccount}\n"
      )
    );
    env = {
      MSMTPQ_Q = config.queue.dir;
      MSMTPQ_LOG = config.queue.log;
      # no ping probe (fails on ICMP-filtering networks): try to send and
      # leave the mail queued if that fails
      EMAIL_CONN_TEST = "x";
    };
    # msmtpq passes its arguments on to msmtp, except in queue management
    # mode, which must stay the first argument
    preHook = ''
      if [ "''${1-}" != --q-mgmt ]; then
        set -- --file=${config.msmtprc.path} "$@"
      fi
    '';
    # upstream msmtp-queue execs the unwrapped msmtpq by store path, which
    # would manage a different queue
    filesToExclude = [
      "bin/msmtp-queue"
      "bin/msmtpq.orig"
    ];
    patchHook = ''
      cat > $out/bin/msmtp-queue <<EOF
      #!${config.pkgs.runtimeShell}
      exec $out/bin/msmtpq --q-mgmt "\$@"
      EOF
      chmod +x $out/bin/msmtp-queue
    '';
  };
}
