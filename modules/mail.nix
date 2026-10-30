# Offline-first mail per user, without home-manager.
#
# mbsync keeps a local Maildir in two-way sync with every IMAP account, and
# outgoing mail goes into an msmtpq outbox. `mail-sync` (also run by a user
# timer every 5 minutes) sends the outbox, syncs, and optionally reindexes
# notmuch. Two aerc binaries are built from the same accounts:
#
#   aerc         local Maildir + outbox; works without network
#   aerc-online  direct IMAP/SMTP; escape hatch for when the sync is broken
#
# The programs are configured through wrapper modules (../wrapperModules and
# upstream lassulus/wrappers); nothing is written to $HOME except mail and
# the outbox.
{ wrapperModules }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkOption types;
  cfg = config.niveum.mail;
  nixosConfig = config;
  caBundle = "/etc/ssl/certs/ca-certificates.crt";

  serverOptions = defaultPort: {
    host = mkOption { type = types.str; };
    port = mkOption {
      type = types.port;
      default = defaultPort;
    };
    starttls = mkOption {
      type = types.bool;
      default = false;
      description = "Use STARTTLS instead of implicit TLS.";
    };
  };

  accountModule =
    user:
    { name, config, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          default = name;
          readOnly = true;
          internal = true;
        };
        primary = mkOption {
          type = types.bool;
          default = false;
          description = "Default msmtp account and first aerc tab.";
        };
        address = mkOption { type = types.str; };
        aliases = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };
        realName = mkOption {
          type = types.str;
          default = user.realName;
        };
        userName = mkOption {
          type = types.str;
          default = config.address;
        };
        passwordCommand = mkOption {
          type = types.str;
          description = "Shell command printing the password; used by mbsync, msmtp and aerc-online.";
        };
        imap = serverOptions 993;
        smtp = serverOptions 465;
        folders = {
          inbox = mkOption {
            type = types.str;
            default = "INBOX";
          };
          sent = mkOption {
            type = types.str;
            default = "Sent";
          };
          drafts = mkOption {
            type = types.str;
            default = "Drafts";
          };
        };
        aerc = mkOption {
          type = types.attrsOf types.str;
          default = { };
          example.pgp-key-id = "0123456789ABCDEF";
          description = "Additional accounts.conf keys, for both aerc and aerc-online.";
        };
        maildir = mkOption {
          type = types.str;
          default = "${user.maildir}/${name}";
          readOnly = true;
          internal = true;
        };
      };
    };

  userModule =
    { name, config, ... }:
    {
      options = {
        realName = mkOption { type = types.str; };
        maildir = mkOption {
          type = types.str;
          default = "${nixosConfig.users.users.${name}.home}/Maildir";
          description = "Directory holding one Maildir per account.";
        };
        accounts = mkOption {
          type = types.attrsOf (types.submodule (accountModule config));
          default = { };
        };
        aerc.settings = mkOption {
          type = types.attrsOf (types.attrsOf types.anything);
          default = { };
          description = "aerc.conf sections, shared by aerc and aerc-online.";
        };
        aerc.binds = mkOption {
          type = types.attrsOf (types.attrsOf types.str);
          default = { };
          description = "binds.conf contexts; `global` is written before the first section header.";
        };
        notmuch = {
          enable = lib.mkEnableOption "notmuch indexing after each sync";
          settings = mkOption {
            type = types.attrsOf (types.attrsOf types.anything);
            default = { };
            description = "Merged over the generated notmuch config (database path, user identity).";
          };
        };
      };
    };

  # msmtpq stores its arguments, including the config path, with each queued
  # mail; keep that path stable across generations and garbage collections
  msmtprcPath = user: "/etc/niveum-mail/${user}/msmtprc";

  # exactly one, see the assertion below
  primaryAccount =
    userCfg: lib.findFirst (account: account.primary) null (lib.attrValues userCfg.accounts);

  msmtpq =
    user: userCfg:
    let
      accounts = lib.attrValues userCfg.accounts;
      smtp = account: from: {
        inherit from;
        inherit (account.smtp) host port;
        tls_starttls = account.smtp.starttls;
        user = account.userName;
        passwordeval = account.passwordCommand;
      };
    in
    wrapperModules.msmtpq.apply {
      inherit pkgs;
      settings.defaults = {
        auth = true;
        tls = true;
        tls_trust_file = caBundle;
        # don't let a captive portal hang aerc's :send
        timeout = 20;
      };
      # aliases are accounts of their own, so --read-envelope-from finds them
      settings.accounts = lib.listToAttrs (
        lib.concatMap (
          account:
          [ (lib.nameValuePair account.name (smtp account account.address)) ]
          ++ map (alias: lib.nameValuePair alias (smtp account alias)) account.aliases
        ) accounts
      );
      defaultAccount = (primaryAccount userCfg).name;
      msmtprc.path = msmtprcPath user;
    };

  userPackages =
    user: userCfg:
    let
      outbox = (msmtpq user userCfg).wrapper;
      accounts = lib.attrValues userCfg.accounts;
      primary = primaryAccount userCfg;

      mbsync =
        (wrapperModules.mbsync.apply {
          inherit pkgs;
          settings = {
            IMAPAccount = lib.genAttrs' accounts (
              account:
              lib.nameValuePair account.name {
                Host = account.imap.host;
                Port = account.imap.port;
                User = account.userName;
                PassCmd = account.passwordCommand;
                TLSType = if account.imap.starttls then "STARTTLS" else "IMAPS";
                CertificateFile = caBundle;
              }
            );
            IMAPStore = lib.genAttrs' accounts (
              account: lib.nameValuePair "${account.name}-remote" { Account = account.name; }
            );
            MaildirStore = lib.genAttrs' accounts (
              account:
              lib.nameValuePair "${account.name}-local" {
                Path = "${account.maildir}/";
                Inbox = "${account.maildir}/${account.folders.inbox}";
                SubFolders = "Verbatim";
              }
            );
            Channel = lib.genAttrs' accounts (
              account:
              lib.nameValuePair account.name {
                Far = ":${account.name}-remote:";
                Near = ":${account.name}-local:";
                Patterns = [ "*" ];
                Create = "Both";
                Expunge = "Both";
                Remove = "None";
                SyncState = "*";
              }
            );
          };
        }).wrapper;

      notmuch =
        (wrapperModules.notmuch.apply {
          inherit pkgs;
          settings = lib.recursiveUpdate {
            database.path = userCfg.maildir;
            user = {
              name = primary.realName;
              primary_email = primary.address;
              other_email = lib.concatStringsSep ";" (
                lib.concatMap (account: [ account.address ] ++ account.aliases) accounts
              );
            };
          } userCfg.notmuch.settings;
        }).wrapper;

      mailSync = pkgs.writers.writeDashBin "mail-sync" ''
        set -e
        ${lib.concatMapStrings (account: "mkdir -p ${lib.escapeShellArg account.maildir}\n") accounts}
        # send the outbox first; a failed send must not prevent the sync
        ${outbox}/bin/msmtp-queue -r || true
        ${lib.getExe mbsync} --all
        ${lib.optionalString userCfg.notmuch.enable "${lib.getExe notmuch} new"}
      '';

      common =
        account:
        lib.filterAttrs (_: v: v != [ ]) {
          from = "${account.realName} <${account.address}>";
          inherit (account) aliases;
          default = account.folders.inbox;
          copy-to = account.folders.sent;
          postpone = account.folders.drafts;
        };

      offlineAccount =
        account:
        common account
        // {
          source = "maildir://${account.maildir}";
          outgoing = "${outbox}/bin/msmtpq --read-envelope-from --read-recipients";
          check-mail-cmd = "${pkgs.systemd}/bin/systemctl --user start --no-block mail-sync.service";
        }
        // account.aerc;

      onlineAccount =
        account:
        let
          url =
            protocol: server:
            "${protocol}${
              lib.optionalString (!server.starttls) "s"
            }://${lib.escapeURL account.userName}@${server.host}:${toString server.port}";
        in
        common account
        // {
          source = url "imap" account.imap;
          source-cred-cmd = account.passwordCommand;
          outgoing = url "smtp" account.smtp;
          outgoing-cred-cmd = account.passwordCommand;
        }
        // account.aerc;

      aerc =
        mkAccount:
        wrapperModules.aerc.apply {
          inherit pkgs;
          inherit (userCfg.aerc) settings binds;
          accounts = lib.genAttrs' accounts (account: lib.nameValuePair account.name (mkAccount account));
          accountOrder = [ primary.name ];
        };
    in
    [
      (aerc offlineAccount).wrapper
      ((aerc onlineAccount).apply {
        binName = "aerc-online";
        # don't take over `aerc mailto:…` from a running offline aerc
        flags."--no-ipc" = true;
        # the offline aerc provides these; identical symlinks (man pages,
        # filters) are fine in the same profile, differing files collide
        filesToExclude = [
          "bin/aerc"
          "share/applications/*"
        ];
      }).wrapper
      mailSync
      outbox
    ]
    ++ lib.optional userCfg.notmuch.enable notmuch;

  # one unit for every user; it only runs for users with a mail profile
  mailSyncPath = "/etc/profiles/per-user/%u/bin/mail-sync";
in
{
  options.niveum.mail.users = mkOption {
    type = types.attrsOf (types.submodule userModule);
    default = { };
    description = "Offline-first mail setup per user (keys are `users.users` attribute names).";
  };

  config = lib.mkIf (cfg.users != { }) {
    assertions = lib.mapAttrsToList (user: userCfg: {
      assertion = lib.count (account: account.primary) (lib.attrValues userCfg.accounts) == 1;
      message = "niveum.mail.users.${user}: exactly one account must be primary.";
    }) cfg.users;

    users.users = lib.mapAttrs (user: userCfg: { packages = userPackages user userCfg; }) cfg.users;

    environment.etc = lib.mapAttrs' (
      user: userCfg:
      lib.nameValuePair "niveum-mail/${user}/msmtprc" { text = (msmtpq user userCfg).msmtprc.content; }
    ) cfg.users;

    systemd.user.services.mail-sync = {
      description = "Send the mail outbox and sync the local Maildir";
      unitConfig.ConditionPathExists = mailSyncPath;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = mailSyncPath;
      };
    };
    systemd.user.timers.mail-sync = {
      wantedBy = [ "timers.target" ];
      unitConfig.ConditionPathExists = mailSyncPath;
      timerConfig.OnCalendar = "*:0/5";
    };
  };
}
