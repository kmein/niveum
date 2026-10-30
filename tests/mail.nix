# modules/mail.nix end to end: alice and bob on one client machine, a maddy
# server with IMAPS/SMTPS. Checks that aerc-online talks to the server
# directly, and that the offline aerc reads the local Maildir and queues
# outgoing mail while the network is down, delivering it on the next sync.
{
  self,
  pkgs,
}:
let
  certs = import "${pkgs.path}/nixos/tests/common/acme/server/snakeoil-certs.nix";
  domain = certs.domain;
  passwordFile = pkgs.writeText "mail-password" "hunter2";
  users = {
    alice = 1000;
    bob = 1001;
  };
in
pkgs.testers.runNixOSTest {
  name = "mail";

  nodes.server =
    { options, ... }:
    {
      services.maddy = {
        enable = true;
        hostname = domain;
        primaryDomain = domain;
        ensureAccounts = map (user: "${user}@${domain}") (builtins.attrNames users);
        ensureCredentials = pkgs.lib.mapAttrs' (
          user: _: pkgs.lib.nameValuePair "${user}@${domain}" { passwordFile = "${passwordFile}"; }
        ) users;
        tls = {
          loader = "file";
          certificates = [
            {
              certPath = "${certs.${domain}.cert}";
              keyPath = "${certs.${domain}.key}";
            }
          ];
        };
        # the module only opens plaintext listeners
        config =
          builtins.replaceStrings
            [ "imap tcp://0.0.0.0:143" "submission tcp://0.0.0.0:587" ]
            [ "imap tls://0.0.0.0:993" "submission tls://0.0.0.0:465" ]
            options.services.maddy.config.default;
      };
      networking.firewall.allowedTCPPorts = [
        993
        465
      ];
    };

  nodes.client =
    { nodes, lib, ... }:
    {
      imports = [ self.nixosModules.mail ];
      security.pki.certificateFiles = [ certs.ca.cert ];
      networking.extraHosts = "${nodes.server.networking.primaryIPAddress} ${domain}";
      environment.systemPackages = [
        pkgs.tmux
        pkgs.curl
      ];

      users.users = lib.mapAttrs (_: uid: {
        isNormalUser = true;
        inherit uid;
        linger = true;
      }) users;

      niveum.mail.users = lib.mapAttrs (user: _: {
        realName = user;
        aerc.binds."compose::review".y = ":send<Enter>";
        accounts.test = {
          primary = true;
          address = "${user}@${domain}";
          passwordCommand = "cat ${passwordFile}";
          imap.host = domain;
          smtp.host = domain;
        };
      }) users;
    };

  testScript = ''
    import shlex

    uids = ${builtins.toJSON users}

    def as_user(user, command):
        uid = uids[user]
        env = f"export XDG_RUNTIME_DIR=/run/user/{uid} DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/{uid}/bus"
        return f"su - {user} -c " + shlex.quote(f"{env}; {command}")

    def sync(user):
        client.succeed(as_user(user, "systemctl --user start mail-sync.service"))

    def maildir_has(user, folder, subject):
        return f"grep -rlF 'Subject: {subject}' /home/{user}/Maildir/test/{folder}/cur /home/{user}/Maildir/test/{folder}/new"

    def server_has(user, folder, subject):
        # IMAP SEARCH straight against the server, bypassing the client's sync
        return (
            f"curl --silent --show-error --user {user}@${domain}:$(cat ${passwordFile}) "
            f"'imaps://${domain}/{folder}' --request 'SEARCH SUBJECT {subject}' | grep -E '^\\* SEARCH [0-9]+'"
        )

    def screen(user, session):
        return client.succeed(as_user(user, f"tmux capture-pane -p -t {session}"))

    def wait_for_screen(user, session, text):
        try:
            client.wait_until_succeeds(
                as_user(user, f"tmux capture-pane -p -t {session} | grep -F {shlex.quote(text)}"),
                timeout=60,
            )
        except Exception:
            print(screen(user, session))
            raise

    def aerc(user, session, binary, *args):
        command = " ".join([binary, *map(shlex.quote, args)])
        client.succeed(as_user(user, f"tmux new-session -d -s {session} -x 200 -y 50 -e EDITOR=true {shlex.quote(command)}"))

    def quit_aerc(user, session):
        client.succeed(as_user(user, f"tmux kill-session -t {session}"))

    def send(user, binary, to, subject):
        session = f"send-{subject}"
        aerc(user, session, binary, f"mailto:{to}@${domain}?subject={subject}&body=hello")
        # the editor (`true`) exits at once, leaving the review screen
        wait_for_screen(user, session, ":send")
        client.succeed(as_user(user, f"tmux send-keys -t {session} y"))
        wait_for_screen(user, session, "Message sent")
        quit_aerc(user, session)

    start_all()
    server.wait_for_unit("maddy.service")
    server.wait_for_open_port(993)
    server.wait_for_open_port(465)
    # oneshot without RemainAfterExit; idempotent, so run it to completion here
    server.succeed("systemctl start maddy-ensure-accounts.service")

    for user, uid in uids.items():
        client.wait_for_unit(f"user@{uid}.service")
        sync(user)

    with subtest("aerc-online sends via SMTP and files the copy via IMAP"):
        send("alice", "aerc-online", "bob", "via-online")
        client.succeed(server_has("bob", "INBOX", "via-online"))
        client.succeed(server_has("alice", "Sent", "via-online"))

    with subtest("aerc-online lists the server's INBOX"):
        aerc("bob", "online", "aerc-online")
        wait_for_screen("bob", "online", "via-online")
        quit_aerc("bob", "online")

    sync("bob")
    client.succeed("ip link set eth1 down")

    with subtest("offline aerc reads the local Maildir"):
        # proves we really are offline
        client.fail(as_user("bob", "systemctl --user start mail-sync.service"))
        aerc("bob", "offline", "aerc")
        wait_for_screen("bob", "offline", "via-online")
        quit_aerc("bob", "offline")

    with subtest("offline aerc queues outgoing mail"):
        send("alice", "aerc", "bob", "via-outbox")
        client.succeed(as_user("alice", "msmtp-queue -d | grep -F via-outbox"))
        client.succeed(maildir_has("alice", "Sent", "via-outbox"))
        # a sync attempt while offline must keep the mail queued
        client.fail(as_user("alice", "systemctl --user start mail-sync.service"))
        client.succeed(as_user("alice", "msmtp-queue -d | grep -F via-outbox"))

    client.succeed("ip link set eth1 up")
    client.wait_until_succeeds("ping -c1 ${domain}")

    with subtest("the next sync sends the outbox and uploads the Sent copy"):
        sync("alice")
        client.fail(as_user("alice", "msmtp-queue -d | grep -F via-outbox"))
        client.succeed(server_has("bob", "INBOX", "via-outbox"))
        client.succeed(server_has("alice", "Sent", "via-outbox"))
        sync("bob")
        client.succeed(maildir_has("bob", "INBOX", "via-outbox"))
  '';
}
