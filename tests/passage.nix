# packages/passage's unlock without a terminal, as rofi-passage does it: the
# unlocked identity decrypts, each use re-arms its expiry timer, it is dropped
# once unused for lockAfter or on passage lock, and unlocking works again after both.
{
  self,
  pkgs,
}:
let
  passage = self.packages.${pkgs.stdenv.hostPlatform.system}.passage.override {
    lockAfter = "20s";
  };
  t = pkgs.writeShellScriptBin "t" ''
    set -eu
    export XDG_RUNTIME_DIR=/run/user/$(id -u)
    case $1 in
      setup)
        ssh-keygen -q -t ed25519 -N 'sekrit pw' -f ~/id
        mkdir -p ~/.passage/store
        ln -s ../id ~/.passage/identities
        cat ~/id.pub > ~/.passage/store/.age-recipients
        printf 'hunter2\n' | passage insert -m web/x >/dev/null
        printf '#!/bin/sh\necho "sekrit pw"\n' > ~/askpass
        chmod +x ~/askpass
        ;;
      unlock) SSH_ASKPASS=~/askpass SSH_ASKPASS_REQUIRE=force passage unlock ;;
      show) passage show web/x ;;
      lock) passage lock ;;
      unlocked) test -f "$XDG_RUNTIME_DIR/passage/identities" ;;
      timer) systemctl --user is-active passage-lock.timer ;;
    esac
  '';
in
pkgs.testers.runNixOSTest {
  name = "passage";

  nodes.machine = {
    users.users.alice = {
      isNormalUser = true;
      # a user manager for the timer, without a login session
      linger = true;
    };
    environment.systemPackages = [
      passage
      t
      pkgs.openssh
    ];
  };

  testScript = ''
    import time

    def alice(cmd):
        # no terminal, as under the compositor
        return f"su - alice -c 't {cmd}' </dev/null"

    machine.wait_for_unit("user@1000.service")
    machine.succeed(alice("setup"))
    machine.fail(alice("show"))

    machine.succeed(alice("unlock"))
    machine.succeed(alice("timer"))
    assert machine.succeed(alice("show")) == "hunter2\n"

    with subtest("each use re-arms the expiry"):
        time.sleep(12)
        machine.succeed(alice("show"))
        time.sleep(12)
        machine.succeed(alice("unlocked"))

    with subtest("unused, the identity is dropped"):
        machine.wait_until_fails(alice("unlocked"), timeout=15)
        machine.fail(alice("show"))

    with subtest("lock drops it at once"):
        machine.succeed(alice("unlock"))
        machine.succeed(alice("lock"))
        machine.fail(alice("unlocked"))
        machine.fail(alice("timer"))
        machine.succeed(alice("lock"))

    machine.succeed(alice("unlock"))
    machine.succeed(alice("timer"))
    machine.succeed(alice("show"))
  '';
}
