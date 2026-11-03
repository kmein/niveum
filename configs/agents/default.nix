# instructions and skills for claude code; omp reads them from ~/.claude too.
# proprietary skills stay unmanaged in ~/.claude/skills, out of this public repo.
#
# the agents run in sbox (bubblewrap + pasta): they see the git checkout they
# were started in, their own state dirs and /nix, but not ~/.ssh, the ssh and
# gpg agents, pass, /run/agenix, the session bus or the compositor. they still
# share our uid, so nix.settings.trusted-users must not contain us: a trusted
# nix user is root.
{
  pkgs,
  lib,
  self,
  ...
}:
let
  # the signing key stays outside; re-sign with `git rebase -S` before pushing
  sbox = pkgs.sbox.override {
    env = {
      GIT_CONFIG_COUNT = "1";
      GIT_CONFIG_KEY_0 = "commit.gpgsign";
      GIT_CONFIG_VALUE_0 = "false";
    };
  };

  # the overlays and the LAN reach services that trust our own addresses
  # (alertmanager, prometheus, the zigbee frontend). rules rather than routes:
  # pasta copies the host's on-link routes, which would be more specific than
  # any unreachable route. set up outside the sandbox's user namespace, so the
  # agent cannot remove them.
  blockPrivateNetworks = pkgs.writeShellScript "block-private-networks" ''
    set -euo pipefail
    PATH=${
      lib.makeBinPath [
        pkgs.iproute2
        pkgs.coreutils
        pkgs.gnugrep
      ]
    }
    # private ranges (42::/16 is retiolum, fc00::/7 hyprspace), plus whatever
    # is on-link or routed by the LAN, e.g. its global ipv6 prefix
    block() {
      family=$1
      shift
      { printf '%s\n' "$@"; ip "$family" route show table main | cut -d' ' -f1; } |
        grep -v -e '^default$' -e '^fe80:' | sort -u |
        while read -r net; do ip "$family" rule add prio 100 to "$net" prohibit; done
    }
    block -4 10.0.0.0/8 100.64.0.0/10 172.16.0.0/12 192.168.0.0/16
    block -6 42::/16 fc00::/7
  '';

  sandboxed =
    {
      name,
      exe,
      state,
    }:
    pkgs.writeShellScriptBin name ''
      # sbox mounts the project read-write, so never start it above a checkout
      top=$(${lib.getExe pkgs.git} rev-parse --show-toplevel 2>/dev/null) || {
        echo "${name}: not in a git checkout; refusing to sandbox $PWD" >&2
        exit 1
      }
      if [ "$top" = "$(realpath "$HOME")" ]; then
        echo "${name}: refusing to sandbox all of $HOME" >&2
        exit 1
      fi
      unset SSH_AUTH_SOCK WAYLAND_DISPLAY DISPLAY
      exec ${lib.getExe sbox} --ephemeral --no-share --history off \
        --chdir "$top" \
        --net-setup ${lib.escapeShellArg blockPrivateNetworks} \
        --persist "$HOME/.cache" \
        ${lib.concatMapStringsSep " " (path: ''--bind-try "$HOME/${path}" "$HOME/${path}"'') state} \
        ${
          # read-only: gh aliases and tea settings would run outside the sandbox
          # the next time we use them
          lib.concatMapStringsSep " " (path: ''--ro-bind-try "$HOME/${path}" "$HOME/${path}"'') [
            ".config/gh"
            ".config/tea"
          ]
        } \
        -- ${lib.getExe pkgs.bash} -c 'cd "$0" && exec "$@"' "$PWD" ${exe} "$@"
    '';

  omp =
    (self.wrapperModules.omp.apply {
      inherit pkgs;
      settings.enabledProviders = [ "claude" ];
    }).wrapper;
in
{
  home-manager.users.me.programs.claude-code = {
    enable = true;
    # the binary is the sandboxed one below; settings.json stays writable
    # for /model, /effort and plugin installs
    package = null;
    context = ./instructions.md;
    skills.astrology = ./skills/astrology;
  };

  environment.systemPackages = [
    (sandboxed {
      name = "omp";
      exe = lib.getExe omp;
      state = [
        ".omp"
        ".claude"
        ".claude.json"
      ];
    })
    (sandboxed {
      name = "claude";
      exe = lib.getExe pkgs.claude-code;
      state = [
        ".claude"
        ".claude.json"
      ];
    })
  ];
}
