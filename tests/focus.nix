# modules/focus.nix end to end: site windows answer nxdomain for a listed domain
# and its subdomains, for every record type, through nss, resolvectl and the stub,
# while other names at the same address keep working; overlapping windows,
# a config change on switch and a crashed hook all take effect; offline windows
# cut open connections and keep the lan reachable; windows cross midnight, last
# 24 hours, follow local time across dst, and survive a corrupt state and a reboot.
{
  self,
  pkgs,
}:
pkgs.testers.runNixOSTest {
  name = "focus";

  nodes.server =
    { config, lib, ... }:
    let
      v6 = (lib.head config.networking.interfaces.eth1.ipv6.addresses).address;
      webroot = pkgs.runCommand "focus-webroot" { } ''
        mkdir $out
        echo ok > $out/index.html
        head -c 50000000 /dev/zero > $out/big
      '';
    in
    {
      services.nginx = {
        enable = true;
        virtualHosts.default = {
          default = true;
          root = webroot;
        };
      };
      services.dnsmasq = {
        enable = true;
        settings = {
          # also answers subdomains
          address = [
            "/distraction.test/198.51.100.1"
            "/distraction.test/${v6}"
            "/work.test/198.51.100.1"
            "/news.test/198.51.100.1"
            "/sunday.test/198.51.100.1"
          ];
          txt-record = [ "distraction.test,hello" ];
          # aaaa for the v4-only names would otherwise time out on the dead upstream
          local = [ "/test/" ];
        };
      };
      # outside the offline allow list, so it counts as the internet
      networking.interfaces.eth1.ipv4.addresses = [
        {
          address = "198.51.100.1";
          prefixLength = 24;
        }
      ];
      networking.firewall = {
        allowedTCPPorts = [
          80
          53
        ];
        allowedUDPPorts = [ 53 ];
      };
    };

  nodes.client =
    { nodes, ... }:
    {
      imports = [ self.nixosModules.focus ];
      # no niveum overlay here
      niveum.focus.package = self.packages.${pkgs.stdenv.hostPlatform.system}.focus;
      services.resolved.enable = true;
      services.resolved.settings.Resolve.DNSSEC = "false";
      networking.nameservers = [ nodes.server.networking.primaryIPAddress ];
      time.timeZone = "Europe/Berlin";
      services.timesyncd.enable = false;
      networking.interfaces.eth1.ipv4.addresses = [
        {
          address = "198.51.100.2";
          prefixLength = 24;
        }
      ];
      environment.systemPackages = [
        pkgs.curl
        pkgs.nftables
        pkgs.dnsutils
      ];

      niveum.focus = {
        enable = true;
        lists = {
          social = [ "distraction.test" ];
          news = [ "news.test" ];
          sunday = [ "sunday.test" ];
        };
        schedules = [
          {
            days = [
              "Mon"
              "Tue"
              "Wed"
              "Thu"
              "Fri"
              "Sat"
              "Sun"
            ];
            from = "09:00";
            until = "17:00";
            block = [ "social" ];
          }
          {
            days = [ "Mon" ];
            from = "12:00";
            until = "13:00";
            block = [
              "news"
              "social"
            ];
          }
          {
            days = [ "Mon" ];
            from = "23:30";
            until = "06:30";
            block = [ "offline" ];
          }
          {
            days = [ "Sun" ];
            from = "00:00";
            until = "00:00";
            block = [ "sunday" ];
          }
        ];
      };
      # inherits the parent, so social becomes both domains
      specialisation.stricter.configuration.niveum.focus.lists.social = [ "work.test" ];
    };

  testScript =
    { nodes, ... }:
    let
      srv6 = (pkgs.lib.head nodes.server.networking.interfaces.eth1.ipv6.addresses).address;
      lan = nodes.server.networking.primaryIPAddress;
    in
    ''
      # OnClockChange makes the timer apply the schedule
      def at(t):
          client.succeed(f"date -s '{t}'")

      def R(name):
          return f"getent ahosts {name}"

      def dl(tag, args):
          client.succeed(f"sh -c 'curl {args} -s --limit-rate 100k -o /dev/null; echo $? > /tmp/{tag}' >/dev/null 2>&1 &")

      def was_cut(tag, timeout):
          client.wait_until_succeeds(f"test -s /tmp/{tag}", timeout=timeout)
          client.fail(f"test $(cat /tmp/{tag}) -eq 0")

      start_all()
      server.wait_for_unit("nginx.service")
      server.wait_for_unit("dnsmasq.service")
      client.wait_for_unit("focus.timer")
      client.wait_for_unit("focus-hook.socket")
      client.succeed("test -S /run/systemd/resolve.hook/focus")

      # 2026-03-02 is a monday
      with subtest("outside any window everything resolves"):
          at("2026-03-02 08:00:00")
          client.wait_until_succeeds(R("distraction.test"))
          client.wait_until_succeeds(R("sub.distraction.test"))
          client.wait_until_succeeds("resolvectl query -t TXT distraction.test")
          client.wait_until_succeeds("curl -sf http://distraction.test")
          client.wait_until_fails("nft list table inet focus")

      with subtest("a site window blocks the domain on every path"):
          at("2026-03-02 09:05:00")
          client.wait_until_fails(R("distraction.test"))
          client.fail(R("sub.distraction.test"))
          client.fail("resolvectl query -t TXT distraction.test")
          client.succeed("dig @127.0.0.53 distraction.test | grep -q NXDOMAIN")
          client.succeed(R("work.test"))
          client.succeed("curl -sf http://work.test")
          client.fail("nft list table inet focus")

      with subtest("overlapping windows"):
          at("2026-03-02 12:30:00")
          client.wait_until_fails(R("news.test"))
          client.fail(R("distraction.test"))
          at("2026-03-02 13:05:00")
          client.wait_until_succeeds(R("news.test"))
          client.fail(R("distraction.test"))

      with subtest("a config change applies mid-window"):
          at("2026-03-02 14:00:00")
          client.succeed("/run/current-system/specialisation/stricter/bin/switch-to-configuration test")
          client.wait_until_fails(R("work.test"))
          client.succeed("${nodes.client.system.build.toplevel}/bin/switch-to-configuration test")
          client.wait_until_succeeds(R("work.test"))
          client.fail(R("distraction.test"))

      with subtest("a crashed hook comes back"):
          client.succeed("systemctl kill -s KILL focus-hook.service")
          client.wait_until_fails(R("distraction.test"))

      with subtest("the window ends"):
          at("2026-03-02 17:30:00")
          client.wait_until_succeeds(R("distraction.test"))
          client.wait_until_succeeds(R("sub.distraction.test"))
          client.wait_until_succeeds("resolvectl query -t TXT distraction.test")

      with subtest("offline cuts open connections and keeps the lan"):
          at("2026-03-02 23:00:00")
          dl("offl", "http://198.51.100.1/big")
          client.sleep(2)
          at("2026-03-02 23:45:00")
          was_cut("offl", 30)
          client.fail("curl -sf --max-time 5 http://198.51.100.1")
          client.fail("curl -sf --max-time 5 'http://[${srv6}]'")
          client.fail("ping -c1 -W2 198.51.100.1")
          client.succeed("curl -sf http://${lan}")
          client.succeed(R("work.test"))

      with subtest("a window carries over midnight"):
          at("2026-03-03 06:00:00")
          client.sleep(5)
          client.fail("curl -sf --max-time 5 http://198.51.100.1")
          at("2026-03-03 06:31:00")
          client.wait_until_succeeds("curl -sf http://198.51.100.1")
          client.wait_until_fails("nft list table inet focus")
          # tuesday starts no window
          at("2026-03-03 23:45:00")
          client.sleep(5)
          client.succeed("curl -sf --max-time 5 http://198.51.100.1")

      # 2026-03-08 is a sunday
      with subtest("a 24-hour window"):
          at("2026-03-08 12:00:00")
          client.wait_until_fails(R("sunday.test"))
          at("2026-03-08 23:59:00")
          client.sleep(5)
          client.fail(R("sunday.test"))
          at("2026-03-09 00:00:30")
          client.wait_until_succeeds(R("sunday.test"))

      with subtest("local time across dst"):
          at("2026-03-30 08:55:00")
          client.wait_until_succeeds(R("distraction.test"))
          # cest, 07:05 utc
          at("2026-03-30 09:05:00")
          client.wait_until_fails(R("distraction.test"))

      with subtest("a corrupt state is rewritten"):
          client.succeed("echo garbage > /run/focus/state.json && systemctl start focus.service")
          client.succeed("grep -q '\"lists\"' /run/focus/state.json")
          client.fail(R("distraction.test"))

      with subtest("after a reboot"):
          client.shutdown()
          client.start()
          client.wait_for_unit("focus.timer")
          at("2026-03-30 10:00:00")
          client.wait_until_fails(R("distraction.test"))

      client.fail("systemctl is-failed focus.service focus-hook.service focus-hook.socket")
    '';
}
