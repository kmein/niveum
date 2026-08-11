# https://github.com/Fluepke/nix-files/blob/2be70b76a198afaa7763132fed645a3c19d5af6e/configuration/common/blackbox-exporter.yml
# https://github.com/xHain-hackspace/xhain-nixfiles/blob/0d6e3b87a07317c2d54cccabf4f90da589319e2c/common/prometheus/blackbox-exporter.yml
{
  modules.http_2xx = {
    http = {
      fail_if_not_ssl = false;
      ip_protocol_fallback = false;
      method = "GET";
      no_follow_redirects = false;
      preferred_ip_protocol = "ip4";
      valid_http_versions = [
        "HTTP/1.1"
        "HTTP/2.0"
      ];
      tls_config.insecure_skip_verify = true;
    };
    prober = "http";
    timeout = "15s";
  };

  modules.icmp = {
    prober = "icmp";
    timeout = "5s";
    icmp = {
      preferred_ip_protocol = "ip4"; # retiolum is 10.243.0.0/16
      ip_protocol_fallback = false;
    };
  };

  modules.tcp_connect = {
    prober = "tcp";
    timeout = "5s";
    tcp = {
      preferred_ip_protocol = "ip4";
      ip_protocol_fallback = false;
      # a banner check, so that a listening socket alone does not count as success
      query_response = [ { expect = "^SSH-2\\.0-"; } ];
    };
  };
}
