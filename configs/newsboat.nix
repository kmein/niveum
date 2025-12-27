{
  pkgs,
  config,
  ...
}:
{
  environment.systemPackages = [
    (pkgs.writers.writeDashBin "miniflux-watch-later" ''
      miniflux_api_token=$(cat ${config.age.secrets.miniflux-api-token.path})
      random_feed_item=$(
        ${pkgs.curl}/bin/curl -u "$miniflux_api_token" --basic -s 'https://feed.kmein.de/v1/entries?starred=true&limit=0' \
        | ${pkgs.jq}/bin/jq -r '.entries[].id' \
        | ${pkgs.coreutils}/bin/shuf -n1
      )
      ${pkgs.xdg-utils}/bin/xdg-open "https://feed.kmein.de/starred/entry/$random_feed_item"
    '')
  ];
}
