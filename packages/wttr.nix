{ writeDashBin, curl, jq }:
writeDashBin "wttr" ''
  ${curl}/bin/curl -s -H "Accept-Language: ''${LANG%_*}" --compressed "wttr.in/''${1-"@$(${curl}/bin/curl -s ipinfo.io | ${jq}/bin/jq -r .ip)"}?0"
''
