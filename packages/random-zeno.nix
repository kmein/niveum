{ writers, lib, curl, pup, gnused, coreutils, pandoc, gawk, jq }:
writers.writeDashBin "random-zeno" ''
  set -efu

  export PATH=${lib.makeBinPath [ curl pup gnused coreutils pandoc gawk jq ]}

  root="http://www.zeno.org"
  character_limit=350

  fetch() {
    curl -sSL "$root$1"
  }

  blacklist='\/Biographie$'

  next_links() {
    html="$1"
    echo "$html" | pup '.zenoTXLinkInt attr{href}' | sed "/$blacklist/d"
    echo "$html" | pup '.zenoTRNavBottom a attr{href}' | sed "/$blacklist/d"
  }

  eval_html() {
    url="$1"
    html="$(fetch "$url")"
    links="$(next_links "$html" | sed '/^\s*$/d')"
    links_count="$(echo "$links" | wc -l)"
    if [ -z "$links" ]
    then
      random_paragraph="$(echo "$html" | pup '.zenoCOMain p' | tr '\n' ' ' | sed 's/<\/p>/&\n/g')"
      [ "$random_paragraph" = null ] && exit
      prettify "$url" "$random_paragraph"
    else
      next_link="$(echo "$links" | shuf -n1)"
      eval_html "$next_link"
    fi
  }

  prettify() {
    url="$1"
    html="$2"

    plain_text="$(echo "$html" | tr '\n' ' ' \
      | sed \
        -e 's/<h[1-6][^>]*>[^<]*<\/h[1-6]>//g' \
        -e 's/\[[0-9]*\]\s*//g' \
      | pandoc -f html -t plain \
      | tr '\n' ' '
    )"

    truncated=$(echo "$plain_text" | awk -v limit="$character_limit" '
      {
        for (i = 1; i <= NF; i++) {
          word_length = length($i) + 1; # +1 for the space
          if (total_length + word_length > limit) {
            print "…";
            break;
          } else {
            total_length += word_length;
            printf "%s ", $i; # Print the word followed by a space
          }
        }
        print "";
      }
    ' | sed 's/\([:,.!?;]\)[^:,.!?;]*…/\1 …/')

    url="$(echo "$url" | jq -sRr @uri)"
    printf "%s\n\n%s\n" "$truncated" "$root$url"
  }

  eval_html "$1"
''
