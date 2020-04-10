#!/usr/bin/env bash
# fzfmenu - fzf as dmenu replacement
# https://github.com/junegunn/fzf/wiki/Examples#fzf-as-dmenu-replacement

input=$(mktemp -u --suffix .fzfmenu.input)
output=$(mktemp -u --suffix .fzfmenu.output)
mkfifo "$input"
mkfifo "$output"
chmod 600 "$input" "$output"

# it's better to use st here (starts a lot faster than pretty much everything else)
st -c fzfmenu -n fzfmenu -g 85x15 -e sh -c "cat $input | fzf $* | tee $output" & disown

# handle ctrl+c outside child terminal window
trap 'kill $! 2>/dev/null; rm -f $input $output' EXIT

cat > "$input"
cat "$output"
