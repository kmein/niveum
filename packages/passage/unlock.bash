# passage unlock: keep a passphrase-free copy of the ssh identity in $XDG_RUNTIME_DIR,
# since age asks only on a terminal and Mod+P has none; gone at logout.
# ssh-keygen asks on the terminal, or through SSH_ASKPASS without one
[[ $# -eq 0 ]] || die "Usage: $PROGRAM unlock"
[[ -n ${XDG_RUNTIME_DIR-} ]] || die "Error: XDG_RUNTIME_DIR is not set."
umask 077
mkdir -p "${UNLOCKED_IDENTITIES_FILE%/*}"
tmp="$UNLOCKED_IDENTITIES_FILE.tmp"
cp -L "${PASSAGE_IDENTITIES_FILE:-$HOME/.passage/identities}" "$tmp" &&
	@sshKeygen@ -q -p -N '' -f "$tmp" >/dev/null &&
	mv "$tmp" "$UNLOCKED_IDENTITIES_FILE" || {
	rm -f "$tmp"
	die "Error: could not unlock the identity."
}
