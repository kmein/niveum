# passage unlock: keep a passphrase-free copy of the ssh identity in $XDG_RUNTIME_DIR,
# since age asks only on a terminal and Mod+P has none. passage lock, the screen
# locker, logout or @lockAfter@ without use drop it.
# ssh-keygen asks on the terminal, or through SSH_ASKPASS without one
[[ $# -eq 0 ]] || die "Usage: $PROGRAM unlock"
[[ -n ${XDG_RUNTIME_DIR-} ]] || die "Error: XDG_RUNTIME_DIR is not set."
umask 077
mkdir -p "${UNLOCKED_IDENTITIES_FILE%/*}"
tmp="$UNLOCKED_IDENTITIES_FILE.tmp"
@systemctl@ --user stop passage-lock.timer 2>/dev/null
cp -L "${PASSAGE_IDENTITIES_FILE:-$HOME/.passage/identities}" "$tmp" &&
	@sshKeygen@ -q -p -N '' -f "$tmp" >/dev/null &&
	@systemdRun@ --user --quiet --collect --unit=passage-lock --on-active=@lockAfter@ \
		--timer-property=AccuracySec=1s \
		@rm@ -f "$UNLOCKED_IDENTITIES_FILE" &&
	mv "$tmp" "$UNLOCKED_IDENTITIES_FILE" || {
	rm -f "$tmp"
	die "Error: could not unlock the identity."
}
