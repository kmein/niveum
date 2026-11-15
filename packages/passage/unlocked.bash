# an identity `passage unlock` left in memory wins over the passphrase-protected one;
# each use re-arms the timer that drops it, as gpg-agent's cache ttl did
UNLOCKED_IDENTITIES_FILE="${XDG_RUNTIME_DIR-}/passage/identities"
if [[ -z ${PASSAGE_IDENTITIES_FILE-} && -f $UNLOCKED_IDENTITIES_FILE ]]; then
	IDENTITIES_FILE="$UNLOCKED_IDENTITIES_FILE"
	@systemctl@ --user try-restart passage-lock.timer 2>/dev/null
fi
