# passage lock: drop the identity `passage unlock` left in memory
[[ $# -eq 0 ]] || die "Usage: $PROGRAM lock"
rm -f "$UNLOCKED_IDENTITIES_FILE"
@systemctl@ --user stop passage-lock.timer 2>/dev/null
