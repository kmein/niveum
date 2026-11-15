# passage otp pass-name: current code of the entry's otpauth://totp/ line
[[ $# -eq 1 ]] || die "Usage: $PROGRAM otp pass-name"
path="$1"
check_sneaky_paths "$path"
passfile="$PREFIX/$path.age"
[[ -f $passfile ]] || die "Error: $path is not in the password store."
uri="$($AGE -d -i "$IDENTITIES_FILE" "$passfile" | grep -m 1 '^otpauth://totp/')" ||
	die "Error: $path has no otpauth://totp/ line."
secret= digits=6 period=30 algorithm=sha1
IFS='&' read -ra params <<< "${uri#*\?}"
for param in "${params[@]}"; do
	case $param in
		secret=*) secret=${param#secret=} ;;
		digits=*) digits=${param#digits=} ;;
		period=*) period=${param#period=} ;;
		algorithm=*) algorithm=${param#algorithm=}; algorithm=${algorithm,,} ;;
	esac
done
[[ -n $secret ]] || die "Error: the otpauth line of $path has no secret."
@oathtool@ --base32 --totp="$algorithm" --digits="$digits" --time-step-size="${period}s" "$secret"
