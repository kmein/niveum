"""Print the call log (Anrufliste) of a Vodafone Station (Arris TG3442DE)."""

import argparse
import datetime
import getpass
import hashlib
import http.cookiejar
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

from cryptography.exceptions import InvalidTag
from cryptography.hazmat.primitives.ciphers.aead import AESCCM

PASSWORD_ENV = "VODAFONE_STATION_PASSWORD"


class Error(Exception):
    pass


OCCUPIED = "another session is active (web UI or a previous run); rerun with --force to end it"


def page_var(page, name):
    m = re.search(rf"\bvar {name}\s*=\s*'([^']*)'", page)
    if not m:
        raise Error(f"no '{name}' on the router page; not an Arris TG3442DE?")
    return m.group(1)


class Station:
    def __init__(self, host, debug=False):
        self.base = f"http://{host}/"
        self.debug = debug
        self.csrf_nonce = None
        self.logged_in = False
        self.opener = urllib.request.build_opener(
            urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar())
        )

    def log(self, msg):
        if self.debug:
            print(f"debug: {msg}", file=sys.stderr)

    def request(self, method, path, body=None):
        headers = {"X-Requested-With": "XMLHttpRequest", "Referer": self.base}
        data = None
        if body is not None:
            data = json.dumps(body).encode()
            headers["Content-Type"] = "application/json"
        if self.csrf_nonce:
            headers["csrfNonce"] = self.csrf_nonce
        req = urllib.request.Request(self.base + path, data, headers, method=method)
        try:
            with self.opener.open(req, timeout=15) as resp:
                text = resp.read().decode("utf-8", "replace")
                self.log(f"{method} /{path} -> {resp.status}, {len(text)} bytes")
                return text
        except urllib.error.HTTPError as e:
            self.log(f"{method} /{path} -> {e.code}")
            raise Error(f"{method} /{path} failed: HTTP {e.code}") from None
        except (urllib.error.URLError, TimeoutError) as e:
            raise Error(f"cannot reach {self.base}: {getattr(e, 'reason', e)}") from None

    def login(self, password, force):
        page = self.request("GET", "")
        session_id = page_var(page, "currentSessionId")
        iv, salt = page_var(page, "myIv"), page_var(page, "mySalt")
        m = re.search(r"_ga\.lockoutTimeLeft\s*=\s*'(\d+)'", page)
        if m and int(m.group(1)) > 0:
            raise Error(f"login locked after failed attempts; retry in {m.group(1)} s")
        # checked before sending the password, so a refusal costs no login attempt
        if page_var(page, "nonActiveSession") != "1" and not force:
            raise Error(OCCUPIED)

        key = hashlib.pbkdf2_hmac("sha256", password.encode(), bytes.fromhex(salt), 1000, 16)
        ccm = AESCCM(key, tag_length=16)
        # same string concatenation as the web UI, so odd passwords encode identically
        plain = f'{{"Password": "{password}", "Nonce": "{session_id}"}}'
        encrypted = ccm.encrypt(bytes.fromhex(iv), plain.encode(), b"loginPassword")
        resp = json.loads(
            self.request(
                "POST",
                "php/ajaxSet_Password.php",
                {"EncryptData": encrypted.hex(), "Name": "admin", "AuthData": "loginPassword"},
            )
        )
        status = resp.get("p_status", "")
        self.log(f"login status {status!r}")
        # mirrors the web UI: "<user>Match" or "Default" (factory password)
        if not (status.find("Match") > 0 or status == "Default"):
            # a wrong password comes back as "Lockout" with a wait time
            wait = resp.get("p_waitTime")
            raise Error(
                "wrong password" + (f"; the router blocks logins for {wait} s" if wait else "")
            )
        self.logged_in = True
        try:
            nonce = ccm.decrypt(bytes.fromhex(iv), bytes.fromhex(resp["encryptData"]), b"nonce")
        except (KeyError, ValueError, InvalidTag):
            raise Error("cannot decrypt the session nonce from the login response") from None
        self.csrf_nonce = nonce.decode()

        # mirrors the web UI's first page load after login, which either accepts
        # the session or has to end the other active one explicitly
        page = self.request("GET", "")
        if page_var(page, "webLoginStatus"):
            if not page_var(page, "sessionValid"):
                raise Error("the router rejected the new session")
        elif page_var(page, "nonActiveSession"):
            raise Error("the router rejected the new session")
        elif not force:
            raise Error(OCCUPIED)
        else:
            resp = json.loads(self.request("POST", "php/ajaxSet_Session.php"))
            if resp.get("LoginStatus") == "no":
                raise Error("could not end the other active session")

    def call_log(self):
        # the page's own loader sends its JSON request object as the raw query string
        query = urllib.parse.quote('{"PhoneLogRecord":{}}', safe=":")
        text = self.request("GET", f"php/phone_call_log_data.php?{query}")
        try:
            records = json.loads(text)["PhoneLogRecord"]
        except (ValueError, KeyError, TypeError):
            raise Error("unexpected call log response (session lost?)") from None
        return [normalize(r) for r in records]

    def logout(self):
        self.request("POST", "php/logout.php")
        self.csrf_nonce = None
        self.logged_in = False


def normalize(record):
    # recent entries carry a translation key instead of a date
    date = record.get("Date", "")
    today = datetime.date.today()
    if date == "PAGE_CALL_LOG_TABLE_TODAY":
        date = today.isoformat()
    elif date == "PAGE_CALL_LOG_TABLE_YESTERDAY":
        date = (today - datetime.timedelta(days=1)).isoformat()
    return {
        "date": date,
        "time": record.get("Time", ""),
        "type": record.get("CallType", "").lower(),
        "number": record.get("ExternalNumber", ""),
        "duration": record.get("Duration") or None,
    }


def print_table(calls):
    header = ["DATE", "TIME", "TYPE", "NUMBER", "DURATION"]
    rows = [[c["date"], c["time"], c["type"], c["number"], c["duration"] or ""] for c in calls]
    widths = [max(len(r[i]) for r in [header, *rows]) for i in range(len(header))]
    for row in [header, *rows]:
        print("  ".join(cell.ljust(w) for cell, w in zip(row, widths)).rstrip())


def read_password():
    password = os.environ.get(PASSWORD_ENV)
    if password:
        return password
    if not sys.stdin.isatty():
        raise Error(f"set {PASSWORD_ENV} or run on a terminal to be prompted")
    return getpass.getpass("router password: ")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host", default="192.168.0.1", help="router address (default: %(default)s)")
    parser.add_argument("--json", action="store_true", help="print JSON instead of a table")
    parser.add_argument(
        "--force", action="store_true", help="end another active web UI session to log in"
    )
    parser.add_argument("--debug", action="store_true", help="log requests to stderr")
    args = parser.parse_args()

    try:
        password = read_password()
        station = Station(args.host, args.debug)
        try:
            station.login(password, args.force)
            calls = station.call_log()
        finally:
            if station.logged_in:
                station.logout()
    except Error as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 130

    if args.json:
        json.dump(calls, sys.stdout, indent=2)
        print()
    elif calls:
        print_table(calls)
    if not calls:
        print("call log is empty", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
