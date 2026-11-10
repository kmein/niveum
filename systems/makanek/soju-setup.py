# brings soju's database in line with the networks and channels declared in
# nix; anything added from a client (other channels, networks) stays.
# talks to the admin socket itself, as sojuctl would take the passwords as
# arguments, visible to every process on the host.
import json
import re
import shlex
import socket
import sys
import time

config = json.load(open(sys.argv[1]))
user = config["user"]


def read_secret(path):
    with open(path) as f:
        return f.read().strip()


def bouncerserv(*words):
    with socket.socket(socket.AF_UNIX) as s:
        s.connect(config["adminSocket"])
        command = " ".join(shlex.quote(w) for w in words)
        s.sendall(f"BOUNCERSERV :{command}\r\n".encode())
        replies = []
        for line in s.makefile(encoding="utf-8"):
            line = line.rstrip("\r\n")
            if line.startswith(":"):
                line = line.split(" ", 1)[1]
            head, _, trailing = line.partition(" :")
            verb, *params = head.split(" ")
            if verb == "PRIVMSG":
                replies.append(trailing)
            elif verb == "BOUNCERSERV" and params[:1] == ["OK"]:
                return replies
            else:
                sys.exit(f"soju: {' '.join(words[:4])}: {trailing or line}")
    sys.exit("soju closed the admin socket without an answer")


def as_user(*words):
    return bouncerserv("user", "run", user, *words)


for _ in range(100):
    try:
        bouncerserv("server", "status")
        break
    except (FileNotFoundError, ConnectionRefusedError):
        time.sleep(0.1)

password = read_secret(config["passwordFile"])
users = {line.split(" ")[0] for line in bouncerserv("user", "status")}
if user in users:
    bouncerserv("user", "update", user, "-password", password)
else:
    bouncerserv(
        "user", "create",
        "-username", user,
        "-password", password,
        "-admin",
        "-nick", config["nick"],
        "-realname", config["realname"],
    )

sasl = dict(
    line.split(" ", 1)
    for line in read_secret(config["saslFile"]).splitlines()
    if line.strip()
)
networks = {
    m.group(1)
    for line in as_user("network", "status")
    if (m := re.match(r"(\S+) \(", line))
}
for name, network in config["networks"].items():
    if name not in networks:
        as_user("network", "create", "-name", name, "-addr", network["addr"], "-enabled", "false")
    # before connecting, so the first connection already authenticates
    as_user("sasl", "set-plain", "-network", name, config["nick"], sasl[name])
    as_user(
        "network", "update", name,
        "-addr", network["addr"],
        "-nick", config["nick"],
        "-realname", config["realname"],
        "-enabled", "true",
    )
    joined = {line.split(" ")[0] for line in as_user("channel", "status", "-network", name)}
    for channel in network["channels"]:
        if channel not in joined:
            as_user("channel", "create", f"{channel}/{name}")
