import json
import os
import sqlite3
import subprocess
import sys
import time

FIELDS = ["temperature", "humidity", "battery", "linkquality"]

state = os.environ["STATE_DIRECTORY"]
db = sqlite3.connect(os.path.join(state, "climate.db"))
# raw payloads, so a new sensor or field needs no schema change
db.execute("create table if not exists readings (ts, device, payload)")
db.execute("create index if not exists by_device on readings (device, ts)")


def export():
    columns = ", ".join(f"json_extract(payload, '$.{f}')" for f in FIELDS)
    out = {}
    query = f"select device, ts, {columns} from readings order by ts"
    for device, *row in db.execute(query):
        series = out.setdefault(device, {k: [] for k in ["ts", *FIELDS]})
        for key, value in zip(series, row):
            series[key].append(value)
    target = os.path.join(state, "data.json")
    with open(target + ".tmp", "w") as f:
        json.dump(out, f, separators=(",", ":"))
    # nginx must never see a half-written file
    os.replace(target + ".tmp", target)


# argv is the mosquitto_sub command line; without it, read stdin for testing
if sys.argv[1:]:
    source = subprocess.Popen(sys.argv[1:], stdout=subprocess.PIPE, text=True)
    lines = source.stdout
else:
    lines = sys.stdin

export()
for line in lines:
    try:
        message = json.loads(line)
        payload = json.loads(message["payload"])
    except (ValueError, KeyError):
        continue
    # a retained message would be logged again on every restart
    if message.get("retain") or not isinstance(payload, dict):
        continue
    if "temperature" not in payload:
        continue
    device = message["topic"].split("/", 1)[1]
    with db:
        db.execute(
            "insert into readings values (?, ?, ?)",
            (int(time.time()), device, json.dumps(payload)),
        )
    export()

# the broker went away; let systemd restart us
sys.exit(1)
