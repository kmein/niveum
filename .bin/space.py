import ephem
from datetime import datetime, date, timedelta

now = datetime.now()
limit = now + timedelta(days=365)


def events_until(limit):
    initial_date = ephem.Date(datetime.now())
    events = {}

    now = initial_date
    while ephem.localtime(now) <= limit:
        now = ephem.next_full_moon(now)
        events[now] = "🌕"

    now = initial_date
    while ephem.localtime(now) <= limit:
        now = ephem.next_new_moon(now)
        events[now] = "🌑"

    now = initial_date
    while ephem.localtime(now) <= limit:
        now = ephem.next_vernal_equinox(now)
        events[now] = "spring equinox"

    now = initial_date
    while ephem.localtime(now) <= limit:
        now = ephem.next_autumnal_equinox(now)
        events[now] = "fall equinox"

    now = initial_date
    while ephem.localtime(now) <= limit:
        now = ephem.next_winter_solstice(now)
        events[now] = "winter solstice"

    now = initial_date
    while ephem.localtime(now) <= limit:
        now = ephem.next_summer_solstice(now)
        events[now] = "summer solstice"
    return events


events = events_until(limit)


for date, event in sorted(events.items(), key=lambda x: x[0]):
    if ephem.localtime(date) < limit:
        print(ephem.localtime(date), event)
