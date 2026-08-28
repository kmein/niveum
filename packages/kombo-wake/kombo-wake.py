"""Wake the Teufel Kombo 42 when the 'teufel' Chromecast starts playing.

The KB 42 receiver has no wake mechanism of its own -- no CEC, no signal
sensing, no trigger input, and it returns to standby after a mains cut. The
only way in is the front power button, which a SwitchBot Bot presses.

That button is a *toggle*, and the Bot in press mode gives no feedback, so we
have to model whether the amp is already on. The model leans on the receiver's
own auto-standby: it drops to standby by itself once the audio signal has been
absent for a while. So the amp is on iff we pressed it and the cast has been
feeding it since. STANDBY_AFTER must match the receiver's real timeout --
measure it once (see --status) rather than trusting the default.

A power-monitoring plug on the receiver would replace this guesswork with
ground truth; see amp_believed_on() for where it would slot in.
"""

from __future__ import annotations

import argparse
import asyncio
import json
import logging
import os
import sys
import time
from pathlib import Path
from uuid import UUID

import pychromecast
from bleak_retry_connector import get_device
from pychromecast.controllers.media import MediaStatusListener
from pychromecast.controllers.receiver import CastStatusListener
from switchbot import Switchbot

# Connect straight to the cast device by address. mDNS discovery is flaky from
# zaatar (pychromecast's browser turns up nothing there even though avahi
# resolves the name fine), and the address is static anyway.
CAST_NAME = "teufel"
CAST_HOST = os.environ.get("KOMBO_CAST_HOST", "192.168.0.194")
CAST_PORT = int(os.environ.get("KOMBO_CAST_PORT", "8009"))
CAST_UUID = UUID(os.environ.get("KOMBO_CAST_UUID", "29c3c6e3-928c-b14f-43ec-3009f91f421b"))

BOT_MAC = os.environ.get("KOMBO_BOT_MAC", "C9:36:35:30:41:08")

# How long the KB 42 tolerates silence before dropping to standby on its own.
# Measure yours: start playing, stop, and time how long until the front LED
# goes red. Set this slightly BELOW that, so we err towards believing the amp
# is off (a spurious press turns it on -- harmless; a missed press leaves you
# with no sound, and a wrong "it's already on" turns it OFF mid-playback).
STANDBY_AFTER = 20 * 60

# Ignore repeat triggers this soon after a press: the amp takes a moment to
# come up and the cast may emit several status changes as it settles.
PRESS_COOLDOWN = 45

STATE_FILE = Path(os.environ.get("KOMBO_STATE", "/var/lib/kombo-wake/state.json"))

log = logging.getLogger("kombo-wake")


# --------------------------------------------------------------------------
# believed amp state


def load_state() -> dict:
    try:
        return json.loads(STATE_FILE.read_text())
    except (OSError, ValueError):
        return {"pressed_at": 0.0, "last_signal": 0.0}


def save_state(state: dict) -> None:
    try:
        STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
        STATE_FILE.write_text(json.dumps(state))
    except OSError as exc:
        log.warning("could not persist state: %s", exc)


def amp_believed_on(state: dict) -> bool:
    """True if we think the receiver is currently out of standby.

    Replace this body with a read of a power-monitoring plug (standby is
    ~0.5 W, running is tens of watts) to make the whole thing exact.
    """
    if not state.get("pressed_at"):
        return False
    quiet_for = time.time() - max(state["last_signal"], state["pressed_at"])
    return quiet_for < STANDBY_AFTER


# --------------------------------------------------------------------------
# the Bot


async def press_bot() -> bool:
    device = await get_device(BOT_MAC)
    if device is None:
        log.error("Bot %s not advertising -- out of range or battery dead", BOT_MAC)
        return False
    bot = Switchbot(device=device)
    await bot.press()
    log.info("pressed the Bot")
    return True


# --------------------------------------------------------------------------
# cast watching
#
# pychromecast calls listeners from its own socket thread, so events are handed
# to the asyncio loop rather than acted on in place.


class Trigger(CastStatusListener, MediaStatusListener):
    def __init__(self, loop: asyncio.AbstractEventLoop, queue: asyncio.Queue) -> None:
        self._loop = loop
        self._queue = queue

    def _emit(self, active: bool, why: str) -> None:
        self._loop.call_soon_threadsafe(self._queue.put_nowait, (active, why))

    # An app launching is the earliest sign of a cast, and gives the amp a head
    # start on waking before audio actually arrives.
    def new_cast_status(self, status) -> None:
        idle = status.app_id in (None, pychromecast.IDLE_APP_ID)
        self._emit(not idle, f"app={status.display_name or status.app_id}")

    def new_media_status(self, status) -> None:
        self._emit(status.player_state == "PLAYING", f"player={status.player_state}")

    def load_media_failed(self, queue_item_id: int, error_code: int) -> None:
        log.warning("cast load failed (item %s, error %s)", queue_item_id, error_code)


async def watch() -> None:
    loop = asyncio.get_running_loop()
    queue: asyncio.Queue = asyncio.Queue()

    cast = pychromecast.get_chromecast_from_host(
        (CAST_HOST, CAST_PORT, CAST_UUID, None, CAST_NAME)
    )
    cast.wait(timeout=30)
    log.info("watching cast %r at %s:%s", cast.name, CAST_HOST, CAST_PORT)

    trigger = Trigger(loop, queue)
    cast.register_status_listener(trigger)
    cast.media_controller.register_status_listener(trigger)

    state = load_state()

    # Refresh the silence clock while a cast is active, so the standby model
    # does not expire mid-playback.
    async def ticker() -> None:
        while True:
            await asyncio.sleep(30)
            status = cast.media_controller.status
            if status and status.player_state == "PLAYING":
                queue.put_nowait((True, "tick"))

    tick_task = asyncio.create_task(ticker())

    try:
        while True:
            active, why = await queue.get()
            now = time.time()

            if not active:
                # Signal stopped; the receiver starts counting down to standby.
                if state["last_signal"] < now:
                    state["last_signal"] = now
                    save_state(state)
                continue

            # Decide before refreshing the clock. amp_believed_on() measures
            # silence since the last signal, so updating it first would make
            # every event look like the amp had just been heard from, and
            # nothing would ever be pressed again after the first time.
            believed_on = amp_believed_on(state)
            state["last_signal"] = now

            if believed_on:
                if why != "tick":
                    log.debug("cast active (%s) -- amp believed already on", why)
                save_state(state)
                continue

            if now - state["pressed_at"] < PRESS_COOLDOWN:
                log.debug("within cooldown, not pressing again")
                continue

            log.info("cast active (%s) and amp believed off -- pressing", why)
            if await press_bot():
                state["pressed_at"] = now
            save_state(state)
    finally:
        tick_task.cancel()
        cast.disconnect()


# --------------------------------------------------------------------------


def main() -> None:
    ap = argparse.ArgumentParser(description=(__doc__ or "").splitlines()[0])
    ap.add_argument("--press", action="store_true", help="press the Bot once and exit")
    ap.add_argument("--status", action="store_true", help="show believed amp state")
    ap.add_argument("--set-on", action="store_true", help="resync: amp is actually ON")
    ap.add_argument("--set-off", action="store_true", help="resync: amp is actually OFF")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)-7s %(message)s",
    )

    if args.status:
        state = load_state()
        on = amp_believed_on(state)
        print(f"amp believed: {'ON' if on else 'OFF'}")
        for key in ("pressed_at", "last_signal"):
            ts = state.get(key) or 0
            when = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(ts)) if ts else "never"
            print(f"  {key:12} {when}")
        print(f"  standby model: {STANDBY_AFTER}s of silence")
        return

    if args.set_on or args.set_off:
        state = load_state()
        now = time.time()
        state["pressed_at"] = now if args.set_on else 0.0
        state["last_signal"] = now if args.set_on else 0.0
        save_state(state)
        print(f"state set to {'ON' if args.set_on else 'OFF'}")
        return

    if args.press:
        ok = asyncio.run(press_bot())
        sys.exit(0 if ok else 1)

    asyncio.run(watch())


if __name__ == "__main__":
    main()
