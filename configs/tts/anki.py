"""Speaks {{tts}} tags with the voices from voices.json, since Anki has no TTS player for Linux."""

import json
import os
import subprocess
import urllib.request
from concurrent.futures import Future
from dataclasses import dataclass
from pathlib import Path

from anki.sound import AVTag, TTSTag
from aqt import mw
from aqt.sound import OnDoneCallback, av_player
from aqt.tts import TTSProcessPlayer, TTSVoice, TTSVoiceMatch

config = json.loads((Path(__file__).parent / "voices.json").read_text())


@dataclass
class Voice(TTSVoice):
    engine: str


class Player(TTSProcessPlayer):
    _path: str | None = None

    def get_available_voices(self) -> list[TTSVoice]:
        return [
            Voice(name=name, lang=name.split("-")[0], engine="piper")
            for name in config["piper"]["voices"]
        ] + [
            Voice(name=f"espeak-ng_{locale}", lang=locale, engine="espeak")
            for locale in config["espeak"]["locales"]
        ]

    def voice_for_tag(self, tag: TTSTag) -> TTSVoiceMatch | None:
        # decks name the country (ar_SA, ta_LK), the voices may come from another one
        if match := super().voice_for_tag(tag):
            return match
        language = tag.lang.split("_")[0]
        for voice in self.voices():
            if voice.lang.split("_")[0] == language:
                return TTSVoiceMatch(voice=voice, rank=-100)
        return None

    # runs on a background thread
    def _play(self, tag: AVTag) -> None:
        assert isinstance(tag, TTSTag)
        self._path = None
        text = tag.field_text.strip()
        if not text:
            return
        match = self.voice_for_tag(tag)
        assert match
        voice = match.voice
        assert isinstance(voice, Voice)
        path = f"{self.temp_file_for_tag_and_voice(tag, voice)}-{tag.speed}.wav"
        if not os.path.exists(path):
            # the cache must not keep a half-written file
            partial = f"{path}.part"
            if voice.engine == "piper":
                request = urllib.request.Request(
                    config["piper"]["url"],
                    data=json.dumps(
                        {"text": text, "voice": voice.name, "length_scale": 1 / tag.speed}
                    ).encode(),
                    headers={"Content-Type": "application/json"},
                )
                with urllib.request.urlopen(request) as response:
                    Path(partial).write_bytes(response.read())
            else:
                # a file, not --stdout, whose wav header would claim an endless stream
                subprocess.run(
                    [
                        config["espeak"]["exe"],
                        "-v", voice.lang.split("_")[0],
                        "-s", str(round(175 * tag.speed)),
                        "-w", partial,
                        "--stdin",
                    ],
                    input=text.encode(),
                    check=True,
                )
            os.replace(partial, path)
        self._path = path

    # runs on the main thread; the wav goes to anki's own audio player, so it can be stopped and replayed
    def _on_done(self, ret: Future, cb: OnDoneCallback) -> None:
        ret.result()
        if self._path is None:
            cb()
            return
        av_player.current_player = None
        av_player.insert_file(self._path)


av_player.players.append(Player(mw.taskman))
