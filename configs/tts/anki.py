"""Speaks {{tts}} tags and AnkiDroid's <tts> markup with the voices from tts.json, since Anki has no TTS player for Linux."""

import json
import os
import re
import subprocess
import urllib.request
from concurrent.futures import Future
from dataclasses import dataclass
from pathlib import Path

from anki import hooks
from anki.collection import Collection
from anki.sound import AVTag, TTSTag, strip_av_refs
from anki.template import TemplateRenderContext, TemplateRenderOutput
from aqt import mw
from aqt.sound import OnDoneCallback, av_player
from aqt.tts import TTSProcessPlayer, TTSVoice, TTSVoiceMatch

config = json.loads((Path(__file__).parent / "tts.json").read_text())

# <tts service="android" voice="ar_SY">, or a bare <tts>, whose language ankidroid asks for per deck
LEGACY_TAG = re.compile(r"<tts\b([^>]*)>(.*?)</tts>", re.S | re.I)
VOICE_ATTRIBUTE = re.compile(r"""\bvoice=["']?([\w-]+)""", re.I)


def speak_legacy_tags(
    col: Collection, text: str, side: str, tags: list[AVTag], deck_language: str | None
) -> str:
    def replace(match: re.Match) -> str:
        content = match.group(2)
        voice = VOICE_ATTRIBUTE.search(match.group(1))
        language = voice.group(1).replace("-", "_") if voice else deck_language
        # anki.utils' html helpers go through a backend without a collection, which refuses
        speech = col._backend.html_to_text_line(text=content, preserve_media_filenames=False)
        speech = strip_av_refs(speech).strip()
        if not language or not speech:
            return content
        tags.append(TTSTag(field_text=speech, lang=language, voices=[], speed=1.0, other_args=[]))
        return f"{content}[anki:play:{side}:{len(tags) - 1}]"

    return LEGACY_TAG.sub(replace, text)


def on_card_did_render(output: TemplateRenderOutput, context: TemplateRenderContext) -> None:
    col = context.col()
    card = context.card()
    deck = col.decks.name(card.odid or card.did).split("::")[0]
    language = config["decks"].get(deck)
    question = output.question_text
    output.question_text = speak_legacy_tags(col, question, "q", output.question_av_tags, language)
    # {{FrontSide}} repeats the question, whose speech belongs to the question side
    answer = output.answer_text
    if output.question_text != question:
        answer = answer.replace(question, output.question_text)
    output.answer_text = speak_legacy_tags(col, answer, "a", output.answer_av_tags, language)


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
hooks.card_did_render.append(on_card_did_render)
