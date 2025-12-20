from flask import Flask
import random
from typing import List
from enum import Enum
import os

app = Flask(__name__)

KING_WEN_SEQUENCE: List[int] = [
    0b111111,
    0b000000,
    0b100010,
    0b010001,
    0b111010,
    0b010111,
    0b010000,
    0b000010,
    0b111011,
    0b110111,
    0b111000,
    0b000111,
    0b101111,
    0b111101,
    0b001000,
    0b000100,
    0b100110,
    0b011001,
    0b110000,
    0b000011,
    0b100101,
    0b101001,
    0b000001,
    0b100000,
    0b100111,
    0b111001,
    0b100001,
    0b011110,
    0b010010,
    0b101101,
    0b001110,
    0b011100,
    0b001111,
    0b111100,
    0b000101,
    0b101000,
    0b101011,
    0b110101,
    0b001010,
    0b010100,
    0b110001,
    0b100011,
    0b111110,
    0b011111,
    0b000110,
    0b011000,
    0b010110,
    0b011010,
    0b101110,
    0b011101,
    0b100100,
    0b001001,
    0b001011,
    0b110100,
    0b101100,
    0b001101,
    0b011011,
    0b110110,
    0b010011,
    0b110010,
    0b110011,
    0b001100,
    0b101010,
    0b010101,
]


class Line(Enum):
    """
    Represents a line in an I Ching hexagram.
    Each line can be one of the following:
    - 6: Old Yin (changing yin)
    - 7: Young Yang (static yang)
    - 8: Young Yin (static yin)
    - 9: Old Yang (changing yang)
    """

    OLD_YIN = 6  # changing yin
    YOUNG_YANG = 7  # static yang
    YOUNG_YIN = 8  # static yin
    OLD_YANG = 9  # changing yang

    def is_changing(self) -> bool:
        """Returns True if the line is changing (old)."""
        return self in [Line.OLD_YIN, Line.OLD_YANG]

    def symbol(self) -> str:
        """Returns the textual representation of the line."""
        symbols = {
            Line.YOUNG_YANG: "───────",
            Line.YOUNG_YIN: "─── ───",
            Line.OLD_YANG: "───o───",
            Line.OLD_YIN: "───x───",
        }
        return symbols[self]

    def binary_value(self) -> int:
        """Returns the binary value of the line (1 for yang, 0 for yin)."""
        return 1 if self in [Line.YOUNG_YANG, Line.OLD_YANG] else 0


class Hexagram:
    """
    Represents an I Ching hexagram.
    Each hexagram consists of six lines.
    """

    def __init__(self, lines: List[Line] | None = None) -> None:
        """
        Initializes a Hexagram.
        If lines are not provided, generates a random hexagram.
        :param lines: List of six integers = the lines of the hexagram.
        """
        if lines is None:
            self.lines = random.choices(
                [Line.OLD_YIN, Line.YOUNG_YANG, Line.YOUNG_YIN, Line.OLD_YANG],
                weights=[1, 5, 7, 3],
                k=6,
            )
        else:
            self.lines = lines

    def print(self) -> str:
        """Prints the hexagram details."""
        return "\n".join(
            [
                "HEXAGRAM {} {}".format(
                    self.king_wen_number(), self.unicode_representation()
                ),
                "Binary: {:06b}".format(self.binary_representation()),
                "Lines (bottom → top): {}".format(
                    " ".join(str(line.value) for line in self.lines)
                ),
                self.render_text(),
            ]
        )

    def render_text(self) -> str:
        """Renders the hexagram in a textual box format."""
        lines = []
        lines.append("┌─────────┐")
        for line in reversed(self.lines):
            body = line.symbol()
            lines.append(f"│ {body} │")
        lines.append("└─────────┘")
        return "\n".join(lines)

    def binary_representation(self) -> int:
        """Returns the binary representation of the hexagram."""
        return sum(
            val << i
            for i, val in enumerate(
                line.binary_value() for line in reversed(self.lines)
            )
        )

    def king_wen_number(self) -> int:
        """Returns the King Wen number of the hexagram."""
        return KING_WEN_SEQUENCE.index(self.binary_representation()) + 1

    def unicode_representation(self) -> str:
        """Returns the Unicode character representing the hexagram."""
        return chr(0x4DC0 + self.king_wen_number() - 1)

    def changing_hexagram(self):
        """Returns the changing hexagram
        if there are changing lines, else None."""
        if any(line.is_changing() for line in self.lines):
            new_lines = [
                (
                    Line.YOUNG_YIN
                    if line == Line.OLD_YANG
                    else Line.YOUNG_YANG if line == Line.OLD_YIN else line
                )
                for line in self.lines
            ]
            return Hexagram(new_lines)
        return None


@app.route("/")
def main():
    result = ""
    hexagram = Hexagram()
    result += hexagram.print()
    if changing_hex := hexagram.changing_hexagram():
        result += "\n"
        result += changing_hex.print()
    return f"<pre>{result}</pre>"


if __name__ == "__main__":
    app.run(port=int(os.environ["ICHING_PORT"]))
