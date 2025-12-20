from flask import Flask, send_file
from pathlib import Path
from random import choice, randint
from io import BytesIO
from PIL import Image
import os

app = Flask(__name__)
TAROT_DIR = Path(os.environ["TAROT_FILES"])


@app.route("/")
def tarot():
    card_path = choice(list(TAROT_DIR.glob("*")))

    with Image.open(card_path) as img:
        if randint(0, 1):
            img = img.rotate(180)
        buf = BytesIO()
        img.save(buf, format="JPEG")
        buf.seek(0)
        return send_file(buf, mimetype="image/jpeg", as_attachment=False)


if __name__ == "__main__":
    app.run(port=int(os.environ["TAROT_PORT"]))
