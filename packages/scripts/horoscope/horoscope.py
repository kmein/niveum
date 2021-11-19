from datetime import datetime
import click
from flatlib.datetime import Datetime
from flatlib.geopos import GeoPos
from flatlib.chart import Chart
import flatlib.const

sign_symbols = {
    flatlib.const.ARIES: "♈",
    flatlib.const.TAURUS: "♉",
    flatlib.const.GEMINI: "♊",
    flatlib.const.CANCER: "♋",
    flatlib.const.LEO: "♌",
    flatlib.const.VIRGO: "♍",
    flatlib.const.LIBRA: "♎",
    flatlib.const.SCORPIO: "♏",
    flatlib.const.SAGITTARIUS: "♐",
    flatlib.const.CAPRICORN: "♑",
    flatlib.const.AQUARIUS: "♒",
    flatlib.const.PISCES: "♓",
}

planet_symbols = {
    flatlib.const.SUN: "☉",
    flatlib.const.MOON: "☽",
    flatlib.const.MERCURY: "☿",
    flatlib.const.VENUS: "♀",
    flatlib.const.MARS: "♂",
    flatlib.const.JUPITER: "♃",
    flatlib.const.SATURN: "♄",
}


def convert_into_stupid_flatlib_format(dt):
    return Datetime(dt.strftime("%Y/%m/%d"), dt.strftime("%H:%M"))


@click.command()
@click.option("--latitude", type=click.FLOAT, required=True)
@click.option("--longitude", type=click.FLOAT, required=True)
@click.option("--date", type=click.DateTime(), default=datetime.now())
def main(latitude: float, longitude: float, date: datetime):
    flatlib_datetime = convert_into_stupid_flatlib_format(date)
    position = GeoPos(latitude, longitude)
    chart = Chart(flatlib_datetime, position)
    for planet in planet_symbols.keys():
        planet_position = chart.getObject(planet)
        print(
            planet_symbols[planet],
            sign_symbols[planet_position.sign],
            "℞" if planet_position.movement() == flatlib.const.RETROGRADE else "",
            end="",
        )
    print()
