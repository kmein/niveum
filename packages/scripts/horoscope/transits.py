from flatlib import aspects, const
from flatlib.chart import Chart
from flatlib.datetime import Datetime
import pytz
from flatlib.geopos import GeoPos
import timezonefinder
import operator
import click
import itertools
from datetime import datetime, timedelta

tf = timezonefinder.TimezoneFinder()

planets = [
    const.SUN,
    const.MOON,
    const.MERCURY,
    const.VENUS,
    const.MARS,
    const.JUPITER,
    const.SATURN,
    const.URANUS,
    const.NEPTUNE,
    const.PLUTO,
]

planet_symbols = {
    const.SUN: "☉",
    const.MOON: "☽",
    const.MERCURY: "☿",
    const.VENUS: "♀",
    const.MARS: "♂",
    const.JUPITER: "♃",
    const.SATURN: "♄",
    const.URANUS: "♅",
    const.NEPTUNE: "♆",
    const.PLUTO: "⯓",
}

aspect_symbols = {
    const.NO_ASPECT: " ",
    const.CONJUNCTION: "☌",
    const.SEXTILE: "⚹",
    const.SQUARE: "□",
    const.TRINE: "△",
    const.OPPOSITION: "☍",
}


def convert_into_stupid_flatlib_format(dt):
    return Datetime(
        dt.strftime("%Y/%m/%d"),
        dt.strftime("%H:%M"),
        dt.utcoffset().total_seconds() / 3600,
    )


here_latitude = 52.52
here_longitude = 13.4


def get_aspects(chart1, chart2, *, threshold):
    for planet1 in chart1.objects:
        for planet2 in chart2.objects:
            aspect = aspects.getAspect(planet1, planet2, const.MAJOR_ASPECTS)
            if aspect.exists() and aspect.orb <= threshold:
                yield aspect


def get_chart(position, dt_naive):
    timezone = pytz.timezone(tf.timezone_at(lat=position.lat, lng=position.lon))
    dt_aware = timezone.localize(dt_naive)
    return Chart(convert_into_stupid_flatlib_format(dt_aware), position, IDs=planets)


def show_aspect(aspect):
    return " ".join(
        [
            planet_symbols[aspect.active.id],
            aspect_symbols[aspect.type],
            planet_symbols[aspect.passive.id],
        ]
    )


@click.command()
@click.option("--natal-latitude", type=click.FLOAT, default=here_latitude)
@click.option("--natal-longitude", type=click.FLOAT, default=here_longitude)
@click.option("--natal-date", type=click.DateTime(), default=datetime.now())
@click.option("--transit-latitude", type=click.FLOAT, default=here_latitude)
@click.option("--transit-longitude", type=click.FLOAT, default=here_longitude)
@click.option("--transit-date", type=click.DateTime(), default=datetime.now())
@click.option("--threshold", type=click.FLOAT, default=5)
def forecast(
    natal_latitude: float,
    natal_longitude: float,
    natal_date: datetime,
    transit_latitude: float,
    transit_longitude: float,
    transit_date: datetime,
    threshold: float,
):
    transit_position = GeoPos(transit_latitude, transit_longitude)
    natal_position = GeoPos(natal_latitude, natal_longitude)
    natal_chart = get_chart(natal_position, natal_date)
    transit_chart = get_chart(transit_position, transit_date)

    offset = 0
    previous_aspects = set(
        show_aspect(a)
        for a in get_aspects(natal_chart, transit_chart, threshold=threshold)
    )
    while True:
        then = transit_date + timedelta(minutes=offset)
        current_chart = get_chart(transit_position, then)
        current_aspects = set(
            show_aspect(a)
            for a in get_aspects(natal_chart, current_chart, threshold=threshold)
        )
        entered = current_aspects - previous_aspects
        exited = previous_aspects - current_aspects
        if entered or exited:
            print(
                then.strftime("%Y-%m-%d %H:%M"),
                "".join([" | +" + a for a in entered] + [" | -" + a for a in exited]),
                sep="",
            )
        previous_aspects = current_aspects
        offset += 1


@click.command()
@click.option("--natal-latitude", type=click.FLOAT, default=here_latitude)
@click.option("--natal-longitude", type=click.FLOAT, default=here_longitude)
@click.option("--natal-date", "-D", type=click.DateTime(), default=datetime.now())
@click.option("--transit-latitude", type=click.FLOAT, default=here_latitude)
@click.option("--transit-longitude", type=click.FLOAT, default=here_longitude)
@click.option("--transit-date", "-d", type=click.DateTime(), default=datetime.now())
@click.option("--threshold", "-t", type=click.FLOAT, default=5)
def current(
    natal_latitude: float,
    natal_longitude: float,
    natal_date: datetime,
    transit_latitude: float,
    transit_longitude: float,
    transit_date: datetime,
    threshold: float,
):
    transit_position = GeoPos(transit_latitude, transit_longitude)
    natal_position = GeoPos(natal_latitude, natal_longitude)
    natal_chart = get_chart(natal_position, natal_date)
    transit_chart = get_chart(transit_position, transit_date)

    relevant_aspects = list(
        get_aspects(natal_chart, transit_chart, threshold=threshold)
    )

    def aspect_switch_date(aspect, *, direction=1, threshold):
        offset = 0
        while True:
            then = transit_date + direction * timedelta(days=offset)
            current_chart = get_chart(transit_position, then)
            aspects = [
                show_aspect(a)
                for a in get_aspects(natal_chart, current_chart, threshold=threshold)
            ]
            if aspect not in aspects:
                return then.date()
            offset += 1

    for aspect in sorted(relevant_aspects, key=operator.attrgetter("orb")):
        aspect_string = show_aspect(aspect)
        print(
            aspect_switch_date(
                aspect_string, direction=-1, threshold=threshold
            ).isoformat(),
            aspect_switch_date(
                aspect_string, direction=1, threshold=threshold
            ).isoformat(),
            aspect_string,
        )
