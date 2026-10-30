---
name: astrology
description: Cast and read horary charts and the user's natal chart with ephemeris-cli, and connect the two (transits, profections, solar return). Use whenever the user asks for a horary chart, "my situation", a chart for a moment, whether a time suits something, or how something relates to their birth chart.
---

# Casting

Use `ephemeris-cli` from https://code.kmein.de/kfm/ephemeris-service (its
README and `--help` document the commands). Run it straight from the flake:

    nix run git+https://code.kmein.de/kfm/ephemeris-service#ephemeris-cli -- \
      natal --when "$(date -u +%Y-%m-%dT%H:%M:%SZ)" --lat <lat> --lon <lon> \
      --planets traditional --aspects conjunction,sextile,square,trine,opposition

If no location is given, use Berlin (52.52, 13.405) and say so. The CLI gives
only the ascendant and MC, so derive whole-sign houses from the ascendant. It
lists aspects by orb and does not say whether they are applying or
separating. For that, for the Moon's next aspects (void of course) and for
sign ingresses, run `events` over the next day or two with the same flags.
All times are UTC. Report local time.

`--when` takes UTC. Convert a local birth time with the zone's own DST rule:

    date -u -d 'TZ="Europe/Berlin" 1999-10-22 06:32' +%FT%TZ

Do not use `TZ=… date -d … -u`: `-u` overrides TZ and leaves the time
unconverted.

# The user's natal chart

Born 1999-10-22, 06:32 CEST, in Kassel (51.3127, 9.4797), which is
`--when 1999-10-22T04:32:00Z`. Cast it fresh each time rather than relying on
remembered positions.

# Horary reading

Read traditionally, following Lilly. First check the strictures: ascendant
under 3° or over 27°, void-of-course Moon, Moon in the via combusta. Then
look at the significators, their dignity (essential dignity including terms
and face; use night triplicity rulers when the Sun is below the horizon),
retrogradation and applying aspects. The Moon's last aspect says what has
just happened. For a general "my situation" question, the querent is the
ruler of the 1st with the Moon as co-significator. When asked whether a moment
suits something, also name a better window from the events sweep.

# Connecting a horary to the natal chart

- Place the horary planets and angles in the natal whole-sign houses. Look for
  horary planets sitting on natal angles or planets, especially a horary
  axis (e.g. an opposition) that falls on the natal ASC/DSC or MC/IC.
- Notice when a horary significator is the natal chart ruler. That makes
  the reading personal.
- Transits: run `events` with `--transits-to <natal UTC> --lat --lon`,
  `--no-eclipses --no-retrogrades --no-sign-changes`, and `--planets
  sun,mercury,venus,mars,jupiter,saturn` (leave out the Moon, or it floods
  the output). Sweep a few months, or about a year for Saturn.
- Annual profection: age in completed years mod 12 counts houses from the
  ascendant sign (age 0 is the 1st). The lord of that sign is the lord of the
  year. Note when a birthday is close, because the lord changes then. Give
  the solar return time, which is the natal Sun conjunct transiting Sun line
  in the sweep.

## Reading the transits output

- Each line is `<natal point> <aspect> <transiting planet>`, with the natal
  point first.
- `orb 0.00°` is printed on every line and means nothing.
- Spans and "exact" times are clipped to the sweep window. An "exact" time at
  the window's start or end, or on a station date, means the closest approach
  or a clipped span, not a real exact hit. Check against positions.
- Transits to the ascendant and MC are not reported. Find them by casting
  `natal --planets saturn,jupiter --when <date>` at sampled dates and
  bracketing the crossing. Say that it was worked out by hand.
- Orbs are wide by default (astrodienst), so the list is long. Pick out the
  slow planets, stations on natal points, and hits to the chart ruler, the
  Sun, the Moon and the angles.
