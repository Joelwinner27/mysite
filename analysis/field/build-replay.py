"""Build the games the home page field animation replays.

For each game below, pulls ESPN's play-by-play, keeps the drives listed, and writes
public/replays/<slug>.json plus an index at src/data/field-games.json. A visitor gets one game
at random. Run from the repo root:  python3 analysis/field/build-replay.py

Plays ESPN describes in ways the animation can't read (returns, trick plays) are set by hand
in SPECIAL, keyed by a bit of the play's text. Yard lines there are from the offense's side:
0 is its own goal line, 100 the goal it attacks.

Each snap also gets the home team's win probability: ESPN's own where it has one (2016 and
later, mostly), otherwise a small score/clock/field position model (see model_wp). Pass targets
get their real position (WR, TE or RB) from ESPN's athlete pages, cached in positions.json, so
the routes can match who caught the ball.
"""
import json
import os
import re
import urllib.request
import zlib
from math import erf, sqrt

# Uniform colors are approximate for that day: the home team (as ESPN lists it) in its dark
# jersey, the road team in white.
TEAMS = {
    "OSU": {"name": "Ohio State", "dark": ["#bb0000", "#a9aeb1", "#bb0000", "#c4c8ca"], "white": ["#f6f3ec", "#a9aeb1", "#bb0000", "#c4c8ca"], "zone": ["OHIO STATE", "#a51d1d"]},
    "MICH": {"name": "Michigan", "dark": ["#00274c", "#00274c", "#ffcb05", "#ffcb05"], "white": ["#f6f3ec", "#00274c", "#ffcb05", "#ffcb05"], "zone": ["MICHIGAN", "#00274c"]},
    "ND": {"name": "Notre Dame", "dark": ["#0c2340", "#c99700", "#c99700", "#c99700"], "white": ["#f6f3ec", "#c99700", "#c99700", "#c99700"], "zone": ["NOTRE DAME", "#0c2340"]},
    "ALA": {"name": "Alabama", "dark": ["#9e1b32", "#9e1b32", "#f6f3ec", "#f6f3ec"], "white": ["#f6f3ec", "#9e1b32", "#f6f3ec", "#f6f3ec"], "zone": ["ALABAMA", "#9e1b32"]},
    "MSU": {"name": "Michigan State", "dark": ["#18453b", "#18453b", "#f6f3ec", "#f6f3ec"], "white": ["#f6f3ec", "#18453b", "#f6f3ec", "#f6f3ec"], "zone": ["SPARTANS", "#18453b"]},
    "AUB": {"name": "Auburn", "dark": ["#0c2340", "#f6f3ec", "#e87722", "#f6f3ec"], "white": ["#f6f3ec", "#f6f3ec", "#e87722", "#f6f3ec"], "zone": ["AUBURN", "#0c2340"]},
    "BSU": {"name": "Boise State", "dark": ["#0033a0", "#0033a0", "#d64309", "#0033a0"], "white": ["#f6f3ec", "#0033a0", "#d64309", "#0033a0"], "zone": ["BOISE STATE", "#0033a0"]},
    "OKLA": {"name": "Oklahoma", "dark": ["#841617", "#841617", "#f6f3ec", "#f6f3ec"], "white": ["#f6f3ec", "#841617", "#f6f3ec", "#f6f3ec"], "zone": ["OKLAHOMA", "#841617"]},
    "TEX": {"name": "Texas", "dark": ["#bf5700", "#f6f3ec", "#bf5700", "#f6f3ec"], "white": ["#f6f3ec", "#f6f3ec", "#bf5700", "#f6f3ec"], "zone": ["TEXAS", "#bf5700"]},
}

GAMES = [
    {
        "slug": "osu-mich-2016", "event": "400869650", "title": "Ohio State 30, Michigan 27 (2OT)", "date": "Nov. 26, 2016",
        "drives": [25, 27, 28, 29, 30], "zones": [["BUCKEYES", "#a51d1d"], ["OHIO STATE", "#a51d1d"]], "final": "OHIO STATE WINS",
        "still": "run for 1 yd to the Mich 15",
        "notes": {
            "run for 1 yd to the Mich 15": "4th and 1. The spot is still argued about in Ann Arbor.",
            "Curtis Samuel run for 15 yds": "Ballgame.",
            "pass complete to Marcus Baugh for 13 yds": "3rd and 18 after two lost-yardage runs.",
        },
    },
    {
        "slug": "osu-mich-2006", "event": "263220194", "title": "Ohio State 42, Michigan 39", "date": "Nov. 18, 2006",
        "drives": [23, 24, 25], "zones": [["BUCKEYES", "#a51d1d"], ["OHIO STATE", "#a51d1d"]], "final": "OHIO STATE WINS",
        "still": "Brian Robiskie for 13 yards",
        "notes": {"Brian Robiskie for 13 yards": "No. 1 vs. No. 2, the day after Bo Schembechler died."},
    },
    {
        "slug": "sugar-2015", "event": "400610178", "title": "Sugar Bowl: Ohio State 42, Alabama 35", "date": "Jan. 1, 2015",
        "drives": [26, 27, 28, 29], "final": "OHIO STATE WINS", "still": "Ezekiel Elliott 85",
        "notes": {"Ezekiel Elliott 85": "Ohio State's third-string quarterback was starting his second game."},
        "special": {"Tyvis Powell": {"kind": "turnover", "action": "pass", "at": [100, -2], "to": [71, -6], "who": "FS"}},
    },
    {
        "slug": "cotton-2025", "event": "401677191", "title": "Cotton Bowl: Ohio State 28, Texas 14", "date": "Jan. 10, 2025",
        "drives": [19, 20], "final": "SCOOP AND SCORE", "still": "Jack Sawyer",
        "notes": {"Jack Sawyer": "Ohio State went on to win the national title."},
        "special": {"Jack Sawyer": {"kind": "turnover", "action": "sack", "at": [83, 3], "to": [-4, 6], "who": "DE2"}},
    },
    {
        "slug": "nd-mich-2011", "event": "312530130", "title": "Michigan 35, Notre Dame 31", "date": "Sept. 10, 2011",
        "drives": [24, 25, 26], "zones": [["MICHIGAN", "#00274c"], ["MICHIGAN", "#00274c"]], "final": "MICHIGAN WINS",
        "still": "Roy Roundtree",
        "notes": {"Roy Roundtree": "Two seconds left. Three touchdowns in the last 72 seconds."},
    },
    {
        "slug": "rose-2024", "event": "401551786", "title": "Rose Bowl: Michigan 27, Alabama 20 (OT)", "date": "Jan. 1, 2024",
        "drives": [21, 24, 25], "final": "MICHIGAN WINS", "still": "Jalen Milroe run for 1 yd to the MICH 2",
        "notes": {"Jalen Milroe run for 1 yd to the MICH 2": "Stopped short on 4th and goal. Michigan went on to win the national title."},
    },
    {
        "slug": "msu-mich-2015", "event": "400763542", "title": "Michigan State 27, Michigan 23", "date": "Oct. 17, 2015",
        "drives": [24, 25], "zones": [["MICHIGAN", "#00274c"], ["MICHIGAN", "#00274c"]], "final": "MICHIGAN STATE WINS",
        "still": "Blake O'Neill",
        "notes": {"Blake O'Neill": "Michigan led with 10 seconds left and only had to punt."},
        "special": {"Blake O'Neill": {"kind": "turnover", "action": "punt", "at": [38, 2], "to": [-4, -4], "who": "LB2"}},
    },
    {
        "slug": "kick-six-2013", "event": "333340002", "title": "Auburn 34, Alabama 28", "date": "Nov. 30, 2013",
        "drives": [23, 24], "zones": [["AUBURN", "#0c2340"], ["AUBURN", "#0c2340"]], "final": "AUBURN WINS",
        "still": "Adam Griffith 57",
        "notes": {
            "T.J. Yeldon rush for 24": "The clock hit zero; a review put one second back.",
            "Adam Griffith 57": "The Kick Six: caught 9 yards deep in the end zone and returned 109.",
        },
        "special": {"Adam Griffith 57": {"kind": "turnover", "action": "fg", "at": [109, 6], "to": [-4, -3], "who": "SS", "wait": [109, 6]}},
    },
    {
        "slug": "fiesta-2007", "event": "270010201", "title": "Fiesta Bowl: Boise State 43, Oklahoma 42 (OT)", "date": "Jan. 1, 2007",
        "drives": [28, 29, 30, 31], "final": "BOISE STATE WINS", "still": "lateral to Jerard Rabb",
        "team": {"Adrian Peterson rush for 25": "OKLA", "Garrett Hartley extra point": "OKLA"},
        "notes": {
            "lateral to Jerard Rabb": "Hook and lateral on 4th and 18 with 7 seconds left.",
            "Vinny Perretta pass complete": "A halfback pass on 4th and 2.",
            "Ian Johnson rush GOOD": "The Statue of Liberty, for the win.",
        },
        "special": {
            "Marcus Walker": {"kind": "turnover", "action": "pass", "at": [34, -9], "to": [-4, -6], "who": "CB1"},
            "lateral to Jerard Rabb": {"kind": "hooklat", "catch": 15},
            "Vinny Perretta pass complete": {"kind": "hbpass"},
            "Ian Johnson rush GOOD": {"kind": "statue"},
        },
    },
]

SKIP = {"Timeout", "End of Game", "End Period", "End of Half", "End of Regulation", "Kickoff", "Kickoff Return (Offense)", "Two Minute Warning"}
QB_NAMES = ("J.T. Barrett", "Wilton Speight", "Troy Smith", "Chad Henne", "Cardale Jones", "Blake Sims", "Will Howard", "Quinn Ewers",
            "Denard Robinson", "Tommy Rees", "J.J. McCarthy", "Jalen Milroe", "Connor Cook", "Nick Marshall", "AJ McCarron",
            "Jared Zabransky", "Paul Thompson")


def get(url):
    return json.load(urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (joelwinner.org)"})))


def fetch(event):
    return get(f"https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event={event}")


POS_FILE = "analysis/field/positions.json"
POSITIONS = json.load(open(POS_FILE)) if os.path.exists(POS_FILE) else {}


def position(athlete_id, season):
    if athlete_id not in POSITIONS:
        try:
            a = get(f"https://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{season}/athletes/{athlete_id}")
            POSITIONS[athlete_id] = a.get("position", {}).get("abbreviation", "")
        except Exception:
            POSITIONS[athlete_id] = ""
    return POSITIONS[athlete_id]


def seconds_left(p):
    q = p["period"]["number"]
    if q > 4:
        return None
    m, s = p["clock"]["displayValue"].split(":")
    return (4 - q) * 900 + int(m) * 60 + int(s)


def model_wp(row, secs, home):
    """Home win probability before a snap, for games ESPN has no numbers for.

    The offense's lead plus the points its field position is worth, against a spread of final
    margins that narrows as the clock runs out: P = Phi((lead + EP) / (13 * sqrt(minutes / 60))).
    Field position counts for less in the last couple of minutes, when there's no time to use it.
    Overtime is treated as about four minutes of football.
    """
    ti = 1 if row["team"] == home else 0
    lead = row["score"][ti] - row["score"][1 - ti]
    ep = {"Extra point": 0.95, "Two-point try": 0.9}.get(row["down"])
    if ep is None:
        ep = (0.07 * row["x"] - 0.5) * (1 if secs is None else min(1, (secs + 60) / 180))
    t = 240 if secs is None else secs
    p = 0.5 * (1 + erf((lead + ep) / (13 * sqrt((t + 180) / 3600)) / sqrt(2)))
    return round(p if ti else 1 - p, 3)


def lean(name):
    return 1 if zlib.crc32(name.encode()) % 2 else -1


def clock(p):
    q = p["period"]["number"]
    return {5: "OT", 6: "2OT", 7: "3OT"}.get(q) or f"{['1st', '2nd', '3rd', '4th'][q - 1]} {p['clock']['displayValue']}"


def kind(p):
    t, text = p["type"]["text"], p["text"]
    if t == "Penalty":
        return "inc" if "pass incomplete" in text else "flag"
    if text.startswith("TEAM run"):
        return "kneel"
    if t.startswith("Rush") or t == "Run":
        return "qbrun" if text.startswith(QB_NAMES) else "run"
    if t.startswith("Pass Reception") or t.startswith("Pass Completion") or t == "Passing Touchdown":
        return "pass"
    if t == "Pass Incompletion":
        return "inc"
    if t == "Sack":
        return "sack"
    if t == "Punt":
        return "punt"
    if t.startswith("Field Goal"):
        return "fg"
    return None


def tidy(text):
    text = re.sub(r",? \(\w[\w. ]* (KICK|Kick)\)", "", text)
    text = re.sub(r" \((\w[\w. ]*) (Pass to|pass to|Run|run)[\w.' -]*\)", "", text)
    text = text.replace("sacked sacked", "sacked").replace("Sawayer", "Sawyer").strip()
    m = re.search(r"(\w[\w.]*), (\w[\w.]*) pass incomplete to (\w[\w'.]*), (\w[\w.]*), PENALTY (\w+) (pass interference)", text)
    if m:
        return f"{m[2]} {m[1]} pass incomplete to {m[4]} {m[3]}. Flag: {m[6]} on {m[5].title()}."
    m = re.match(r"(OHIO ST|MICHIGAN) Penalty, ([\w ]+) \(", text)
    if m:
        return f"Flag: {m[2].lower()} on {'Ohio State' if m[1] == 'OHIO ST' else 'Michigan'}, 5 yards."
    for a, b in [("OhSt", "Ohio State"), ("Mich ", "Michigan "), ("MchSt", "Michigan State"), ("NDame", "Notre Dame"), ("Alab ", "Alabama "),
                 ("Aub ", "Auburn "), ("BoiSt", "Boise State"), ("Okla ", "Oklahoma "), (" yds", " yards"), (" yd ", " yard ")]:
        text = text.replace(a, b)
    text = re.sub(r"\s+", " ", text).strip()
    return text if len(text) <= 150 else text[:text.rfind(" ", 0, 147)] + "…"


def spot(ytez, off, deff):
    if ytez == 50:
        return "the 50"
    return f"{off} {100 - ytez}" if ytez > 50 else f"{deff} {ytez}"


def build(g):
    game = fetch(g["event"])
    comp = game["header"]["competitions"][0]["competitors"]
    side = {c["homeAway"]: c["team"]["abbreviation"] for c in comp}
    ids = {c["team"]["id"]: c["team"]["abbreviation"] for c in comp}
    home, away = side["home"], side["away"]
    drives = game["drives"]["previous"]
    season = game["header"]["season"]["year"]
    athletes = {a["athlete"]["displayName"]: a["athlete"]["id"] for team in game["boxscore"].get("players", [])
                for cat in team["statistics"] for a in cat["athletes"]}
    # score before the first replayed play, from the last play ahead of it
    first_id = next(p["id"] for p in drives[g["drives"][0]]["plays"])
    before = [0, 0]
    for d in drives:
        for p in d["plays"]:
            if p["id"] == first_id:
                break
            before = [p["awayScore"], p["homeScore"]]
        else:
            continue
        break
    rows = []
    for di in g["drives"]:
        keep = [p for p in drives[di]["plays"] if p["type"]["text"] not in SKIP and not any(d in p["text"] for d in g.get("drop", []))]
        for i, p in enumerate(keep):
            text, s = p["text"], p.get("start", {})
            team = next((t for k, t in g.get("team", {}).items() if k in text), None) or ids.get(s.get("team", {}).get("id")) or drives[di]["team"]["abbreviation"]
            other = away if team == home else home
            special = next((v for k, v in g.get("special", {}).items() if k in text), None)
            two = "Two-point" in text
            if p["type"]["text"].startswith("Extra Point"):
                k = (special or {}).get("kind") or (("pass" if " pass " in text else "run") if two else "fg")
                row = {"kind": k, "x": 97, "gain": 3 if two else 0, "toGain": None, "down": "Two-point try" if two else "Extra point",
                       "td": two, "good": "GOOD" in text.upper()}
            else:
                k = (special or {}).get("kind") or kind(p)
                if not k:
                    continue
                ytez = s["yardsToEndzone"]
                if p.get("scoringPlay") or i + 1 >= len(keep) or k in ("punt", "turnover"):
                    gain = 0 if k in ("fg", "punt", "turnover") else p.get("statYardage", 0)
                else:
                    gain = ytez - keep[i + 1]["start"]["yardsToEndzone"]
                dist = s["distance"]
                down = f"{['1st', '2nd', '3rd', '4th'][s['down'] - 1]} & {'Goal' if dist <= 0 or dist >= ytez else dist} at {spot(ytez, team, other)}"
                row = {"kind": k, "x": 100 - ytez, "gain": gain, "toGain": None if dist <= 0 or dist >= ytez else 100 - ytez + dist, "down": down,
                       "td": k != "turnover" and ("Touchdown" in p["type"]["text"] or "TOUCHDOWN" in text.upper() or " Yd Run" in text or " Yd pass" in text)}
                if k == "fg":
                    row["good"] = "Good" in p["type"]["text"] or "GOOD" in text
                if k == "punt":
                    m = re.search(r"punt for (\d+) y", text)
                    row["punt"] = int(m[1]) if m else 40
                if row["td"]:
                    row["gain"] = ytez
                if special:
                    row.update({key: v for key, v in special.items() if key != "kind"})
                    if k == "turnover":
                        row["defTD"] = special["to"][0] < 0
            row.update({"team": team, "clock": clock(p), "text": tidy(text), "pid": p["id"], "secs": seconds_left(p)})
            # who the ball went to, so the route matches: a back, a tight end or a receiver
            m = (re.search(r"pass (?:complete |incomplete )?to ([A-Z][\w.'-]*(?: [A-Z][\w.'-]*)*?)(?= for|\.|,|$| \()", row["text"])
                 or re.match(r"([A-Z][\w.'-]*(?: [A-Z][\w.'-]*)*?) \d+ Yd pass from", row["text"]))
            if m and row["kind"] in ("pass", "inc"):
                name = m[1] if m[1] in athletes else m[1].removesuffix(".")
                pos = position(athletes[name], season) if name in athletes else ""
                row["target"] = "RB" if pos in ("RB", "FB", "HB") else "TE" if pos == "TE" else "WR"
                row["side"] = lean(name)
            elif row["kind"] in ("run", "qbrun"):
                row["side"] = lean(row["text"])
            for key, note in g.get("notes", {}).items():
                if key in text:
                    row["note"] = note
            if g.get("still") and g["still"] in text:
                row["still"] = True
            rows.append(row)
            # a conversion folded into the touchdown's text gets its own play
            failed = re.search(r"fail|no good|blocked", text, re.I)
            m = re.search(r"\((\w[\w. ]*?) (Kick|KICK)\)", text)
            conv = re.search(r"\((\w[\w. ]*?) (Pass to|pass to) ([\w.' ]+?)(?: for [\w -]+)?\)", text)
            run2 = re.search(r"\((\w[\w. ]*?) (Run|run)\)", text)
            if (row["td"] or row.get("defTD")) and not failed and (m or conv or run2) and row["down"] != "Two-point try":
                if row.get("defTD"):
                    row["xp"] = True  # fold the kick into the return so the replay can end on it
                elif m:
                    rows.append({"team": team if row["td"] else other, "kind": "fg", "good": True, "x": 97, "gain": 0, "toGain": None, "down": "Extra point",
                                 "td": False, "clock": clock(p), "text": f"{m[1]} extra point is good", "pid": p["id"], "secs": row["secs"], "conv": True})
                else:
                    rows.append({"team": team, "kind": "pass" if conv else "run", "x": 97, "gain": 3, "toGain": None, "down": "Two-point try", "td": True,
                                 "clock": clock(p), "text": f"Two-point try good: {conv[1]} pass to {conv[3]}" if conv else f"Two-point try good: {run2[1]} run",
                                 "pid": p["id"], "secs": row["secs"], "conv": True, "side": lean(text)})
    # keep score ourselves: ESPN's running score sometimes moves a play early
    score = before
    for r in rows:
        ti = 0 if r["team"] == away else 1
        pts = [0, 0]
        if r["down"] == "Two-point try":
            pts[ti] = 2 if r.get("good", True) else 0
        elif r["down"] == "Extra point":
            pts[ti] = 1 if r.get("good") else 0
        elif r["kind"] == "fg":
            pts[ti] = 3 if r.get("good") else 0
        elif r["td"]:
            pts[ti] = 6
        elif r.get("defTD"):
            pts[1 - ti] = 7 if r.get("xp") else 6
        r["score"], score = score, [score[0] + pts[0], score[1] + pts[1]]
        r["after"] = score
    for a, b in zip(rows, rows[1:]):
        if a["team"] != b["team"]:
            a["endDrive"] = True
    for r in rows:
        if r["kind"] == "punt":
            r["endDrive"] = True
    rows[-1]["endDrive"] = rows[-1]["final"] = True
    final = [int(c["score"]) for c in sorted(comp, key=lambda c: c["homeAway"] != "away")]
    # win probability before each snap: ESPN's number after the previous play, or the model's
    espn = {w["playId"]: w["homeWinPercentage"] for w in game.get("winprobability", [])}
    order = [p["id"] for d in drives for p in d["plays"]]
    for r in rows:
        if espn:
            # a conversion split out of a touchdown's text comes right after that play
            i = order.index(r["pid"]) + (1 if r.get("conv") else 0)
            prev = next((order[j] for j in range(i - 1, -1, -1) if order[j] in espn), None)
            r["wp"] = round(espn[prev], 3) if prev else 0.5
        else:
            r["wp"] = model_wp(r, r["secs"], home)
        for k in ("conv", "pid", "secs"):
            r.pop(k, None)
    if rows[-1]["after"] != final and g["drives"][-1] == len(drives) - 1:
        print(f"  !! {g['slug']}: replay ends {rows[-1]['after']}, ESPN final {final}")
    first = rows[0]
    lead = first["score"][0 if first["team"] == away else 1] - first["score"][1 if first["team"] == away else 0]
    situation = "tied" if lead == 0 else f"up {lead}" if lead > 0 else f"down {-lead}"
    intro = f"{TEAMS[first['team']]['name']} ball, {situation}, {first['clock'].replace('4th ', '').strip()}{' left' if first['clock'].startswith('4th') else ''}."
    zones = g.get("zones") or None
    return {
        "slug": g["slug"], "title": g["title"], "date": g["date"], "away": away, "home": home, "final": g["final"], "intro": intro,
        "teams": {away: {**dict(zip(["jersey", "helmet", "stripe", "pants"], TEAMS[away]["white"])), "zone": TEAMS[away]["zone"]},
                  home: {**dict(zip(["jersey", "helmet", "stripe", "pants"], TEAMS[home]["dark"])), "zone": TEAMS[home]["zone"]}},
        # midfield: the home team's name in plain block letters, or the bowl's name at a bowl
        "midfield": (g["title"].split(":")[0].upper(), "#231c14") if ":" in g["title"] else (TEAMS[home]["zone"][0], TEAMS[home]["zone"][1]),
        "zones": zones, "plays": rows,
        "wpSource": "ESPN" if espn else "model", "wpFinal": 1 if final[1] > final[0] else 0,
    }


index = []
for g in GAMES:
    replay = build(g)
    with open(f"public/replays/{g['slug']}.json", "w") as f:
        json.dump(replay, f, separators=(",", ":"))
    index.append({"slug": g["slug"], "title": g["title"], "date": g["date"]})
    print(f"\n== {g['slug']}: {len(replay['plays'])} plays  ({replay['intro']})")
    for p in replay["plays"]:
        flags = " ".join(k for k in ("td", "endDrive", "final", "still") if p.get(k))
        print(f"  {p['team']:4} {p['kind']:8} x={p['x']:3} gain={p['gain']:4} {p['down']:22.22} {p['score']}->{p['after']} wp={p['wp']:.2f} {p.get('target', ''):2} {p.get('side', ''):2} {flags:18} {p['text'][:50]}")
with open("src/data/field-games.json", "w") as f:
    json.dump(index, f, indent=1)
with open(POS_FILE, "w") as f:
    json.dump(POSITIONS, f, indent=0, sort_keys=True)
