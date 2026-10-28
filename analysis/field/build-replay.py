"""Build the play list the home page field animation replays.

Pulls ESPN's play-by-play for Ohio State 30, Michigan 27 (2OT), Nov. 26, 2016, keeps Ohio State's
tying drive at the end of regulation plus all four overtime possessions, and writes
src/data/field-replay.json. Run from the repo root:  python3 analysis/field/build-replay.py
"""
import json
import re
import urllib.request

EVENT = "400869650"
OUT = "src/data/field-replay.json"
QBS = ("J.T. Barrett", "Wilton Speight")
NAMES = {"OSU": "Ohio State", "MICH": "Michigan"}
SKIP = {"Timeout", "End of Game", "End Period", "End of Half", "Kickoff Return (Offense)"}
# A line of context under a few plays. Edit freely.
NOTES = {
    "run for 1 yd to the Mich 15": "4th and 1. The spot is still argued about in Ann Arbor.",
    "Curtis Samuel run for 15 yds": "Ballgame.",
    "pass complete to Marcus Baugh for 13 yds": "3rd and 18 after two lost-yardage runs.",
}

url = f"https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event={EVENT}"
req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (joelwinner.org)"})
game = json.load(urllib.request.urlopen(req))
drives = [
    d for d in game["drives"]["previous"]
    if any(p["period"]["number"] >= 5 or p["text"].startswith("Tyler Durbin 23 Yd Field Goal") for p in d["plays"])
]


def clock(p):
    q = p["period"]["number"]
    return {5: "OT", 6: "2OT"}.get(q) or f"{['1st', '2nd', '3rd', '4th'][q - 1]} {p['clock']['displayValue']}"


def kind(p):
    t, text = p["type"]["text"], p["text"]
    if t == "Penalty":
        return "inc" if "pass incomplete" in text else "flag"
    if t.startswith("Rush"):
        return "qbrun" if text.startswith(QBS) else "run"
    return {"Pass Reception": "pass", "Passing Touchdown": "pass", "Pass Incompletion": "inc", "Sack": "sack"}.get(t) or (
        "fg" if t.startswith("Field Goal") else None
    )


def tidy(text):
    text = re.sub(r",? \(\w[\w. ]* KICK\)", "", text).strip()
    m = re.search(r"pass incomplete to (\w+), (\w[\w.]*), PENALTY (\w+) (pass interference)", text)
    if m:
        return f"J.T. Barrett pass incomplete to {m[2]} {m[1]}. Flag: {m[4]} on {m[3].title()}, 15 yards."
    m = re.match(r"(OHIO ST|MICHIGAN) Penalty, ([\w ]+) \(", text)
    if m:
        return f"Flag: {m[2].lower()} on {'Ohio State' if m[1] == 'OHIO ST' else 'Michigan'}, 5 yards."
    return text.replace("OhSt", "Ohio State").replace("Mich ", "Michigan ")


plays, score = [], [17, 14]  # Michigan, Ohio State before the drive
for d in drives:
    team = d["team"]["abbreviation"]
    keep = [p for p in d["plays"] if p["type"]["text"] not in SKIP]
    for i, p in enumerate(keep):
        k = kind(p)
        if not k:
            continue
        s, text = p["start"], p["text"]
        x = 100 - s["yardsToEndzone"]
        scoring = p.get("scoringPlay", False)
        if i + 1 < len(keep) and not scoring:
            gain = s["yardsToEndzone"] - keep[i + 1]["start"]["yardsToEndzone"]
        else:
            gain = 0 if k == "fg" else p.get("statYardage", 0)
        after = [p["awayScore"], p["homeScore"]]
        pat = re.search(r"\((\w[\w. ]*) KICK\)", text)
        row = {
            "team": team,
            "kind": k,
            "x": x,
            "gain": gain,
            "toGain": None if s["distance"] <= 0 or s["distance"] >= s["yardsToEndzone"] else x + s["distance"],
            "down": s["downDistanceText"].replace("& 0", "& Goal"),
            "clock": clock(p),
            "text": tidy(text),
            "score": score,
            "after": [after[0] - (pat and team == "MICH"), after[1] - (pat and team == "OSU")] if pat else after,
            "td": "Touchdown" in p["type"]["text"],
        }
        if k == "fg":
            row["good"] = "Good" in p["type"]["text"]
        for key, note in NOTES.items():
            if key in text:
                row["note"] = note
        plays.append(row)
        score = row["after"]
        if pat:
            plays.append({
                "team": team, "kind": "fg", "good": True, "x": 97, "gain": 0, "toGain": None, "down": "Extra point",
                "clock": clock(p), "text": f"{pat[1]} extra point is good", "score": score, "after": after, "td": False,
            })
            score = after
    plays[-1]["endDrive"] = True

plays[-1]["final"] = True
replay = {
    "title": "Ohio State 30, Michigan 27 (2OT)",
    "date": "Nov. 26, 2016",
    "names": NAMES,
    "plays": plays,
}
with open(OUT, "w") as f:
    json.dump(replay, f, indent=1)
print(f"{len(plays)} plays -> {OUT}")
for p in plays:
    print(f"{p['team']:4} {p['kind']:5} x={p['x']:3} gain={p['gain']:3} {p['down']:22} {p['score']}->{p['after']} {p['text'][:70]}")
