---
title: "Michigan was favored for four minutes. Indiana had the other three hours."
type: quick-hit
sport: "CFB"
date: 2026-10-26
summary: "Polymarket had Indiana as the favorite almost wire to wire in a 13–6 overtime win. Michigan's one window came on its last drive of regulation, and it closed in overtime."
stats:
  - { value: "60%", label: "Michigan's peak win chance, on its last drive of regulation" }
  - { value: "1.4", label: "Michigan yards per carry (57 yards on 42 rushes)" }
  - { value: "2 of 4", label: "Michigan field goals made; both misses ended drives of 40+ yards" }
chart: "/analysis/michigan-indiana/wp.png"
chartCaption: "Indiana's chance to win, minute by minute, from Polymarket prices. Shaded red when Indiana was favored, blue when Michigan was."
thumbnail: "/analysis/michigan-indiana/wp.png"
data: "Polymarket US price history (one minute intervals) and ESPN play by play"
code: "https://github.com/Joelwinner27/mysite/blob/main/analysis/quick-hits/win-probability.R"
tools: ["R", "ggplot2", "jsonlite", "Polymarket API"]
---

Indiana opened as a 74% favorite in Ann Arbor and spent almost the entire afternoon there. Two Nico Radicic field goals put the Hoosiers up 6–0 in the first half, and Michigan didn't help itself: a fumble, an interception and a missed field goal before the break.

The market only flipped once. Michigan's final drive of regulation went 53 yards and pushed its win chance to 60%, the only stretch all day it was the favorite. The drive stalled, Trey Butkowski tied it at 6 with a 26 yard field goal as time expired, and Indiana went right back to about 57%. In overtime Josh Hoover ran it in from 4 yards out, and Michigan turned it over on downs.

The number that explains it is 1.4. Michigan ran 42 times for 57 yards, so every drive had to be built through the air, and two of the ones that got close ended in missed kicks. Michigan falls to 4–3 and goes to Rutgers next; Indiana hosts Minnesota.
