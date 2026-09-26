---
title: "Pi Line Board"
author: "Joel Winner"
date: "2026-09-26"
---

A Las Vegas sportsbook–style line board for college football and the NFL that runs all day on a Raspberry Pi plugged into an old TV. It shows live scores, spreads, totals and moneylines, settles who covered once games end, and has a full-screen view of any game with a live field.

![NFL line board](/projects/pi-scoreboard/nfl-board.png)

**What it does**

- Every game's score, spread, total and moneyline, with college games sorted by AP ranking and NFL games grouped by kickoff slot
- A game view with the line of scrimmage, first-down marker and each team's top passer, rusher and receiver
- NFL fantasy leaders by position and a "My Players" screen, using ESPN's PPR scoring
- A phone remote, opened by scanning a QR code on the TV, to switch leagues, filter games and open any game

![Game detail view](/projects/pi-scoreboard/game-detail.png)

**How it's built**

The board is one HTML/JavaScript page that pulls live data from ESPN's public APIs. A small Python server on the Pi serves it and relays the phone remote. The Pi runs everything as services with a watchdog, so it recovers on its own from ESPN outages, frozen pages and power cuts.

[Try the live demo](https://joelwinner27.github.io/pi-scoreboard/LineBoard.html) · [Source code on GitHub](https://github.com/Joelwinner27/pi-scoreboard)
