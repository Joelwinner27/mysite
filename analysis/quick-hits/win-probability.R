# Win probability chart for one game, from Polymarket US prices (same source as the Pi line board).
# Run from the repo root:  Rscript analysis/quick-hits/win-probability.R
# Change the settings block for a new game.

library(jsonlite)
library(dplyr)
library(ggplot2)

# ---- Settings ---------------------------------------------------------------
# Polymarket slug: away team first. The price is the away ("long") team's win chance.
slug      <- "aec-cfb-ind-mich-2026-10-24"
espn_id   <- "401858496"                  # ESPN game id, for plays and scoring markers
long_team <- list(name = "Indiana",  abbr = "IU",   color = "#990000")   # IU crimson
short_team <- list(name = "Michigan", abbr = "MICH", color = "#00274C")  # Michigan blue
out_png   <- "public/analysis/michigan-indiana/wp.png"

options(HTTPUserAgent = "Mozilla/5.0 (joelwinner.org)")
paper <- "#F8F2E6"; ink <- "#231C14"; muted <- "#6E604C"

# ---- Plays from ESPN ----------------------------------------------------------
summary <- fromJSON(paste0("https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=", espn_id),
                    simplifyVector = FALSE)
plays <- bind_rows(lapply(summary$drives$previous, function(d) bind_rows(lapply(d$plays, function(p) tibble(
  time   = as.POSIXct(p$wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  period = p$period$number,
  text   = p$text,
  away   = p$awayScore,
  home   = p$homeScore,
  scoring = isTRUE(p$scoringPlay)
)))))
kickoff  <- min(plays$time)
game_end <- max(plays$time) + 5 * 60

# ---- Prices from Polymarket -------------------------------------------------
hist <- fromJSON(paste0("https://gateway.polymarket.us/v1/price-history?fidelity=1&fixedInterval=1&symbol=", slug))$history
prices <- hist |>
  mutate(time = as.POSIXct(ifelse(timestamp > 1e12, timestamp / 1000, timestamp), origin = "1970-01-01", tz = "UTC")) |>
  filter(abs(longPrice + shortPrice - 1) <= 0.05, longPrice >= 0, longPrice <= 1) |>   # drop placeholder quotes
  arrange(time)

opening <- prices |> filter(time <= kickoff) |> slice_tail(n = 1) |> mutate(time = kickoff)
wp <- bind_rows(opening, prices |> filter(time > kickoff, time <= game_end)) |>
  transmute(time, p = longPrice)
# The market settles at 1/0 after the game; stop at the final whistle.
wp <- wp |> filter(time <= max(plays$time) + 60)

# ---- Scoring plays ----------------------------------------------------------
short_type <- function(x) ifelse(grepl("Field Goal", x), "FG", ifelse(grepl("Safety", x), "SAF", "TD"))
scores <- bind_rows(lapply(summary$scoringPlays, function(s) tibble(
  team = s$team$abbreviation, type = short_type(s$type$text), away = s$awayScore, home = s$homeScore
)))
# Scoring plays come in the same order as the plays flagged as scoring; take their times from there.
scores$time <- plays$time[plays$scoring]
at <- function(t) wp$p[findInterval(as.numeric(t), as.numeric(wp$time), all.inside = TRUE)]
scores <- scores |> mutate(p = at(time + 60), label = paste0(team, " ", type, "  ", away, "-", home),
                           color = ifelse(team == long_team$abbr, long_team$color, short_team$color))

# ---- Key numbers (printed for the post) ---------------------------------------
step_min <- as.numeric(diff(wp$time), units = "mins")
cat(sprintf("Opening: %s %.0f%%\n", long_team$name, 100 * wp$p[1]))
cat(sprintf("%s peak: %.0f%% at %s ET\n", short_team$name, 100 * (1 - min(wp$p)), format(wp$time[which.min(wp$p)], "%H:%M", tz = "America/New_York")))
cat(sprintf("%s favored for %.0f minutes of real time\n", short_team$name, sum(step_min[head(wp$p, -1) < 0.5])))
cat(sprintf("%s low before OT: %.0f%%\n", long_team$name, 100 * min(wp$p)))
print(scores |> transmute(label, time = format(time, "%H:%M", tz = "America/New_York"), p = round(100 * p)))

# ---- Chart -------------------------------------------------------------------
quarters <- plays |> group_by(period) |> summarise(time = min(time), .groups = "drop") |>
  mutate(label = ifelse(period > 4, "OT", paste0("Q", period)))
half <- mean(c(max(plays$time[plays$period == 2]), min(plays$time[plays$period == 3])))

g <- ggplot(wp, aes(time, p)) +
  geom_ribbon(aes(ymin = 0.5, ymax = pmax(p, 0.5)), fill = long_team$color, alpha = 0.22) +
  geom_ribbon(aes(ymin = pmin(p, 0.5), ymax = 0.5), fill = short_team$color, alpha = 0.45) +
  geom_hline(yintercept = 0.5, color = muted, linewidth = 0.3, linetype = "dashed") +
  geom_vline(data = quarters, aes(xintercept = time), color = "#CFC3AD", linewidth = 0.3) +
  geom_text(data = quarters, aes(x = time, y = 1.02, label = label), hjust = -0.2, vjust = 0, size = 3.4, color = muted, family = "mono") +
  annotate("text", x = half, y = 0.5, label = "HALF", vjust = -0.6, size = 3, color = muted, family = "mono") +
  geom_line(color = ink, linewidth = 0.7) +
  geom_point(data = scores, aes(time, p), shape = 21, size = 2.8, stroke = 1.2, fill = paper, color = scores$color) +
  geom_label(data = scores, aes(time, p, label = label), nudge_y = ifelse(scores$p > 0.4, 0.08, -0.08), size = 3.1,
             label.size = 0, fill = paper, color = scores$color, family = "mono", fontface = "bold") +
  annotate("text", x = kickoff, y = 0.96, label = paste0(toupper(long_team$name), " FAVORED"), hjust = 0, color = long_team$color, fontface = "bold", size = 4.2) +
  annotate("text", x = kickoff, y = 0.04, label = paste0(toupper(short_team$name), " FAVORED"), hjust = 0, color = short_team$color, fontface = "bold", size = 4.2) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1.06), expand = c(0, 0)) +
  scale_x_datetime(expand = expansion(mult = c(0.01, 0.03))) +
  labs(title = paste0(long_team$name, " ", tail(scores$away, 1), ", ", short_team$name, " ", tail(scores$home, 1),
                      if (max(plays$period) > 4) " (OT)" else ""),
       subtitle = paste0(long_team$name, "'s chance to win, from Polymarket prices"),
       caption = "Data: Polymarket US, ESPN  |  joelwinner.org", x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = paper, color = NA),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = muted, family = "mono"),
        plot.title = element_text(face = "bold", color = ink, size = 18),
        plot.subtitle = element_text(color = muted, margin = margin(b = 12)),
        plot.caption = element_text(color = muted, family = "mono", size = 9),
        plot.margin = margin(16, 20, 12, 12))

ggsave(out_png, g, width = 8, height = 5, dpi = 200, bg = paper)
cat("Saved", out_png, "\n")
