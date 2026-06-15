library(tidyverse)
library(baseballr)

Ohtani <- playerid_lookup(last_name = "Ohtani", first_name ="Shohei" )

Ohtani.Data <- scrape_statcast_savant_pitcher_all(start_date = "2025-03-17", end_date = Sys.Date(), pitcherid = Ohtani$mlbam_id)

Ohtani.Clean <- Ohtani.Data %>%
  mutate(
    pfx_x_in = -12 * pfx_x,
    pfx_z_in = 12 * pfx_z,
    stand = recode(stand, "R" = "Right", "L" = "Left")
  )
Ohtani.Clean$stand <- as.factor(Ohtani.Clean$stand)
table(Ohtani.Clean$pitch_name)

Pitch_Colors <- c(
  "4-Seam Fastball" = "dodgerblue",
  "Curveball" = "pink",
  "Cutter", "red",
  "Sinker" = "lightgreen",
  "Slider" = "violet",
  "Split-Finger" = "orange",
  "Sweeper" = "darkgreen"
)

Ohtani.Pitches <- unique(Ohtani.Clean$pitch_name)

Ohtani.Graph <- Ohtani.Clean %>%
  ggplot(aes(x = pfx_x_in, y = pfx_z_in, fill = pitch_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(size = 2, alpha = 1, shape = 21,stroke = 1, color = "black") +
  scale_fill_manual(
    values = Pitch_Colors,
    limits = Ohtani.Pitches
  ) +
  scale_x_continuous(
    limits = c(-25,25),
    breaks = seq(-20,20,5)
  ) +
  scale_y_continuous(
    limits = c(-25,25),
    breaks = seq(-20,20,5)
  ) +
  coord_equal() +
  labs(
    title = "Shohei Ohtani Pitch Movement Plot",
    subtitle = "2024 Season",
    caption = "Baseball Savant Data via baseballr",
    x = "Horizontal Movement (IN)",
    y = "Induced Vertical Movement (IN)",
    fill = "Pitch Name"
  )

Ohtani.Graph
