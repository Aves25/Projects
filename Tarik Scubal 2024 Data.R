library(tidyverse)
library(baseballr)

Skubalref <- playerid_lookup("Skubal","Tarik")

Skubal <- scrape_statcast_savant_pitcher(start_date = "2024-03-28", end_date = Sys.Date(), pitcherid = Skubalref$mlbam_id)

Skubal.Clean <- Skubal %>%
  mutate(
     pfx_x_in = -12 * pfx_x,
     pfx_z_in = 12 * pfx_z,
     stand = recode(stand,"L" = "Left", "R" = "Right")
  )

Skubal.Clean <- Skubal.Clean %>%
  filter(pitch_name != "")

Skubal.Clean$stand <- as.factor(Skubal.Clean$stand)

table(Skubal.Clean$pitch_name)
Skubal.Pitches <- unique(Skubal.Clean$pitch_name)
pitch_colors <- c(
  '4-Seam Fastball' = "navy",
  "Changeup" = "maroon",
  "Slider" = "orange",
  "Sinker" = "grey40",
  "Knuckle Curve" = "green"
)

Skubal.Plot <- Skubal.Clean %>%
  ggplot(aes(x = pfx_x_in, y = pfx_z_in, fill = pitch_name)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2, alpha = 1, shape = 21, stroke = 1, color = "black") +
  scale_fill_manual(
    values = pitch_colors,
    limits = Skubal.Pitches
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
    title = "Tarik Skubal Pitch Movement Plot",
    subtitle = "2024 Season",
    caption = "Baseball Savant Data via baseballr",
    x = "Horizontal Movement (IN)",
    y = "Induced Vertical Movement (IN)",
    fill = "Pitch Name"
  )

table(Skubal.Clean$pitch_name)

Skubal.Release <- Skubal.Clean %>%
  ggplot(aes(x = release_pos_x, y  = release_pos_z, color = pitch_name)) +
  xlim(-4,4) + ylim(2,7) +
  geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
  geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 3, na.rm = T) +
  coord_fixed() +
  theme_bw() +
  labs(
    title = "Tarik Skubal Release Chart",
    subtitle = "2024 Season",
    caption = "Data from Baseball Savant via baseballr",
    x = "Horizontal Release (ft)"
  )
ggsave("Tarik Skubal Release Plot.png", Skubal.Release,height = 6, width = 8, dpi = 300)



New.Skubal <- Skubal.Clean %>%
  mutate(
    Outcome = case_match(
      description,
      c("ball", "blocked_ball", "pitchout", 
        "hit_by_pitch") ~ "ball",
      c("swinging_strike", "swinging_strike_blocked",
        "foul", "foul_bunt", "foul_tip", 
        "hit_into_play",  "missed_bunt" ) ~ "swing",
      "called_strike" ~ "called_strike"),
    Home = ifelse(inning_topbot == "Bot", 1, 0),
    Count = paste(balls, strikes, sep = "-")
  )

taken <- New.Skubal|>
  filter(Outcome != "swing")
taken_select <- select(
  taken, pitch_type, release_speed,
  description, stand, p_throws, Outcome,
  plate_x, plate_z, fielder_2_1,
  pitcher, batter, Count, Home, zone 
)


plate_width <- 17 + 2 * (9/pi)
k_zone_plot <- ggplot(
  NULL, aes(x = plate_x, y = plate_z)
) + 
  geom_rect(
    xmin = -(plate_width/2)/12, 
    xmax = (plate_width/2)/12, 
    ymin = 1.5, 
    ymax = 3.6, color = "navy", alpha = 0
  ) + 
  coord_equal() + 
  scale_x_continuous(
    "Horizontal location (ft.)", 
    limits = c(-2, 2)
  ) + 
  scale_y_continuous(
    "Vertical location (ft.)", 
    limits = c(0, 5)
  )

k_zone_plot %+% 
  sample_n(taken, size = 1400) +
  aes(color = Outcome) +
  geom_point(alpha = 0.2) + 
  scale_color_manual(values = crc_fc)
