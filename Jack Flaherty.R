library(tidyverse)
library(baseballr)

Flahertyref <- playerid_lookup("Flaherty","Jack")

Flahertyref <- Flahertyref %>%
  filter(given_name == "Jack Rafe")

Flaherty <- scrape_statcast_savant_pitcher(start_date = "2024-10-13",end_date = "2024-10-13",pitcherid = Flahertyref$mlbam_id)

Flaherty.Clean <- Flaherty %>%
  mutate(
    pfx_x_in =-12 * pfx_x,
    pfx_z_in = 12 * pfx_z,
    stand = recode(stand, L = "Left", R = "Right")
    )

Flaherty.Clean$stand <- as.factor(Flaherty.Clean$stand)

table(Flaherty.Clean$pitch_name)

pitch.colors <- c(
  "4-Seam Fastball" = "dodgerblue",
  "Knuckle Curve" = "grey",
  "Sinker" = "red",
  "Slider" = "green"
)

Flaherty.Pitches <- unique(Flaherty.Clean$pitch_name)

 Flaherty.Graph <- Flaherty.Clean %>%
  ggplot(aes(x = pfx_x_in, y = pfx_z_in, fill = pitch_name)) +
  geom_point(shape = 21, stroke = 1,alpha = 1,size = 2, color = "black") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(
    values = pitch.colors,
    limits = Flaherty.Pitches
  ) +
  scale_x_continuous(
    breaks = seq(-20,20,5),
    limits = c(-25,25)
  ) +
  scale_y_continuous(
    breaks = seq(-20,20,5),
    limits = c(-25,25)
  ) +
  coord_equal() +
  labs(
    title = "Jack Flaherty Pitch Movement Chart",
    subtitle = "2024 Season Game 1 NLCS vs Mets",
    caption = "Data from Baseball Savant via baseballr",
    x = "Horizontal Movement (IN)",
    y = "Vertical Movement (IN)",
    fill = "Pitch"
  ) +
  theme(
    plot.title = element_text(hjust = .5, face = "bold", color = "dodgerblue"),
    plot.subtitle = element_text(hjust = .5, face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
 
 swing <- c('foul_bunt','foul','hit_into_play','swinging_strike', 'foul_tip',
            'swinging_strike_blocked','missed_bunt','bunt_foul_tip')
 whiff <- c('swinging_strike', 'foul_tip', 'swinging_strike_blocked')

 Flaherty.Clean$swing <- ifelse(Flaherty$description %in% swing, TRUE, FALSE)
 Flaherty.Clean$whiff <- ifelse(Flaherty.Clean$description %in% whiff,TRUE,FALSE)
 Flaherty.Clean$Chase <- ifelse(Flaherty.Clean$zone > 10 & Flaherty$swing == TRUE, TRUE, FALSE)
Flaherty.Clean$In_zone <- ifelse(Flaherty.Clean$zone < 10, TRUE, FALSE)
Flaherty.Clean$Out_Zone <- ifelse(Flaherty.Clean$zone > 10,T,F)
 
Flaherty.Table <- Flaherty.Clean %>%
   group_by(pitch_name) %>%
   summarize(
     Count = n(),
     MPH = mean(release_speed),
     IVB = mean(pfx_z_in),
     HB = mean(pfx_x_in),
     Spin_rate = mean(release_spin_rate),
     Whiff_Rate = sum(whiff)/ sum(swing) * 100,
     Chase_Rate = sum(Chase) / sum(Out_Zone) * 100,
     BA = round(sum(events %in% c("single", "double", "triple", "home_run")) / sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout")), 3),
     SLG = round(sum((events == "single") * 1 + (events == "double") * 2 + (events == "triple") * 3 + (events == "home_run") * 4) / sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout")), 3),
     Strikeouts = sum(events == "strikeout")
   )
 
 Flaherty.Table$Spin_rate<- round(Flaherty.Table$Spin_rate,digits = 0)
 Flaherty.Table$IVB <- round(Flaherty.Table$IVB,digits = 1)
Flaherty.Table$HB <- round(Flaherty.Table$HB) 
Flaherty.Table$MPH <- round(Flaherty.Table$MPH, digits = 1)
Flaherty.Table$Whiff_Rate <- round(Flaherty.Table$Whiff_Rate, digits = 1)
Flaherty.Table$Chase_Rate <- round(Flaherty.Table$Chase_Rate, digits = 1)
Flaherty.Table$BA[is.nan(Flaherty.Table$BA)] <- 0
Flaherty.Table$SLG[is.nan(Flaherty.Table$SLG)] <- 0

Flaherty.Table <- Flaherty.Table %>%
  arrange(desc(Count))

library(gt)
library(gtExtras)

Flaherty.GT <- Flaherty.Table %>%
  gt() %>%
  fmt_number(
    columns = c(MPH,IVB,HB,Whiff_Rate,Chase_Rate),decimals = 1
  ) %>%
  cols_label(
    pitch_name = "Pitch",
    Count = "Count",
    MPH = "MPH",
    IVB = "IVB",
    HB = "HB",
    Spin_rate = "Spin Rate",
    Whiff_Rate = "Whiff %",
    Chase_Rate = "Chase %",
    BA = "BA",
    SLG = "SLG",
    Strikeouts = "Strikeouts"
  ) %>%
  tab_header(
    title = "Pitch Summary for Jack Flaherty"
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "all", color = "dodgerblue", weight = px(2)),
      cell_text(weight = "bold", color = "black"),
      cell_fill(color = "dodgerblue")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "pitch_name")
  )
Flaherty.GT
  
  
Flaherty.Release <- Flaherty.Clean %>%
  ggplot(aes(x = release_pos_x, y  = release_pos_z, color = pitch_name)) +
  xlim(-4,4) + ylim(2,7) +
  geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
  geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 3, na.rm = T) +
  coord_fixed() +
  theme_bw() +
  labs(
    title = "Jack Flaherty Release Chart",
    subtitle = "2024 Season",
    caption = "Data from Baseball Savant via baseballr",
    x = "Horizontal Release (ft)",
    y = "Vertical Release (ft)",
    color = "Pitch"
  ) +
  theme(
    plot.title = element_text(hjust = .5, face = "bold", color = "dodgerblue"),
    plot.subtitle = element_text(hjust = .5, face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

library(grid)
library(gridExtra)

table_grob <- as_gtable(Flaherty.GT)


Flaherty.Overall <- grid.arrange(table_grob, Flaherty.Graph, Flaherty.Release, 
             nrow = 2, ncol = 2, 
             layout_matrix = rbind(c(1, 1), c(2, 3)),
             heights = c(.4,.6),
             widths = c(.5,.5)) 
ggsave("Flaherty Summary.png",Flaherty.Overall, height = 6, width = 8, dpi = 300)
