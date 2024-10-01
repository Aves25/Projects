library(tidyverse)
library(baseballr)

Millerref <- playerid_lookup("Miller","Bobby")

Miller <- scrape_statcast_savant_pitcher(start_date = "2024-03-28",end_date = Sys.Date(), pitcherid = Millerref$mlbam_id)

Miller.Clean <- Miller %>%
  mutate(
    pfx_x_in = -12 * pfx_x,
    pfx_z_in = 12 * pfx_z,
    stand = recode(stand, "R" = "Right", "L" = "Left")
  )
Miller.Clean$stand <- as.factor(Miller.Clean$stand)
table(Miller.Clean$pitch_name)

Pitch_Colors <- c(
  "4-Seam Fastball" = "dodgerblue",
  "Changeup" = "pink",
  "Curveball", "red",
  "Sinker" = "lightgreen",
  "Slider" = "violet"
)

Miller.Pitches <- unique(Miller.Clean$pitch_name)

  Miller.Graph <- Miller.Clean %>%
  ggplot(aes(x = pfx_x_in, y = pfx_z_in, fill = pitch_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(size = 2, alpha = 1, shape = 21,stroke = 1, color = "black") +
  scale_fill_manual(
    values = Pitch_Colors,
    limits = Miller.Pitches
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
    title = "Bobby Miller Pitch Movement Plot",
    subtitle = "2024 Season",
    caption = "Baseball Savant Data via baseballr",
    x = "Horizontal Movement (IN)",
    y = "Induced Vertical Movement (IN)",
    fill = "Pitch Name"
  )

  Percentages <- Miller.Clean %>%
    group_by(pitch_name,stand) %>%
    summarize(Count = n()) %>%
    mutate(Total_Pitches = sum(Count),
           Percentage = (Count / Total_Pitches) * 100)
  
 Miller.Pitch_Graph  <- ggplot(Percentages, aes(x = pitch_name, y = Percentage, fill = pitch_name)) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
             position = position_stack(vjust = 1), 
             size = 3, vjust = -0.5) +
   facet_wrap(~stand) +
   labs(x = "Pitch Type", y = "Percentage", fill = "Pitch Type", 
        title = "Pitch Type Percentages by Batter Handedness") +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),
     plot.title = element_text(color = "blue", face = "bold", size = 14),
     panel.background = element_rect(fill = "white"), # White background for the plotting area
     plot.background = element_rect(fill = "white")   # White background for the entire plot
   )
  
  ggsave("Bobby Miller 2024 Season Plot.png",Miller.Graph, height = 6, width = 8, dpi = 300)
  ggsave("Bobby Miller Pitch Percentages.png", Miller.Pitch_Graph, height = 6, width = 8, dpi = 300)

 Miller.clean_Summary <- Miller.Clean %>%
   group_by(pitch_name, stand) %>%
   summarise(
     BA = sum(events %in% c("single", "double", "triple", "home_run")) / sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout")),
     SLG = sum((events == "single") * 1 + (events == "double") * 2 + (events == "triple") * 3 + (events == "home_run") * 4) / sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout")),
     Whiff_Rate = 100 * sum(description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked")) / sum(description %in% c("foul_bunt", "foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked")),
     Chase_Rate = 100 * sum(description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked") & (plate_x < -0.83 | plate_x > 0.83 | plate_z < 1.5 | plate_z > 3.5)) / sum(plate_x < -0.83 | plate_x > 0.83 | plate_z < 1.5 | plate_z > 3.5),
     Strikeouts = sum(events == "strikeout")
   )

 Miller.clean_Summary$BA <- round(Miller.clean_Summary$BA,digits = 3)  
Miller.clean_Summary$SLG <- round(Miller.clean_Summary$SLG, digits = 3) 
Miller.clean_Summary$Whiff_Rate <- round(Miller.clean_Summary$Whiff_Rate, digits = 1)
Miller.clean_Summary$Chase_Rate <- round(Miller.clean_Summary$Chase_Rate, digits = 1)

Miller.Overall.Summary <- Miller.Clean %>%
  group_by(pitch_name) %>%
  summarise(
    BA = round(sum(events %in% c("single", "double", "triple", "home_run")) / sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout")), 3),
    SLG = round(sum((events == "single") * 1 + (events == "double") * 2 + (events == "triple") * 3 + (events == "home_run") * 4) / sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout")), 3),
    Whiff_Rate = 100 * sum(description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked")) / sum(description %in% c("foul_bunt", "foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked")),
    Chase_Rate = 100 * sum(description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked") & (plate_x < -0.83 | plate_x > 0.83 | plate_z < 1.5 | plate_z > 3.5)) / sum(plate_x < -0.83 | plate_x > 0.83 | plate_z < 1.5 | plate_z > 3.5),
    Strikeouts = sum(events == "strikeout")
  )
Miller.Overall.Summary$Whiff_Rate <- round(Miller.Overall.Summary$Whiff_Rate,digits = 2)
Miller.Overall.Summary$Chase_Rate <- round(Miller.Overall.Summary$Chase_Rate,digits = 2)
Miller.Overall.Summary

library(gt)
library(gtExtras)

Miller.Overall.Summary %>%
  gt() %>%
  fmt_number(
    columns = c(BA, SLG, Whiff_Rate, Chase_Rate), 
    decimals = 2
  ) %>%
  cols_label(
    pitch_name = "Pitch Name",
    BA = "Batting Average",
    SLG = "Slugging Percentage",
    Whiff_Rate = "Whiff Rate (%)",
    Chase_Rate = "Chase Rate (%)",
    Strikeouts = "Strikeouts"
  ) %>%
  tab_header(
    title = "Pitch Summary for Bobby Miller"
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "all",color = "dodgerblue", weight = px(2)),
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

Fastball <- Miller.Clean %>%
  filter(pitch_name == "4-Seam Fastball") %>%
  filter(!is.na(plate_x) & !is.na(plate_z))

 Miller.HeatMaps<- Miller.Clean %>%
ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d(aes(fill = ..density..), geom = "raster",contour = F) +
  scale_fill_gradientn(colours = c("blue","red","white")) +
  annotate("rect", xmin = -1, xmax = 1, 
           ymin = 1.6, ymax = 3.4, fill = NA,
           color = "black", alpha = .1) +
  ylim(1,4) + xlim(-1.8,1.8) + theme_bw() + theme_classic() +
  guides(fill = F) +
  facet_wrap(~pitch_name) +
  labs(
    x = "Horizontal Pitch Location",
    y = "Vertical Pitch Location",
    title = "Bobby Miller Pitch Location Heat Maps",
    subtitle = "2024 Season, From Catchers POV",
    caption = "Data from Baseball Savant via baseballr"
  ) +
  theme(plot.title = element_text(hjust = .5, color = "dodgerblue"),
        plot.subtitle = element_text(hjust = .5,face = "bold",color = "dodgerblue"),
        axis.title = element_text(face = "bold",color = "dodgerblue"))

ggsave("Bobby Miller Heat Maps Based on Pitches.png",Miller.HeatMaps,height = 6,width = 8,dpi = 300)
