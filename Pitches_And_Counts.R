library(tidyverse)
library(baseballr)
library(gt)
library(gtExtras)
library(markdown)
player.id <- playerid_lookup(first_name = "Shohei", last_name = "Ohtani" )

Data <- scrape_statcast_savant_pitcher(
  start_date = "2026-03-27",
  end_date = Sys.Date() - 1,
  pitcherid = player.id$mlbam_id
)

Data.Clean <- Data %>%
  mutate(
    count = paste0(balls, "-", strikes),
    count = factor(
      count,
      levels = c(
        "0-0", "0-1","0-2",
        "1-0","1-1","1-2",
        "2-0", "2-1","2-2",
        "3-0","3-1","3-2"
      )
    )
  )

table(Data.Clean$pitch_name)

Pitch.Summary <- Data.Clean %>%
  group_by(count,pitch_name) %>%
  summarise(Pitches = n(), .groups = "drop") %>%
  group_by(count) %>%
  mutate(
    Pitch_PCT = Pitches / sum(Pitches)
  ) %>%
  ungroup()


Pitch.Table <- Pitch.Summary %>%
  select(-Pitches) %>%
  pivot_wider(
    names_from = count,
    values_from = Pitch_PCT,
    values_fill = 0
  )

PitchGT <- Pitch.Table %>%
  gt() %>%
  tab_header(
    title = md("**Shohei Ohtani Pitch Percentage per Count**"),
    subtitle = md("*2026 Season, 3-27-26 to 6-10-26*")
  ) %>%
  cols_label(
    pitch_name = "Pitch"
  ) %>%
  fmt_percent(
    columns = -pitch_name,
    decimals = 0
  ) %>%
  data_color(
    columns = -pitch_name,
    fn = scales::col_numeric(
      palette = c("red", "yellow", "green"),
      domain = c(0, 1)
    )
  ) %>%
  tab_options(
    heading.background.color = "dodgerblue"
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_title(groups = c("title", "subtitle"))
  )

gtsave(PitchGT,"Pitch Table by Count.png")

table(Pitch.Summary$pitch_name)