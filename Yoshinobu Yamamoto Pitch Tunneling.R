library(tidyverse)
library(baseballr)

# Step 1: Get Player ID and game data
Yamamoto <- playerid_lookup(last_name = "Yamamoto", first_name = "Yoshinobu")

Game <- statcast_search_pitchers(
  start_date = "2026-06-13", end_date = "2026-06-13", pitcherid = Yamamoto$mlbam_id
)

# Step 2: Create Previous Pitch Variables
Game <- Game %>%
  arrange(at_bat_number, pitch_number) %>%
  mutate(
    prev_pitch = lag(pitch_type),
    prev_velo = lag(release_speed),
    prev_x = lag(pfx_x),
    prev_z = lag(pfx_z)
  )

# Step 3: Sequence Matrix
Sequences <- Game %>%
  filter(!is.na(prev_pitch)) %>%
  count(prev_pitch, pitch_type)

  ggplot(Sequences, aes(prev_pitch,pitch_type, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Yoshinobu Yamamoto Sequencing Plot",
    x = "Previous Pitch",
    y = "Current Pitch"
  )
  
  # Step 4: Tunnel Distance
  Game <- Game %>%
    mutate(
      tunnel_distance =
        sqrt(
          (pfx_x - prev_x) ^2 +
            (pfx_z - prev_z) ^2
        )
    )
  
  Game %>%
    group_by(prev_pitch, pitch_type) %>%
    summarize(
      avg_tunnel = mean(tunnel_distance, na.rm = TRUE)
    )
  
  # Step 5: Whiffs by Sequence
  whiffs <- c(
    "swinging_strike",
    "swinging_strike_blocked"
  )
  
  Whiffs <- Game %>%
    filter(!is.na(prev_pitch)) %>%
    group_by(prev_pitch, pitch_type) %>%
    summarize(
      pitches = n(),
      whiffs = sum(description %in% whiffs),
      whiff_rate = whiffs / pitches
    ) %>%
    arrange(desc(whiff_rate))
  
  # Step 6: Strikeout Sequence Trees
  ks <- Game %>%
    filter(events == "strikeout")
  
 # Step 7: Tunnel Visualization
  
  ggplot(Game,
         aes(release_pos_x,
             release_pos_z,
             color = pitch_type)) +
    geom_point(size = 3)
  
  
 # Step 8: Advanced Version
  
  Game <- Game %>%
    mutate(
      tunnel_score =
        abs(release_speed - prev_velo) *
        tunnel_distance
    )
  
  sequence_whiffs <- Game %>%
    filter(!is.na(prev_pitch)) %>%
    group_by(prev_pitch, pitch_type) %>%
    summarize(
      pitches = n(),
      whiffs = sum(description %in% whiffs),
      whiff_rate = whiffs / pitches,
      .groups = "drop"
    ) %>%
    filter(pitches >= 5) %>%
    arrange(desc(whiff_rate))
  
  
  
  
  
  Game %>%
    filter(description %in% whiffs) %>%
    count(
      balls,
      strikes,
      prev_pitch,
      pitch_type
    )
  