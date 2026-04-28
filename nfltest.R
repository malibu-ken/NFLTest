library(nflfastR)
library(nflreadr)
library(nflplotR)
library(dplyr)
library(ggplot2)

#Load 2025 play by play info and join with combine information
rosters_2025 <- load_rosters(2025) %>%
  distinct(gsis_id)

players_bridge <- load_players() %>%
  filter(!is.na(gsis_id), !is.na(pfr_id)) %>%
  distinct(pfr_id, .keep_all = TRUE) %>%
  select(gsis_id, pfr_id, display_name, position)

combine_with_gsis <- load_combine() %>%
  left_join(players_bridge, by = "pfr_id")

combine_2025_players <- combine_with_gsis %>%
  semi_join(rosters_2025,by = "gsis_id")

combine_2025_skill <- combine_2025_players %>%
  filter(position %in% c("WR", "TE", "RB", "FB"))

pbp <- load_pbp(2025) %>%
  filter(play_type == "pass",
         season_type == "REG")

rcvrs <- load_players() %>%
  filter(position %in% c("WR", "TE", "RB", "FB"))

full_pbp <- pbp %>%
  left_join(rcvrs, by = c("receiver_player_id" = "gsis_id")) %>%
  filter(!is.na(receiver_player_id))

player_2025_stats <- full_pbp %>%
  filter(
    season == 2025,
    play_type == "pass",
    !is.na(receiver_player_id)
  ) %>%
  group_by(receiver_player_id) %>%
  summarise(
    nfl_season = 2025,
    targets = n(),                                   # receiver appeared as target
    receptions = sum(complete_pass == 1),            # completed to that receiver
    yards = sum(receiving_yards, na.rm = TRUE),
    .groups = "drop"
  )

combine_clean <- combine_2025_players %>%
  rename(
    combine_season = season)

final_df <- player_2025_stats %>%
  left_join(combine_clean, by = c("receiver_player_id" = "gsis_id")) %>%
  filter(position %in% c("WR", "TE", "RB", "FB"))

#Cleaning up df - converting height to inches
final_df <- final_df %>%
  mutate(height_inches =
      as.numeric(sub("-.*$", "", ht)) * 12 +     # feet → inches
      as.numeric(sub("^.*-", "", ht))             # add remaining inches
  ) %>% select(-ht)

#Modeling data

#First check: 40 yard dash
df_forty <- final_df %>% filter(!is.na(forty))

#This is a work in progress.
