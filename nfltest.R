library(nflfastR)
library(nflreadr)
library(nflplotR)
library(dplyr)
library(ggplot2)

# Load Chiefs @ Rams
kcrams <- load_pbp(2018) |>
  filter(game_id == "2018_11_KC_LA")


kc_skill_positions <- load_rosters(seasons = 2018) |>
  filter(
    team == "KC",
    position %in% c("WR", "TE", "RB")
  ) |>
  select(gsis_id, position)



mahomes_comp <- kcrams |>
  filter(
    passer_player_id == "00-0033873",   # Patrick Mahomes
    complete_pass == 1,
    !is.na(receiver_player_id),
    !is.na(yards_gained)
  ) |>
  left_join(
    kc_skill_positions,
    by = c("receiver_player_id" = "gsis_id")
  ) |>
  mutate(is_td = touchdown == 1)



ggplot(
mahomes_comp,
aes(
  x = reorder(receiver_player_name, yards_gained, max),
  y = yards_gained,
  color = position,
  shape = is_td
)
) +
  geom_jitter(
    width = 0.25,
    alpha = 0.75,
    size = 3
  ) +
  scale_color_manual(
    values = c(
      RB = "red",
      TE = "gold",
      WR = "green"
    ),
    name = "Receiver Position"
  ) +
  scale_shape_manual(
    values = c(
      `FALSE` = 16,  # circle
      `TRUE`  = 17   # triangle
    ),
    labels = c("Non TD", "Touchdown"),
    name = "Play Result"
  ) +
  labs(
    title = "Patrick Mahomes Completions vs Rams (2018)",
    subtitle = "Yards Gained by Receiver\nColor = Position, Shape = Touchdown",
    x = "Receiver",
    y = "Yards Gained"
  ) +
  coord_flip()

