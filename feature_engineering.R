
## Packages

library(arrow)
library(tidyverse)
library(sportyR)
library(gganimate)
library(gifski)

source("SMT_Data_Starter.R") ## where animate_play function is located
options(scipen = 10)

## Data Collection

teams = c("ANI", "ARN", "PHD", "VAS")

all_ball_events = ball_events %>% 
  filter(home_team %in% teams) %>% 
  collect()

all_game_info = game_info %>% 
  filter(home_team %in% teams) %>% 
  collect()

all_ball_positions = ball_positions %>% 
  filter(home_team %in% teams) %>% 
  collect()

all_player_positions = player_positions %>% 
  filter(home_team %in% teams) %>% 
  collect()


## Coordinates of Bases

x_home = 0
y_home = 0

x_1b = (45 * sqrt(2))
y_1b = x_1b

x_2b = 0
y_2b = (90 * sqrt(2))

x_3b = -x_1b
y_3b = y_1b


## Collecting the End Phase of Play

end_phases = all_ball_events %>% 
  group_by(game_string, play_per_game) %>% 
  filter(lead(player_id) == 0 & lead(ball_eventcode) == 5) %>% 
  mutate(game_string_ppg = paste(game_string,
                                 play_per_game,
                                 sep = "_")) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp) ## timestamp indicates when the last part of the play is


## Filtering Through Balls in Play

ball_in_play = all_ball_events %>% 
  mutate(game_string_ppg = paste(game_string,
                                 play_per_game,
                                 sep = "_")) %>% 
  group_by(game_string_ppg) %>% 
  filter((player_id == 10 & ball_eventcode == 4) & (lead(ball_eventcode) != 2 & lead(ball_eventcode) != 5)) %>% 
  ## next row in play can't be a catch/catcher acquires ball then the play ends
  ungroup() %>% 
  select(game_string_ppg, timestamp, player_id, ball_eventcode)

## Collecting the end phase of play/plays where the ball was put in play and the last acquisition was made either at 2B, 3B, or home

all_ball_positions = all_ball_positions[-1, ] ## First row is problematic

#### Conversion of numeric data to numeric types

all_ball_pos = all_ball_positions %>% 
  mutate(game_string_ppg = paste(game_string, play_per_game, sep = "_"),
         timestamp = as.numeric(timestamp),
         ball_position_x = as.numeric(ball_position_x),
         ball_position_y = as.numeric(ball_position_y),
         ball_position_z = as.numeric(ball_position_z)) %>%
  filter(!is.na(timestamp)) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, ball_position_x, ball_position_y, ball_position_z)
  

## Adding ball distances from each base, and classifying the closest base

all_ball_end = all_ball_pos %>% 
  group_by(game_string_ppg) %>% 
  mutate(dist_3b = sqrt(((ball_position_x - x_3b)^2) + ((ball_position_y - y_3b)^2)), ## ball distance from 3rd
         dist_2b = sqrt(((ball_position_x - x_2b)^2) + ((ball_position_y - y_2b)^2)), ## ball distance from 2nd
         dist_home = sqrt(((ball_position_x - x_home)^2) + ((ball_position_y - y_home)^2)), ## ball distance from home
         min_dist = ifelse(pmin(dist_3b, dist_2b, dist_home) > 5, NA, pmin(dist_3b, dist_2b, dist_home)), ## need to classify closest base
         final_base = ifelse(min_dist == dist_3b, "third", 
                             ifelse(min_dist == dist_2b, "second",
                                    ifelse(min_dist == dist_home, "home", NA)))) %>% 
  ## NA = ball ends at either 1B, cutoff man away from a base, or too far away from base to make a tag
  rename("final_x" = ball_position_x,
         "final_y" = ball_position_y,
         "final_z" = ball_position_z) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, final_x, final_y, final_z, dist_3b, dist_2b, dist_home, final_base) %>% 
  inner_join(end_phases, by = c("game_string_ppg", "timestamp"))
  

## Collecting the end phase of play on balls in play

end_in_play = all_ball_end %>% 
  group_by(game_string_ppg) %>% 
  filter(game_string_ppg %in% ball_in_play$game_string_ppg) %>% 
  filter(!is.na(final_base)) %>% 
  ungroup()


## Filtering Position Data/Converting variables to numeric variables

game_positions = all_player_positions %>% 
  group_by(game_string, play_per_game) %>% 
  mutate(game_string_ppg = paste(game_string, play_per_game, sep = "_")) %>% 
  filter(game_string_ppg %in% end_in_play$game_string_ppg) %>% ## only want position data on balls in play
  filter(player_id %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) %>% ## only want position players/runners
  mutate(play_per_game = as.numeric(play_per_game),
         timestamp = as.numeric(timestamp),
         player_id = as.numeric(player_id),
         field_x = as.numeric(field_x),
         field_y = as.numeric(field_y)
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, player_id, field_x, field_y)



## Filtering Ball-in-Play Data to include the phases of the play

relay_events = all_ball_events %>% 
  mutate(game_string_ppg = paste(game_string, play_per_game, sep = "_")) %>% 
  filter(game_string_ppg %in% ball_in_play$game_string_ppg) %>% ## only including plays where there is a ball in play
  filter((player_id != 1 & ball_eventcode != 1) & (player_id != 0 & ball_eventcode != 5)) %>% 
  select(game_string_ppg, timestamp, player_id, ball_eventcode)
  

relay_ball_positions = all_ball_positions %>% 
  mutate(game_string_ppg = paste(game_string, play_per_game, sep = "_")) %>% 
  filter(game_string_ppg %in% all_ball_end$game_string_ppg) %>% 
  mutate(timestamp = as.numeric(timestamp),
         ball_position_x = as.numeric(ball_position_x),
         ball_position_y = as.numeric(ball_position_y),
         ball_position_z = as.numeric(ball_position_z)) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, ball_position_x, ball_position_y, ball_position_z)


relays = inner_join(relay_events, relay_ball_positions, by = c("game_string_ppg", "timestamp"))

##### relays - master dataset

## Merging the locations of the ball at the end of the play

end_ball_locations = all_ball_end %>% 
  select(game_string_ppg, final_x, final_y, final_z, final_base)

relays = inner_join(relays, end_ball_locations, by = "game_string_ppg", relationship = "many-to-many")

relays = relays %>% 
  filter(!is.na(final_base))
  
## Calculating distance between the ball's location and its end location

relays = relays %>% 
  group_by(game_string_ppg) %>% 
  mutate(dist_from_end = sqrt(((ball_position_x - final_x)^2) + ((ball_position_y - final_y)^2) + ((ball_position_z - final_z)^2))) %>% 
  ungroup()

## Adding a Cutoff Feature/Player Postion Data

#### Creating a data set that merges the master data set with our filtered position data

ball_acquisitions = inner_join(relays, game_positions, by = c("game_string_ppg", "timestamp", "player_id")) 

#### Eliminating ground ball outs

hits = ball_acquisitions %>% 
  group_by(game_string_ppg) %>% 
  filter((player_id == 10 & ball_eventcode == 4) & lead(player_id %in% c(7, 8, 9))) %>% 
  ## next ball acquisition is an outfielder, which eliminates ground ball force plays at 2nd, 3rd, or home
  ungroup()

## Adding cutoff variable

ball_acquisitions = ball_acquisitions %>% 
  group_by(game_string_ppg) %>% 
  filter(game_string_ppg %in% hits$game_string_ppg) %>% 
  mutate(is_cutoff = ifelse(
    game_string_ppg == lead(game_string_ppg) & (ball_eventcode == 2 & lead(player_id) %in% c(3, 4, 5, 6)), "yes", "no"
  )) %>% ## next ball acquisition in the play sequence is made by an infielder
  ungroup() %>% 
  select(game_string_ppg, timestamp, player_id, ball_eventcode, field_x, field_y, is_cutoff) %>% 
  rename(
    "player_x" = field_x,
    "player_y" = field_y
  )

yes_cutoff = ball_acquisitions %>% 
  filter(is_cutoff == "yes") %>% 
  select(game_string_ppg, is_cutoff)

no_cutoff = ball_acquisitions %>% 
  filter(is_cutoff == "no") %>% 
  select(game_string_ppg, is_cutoff)

cutoff_results = rbind(yes_cutoff, no_cutoff) ## appending yes/no cutoff data for the sake of model logic


relays = inner_join(relays, cutoff_results, by = "game_string_ppg", relationship = "many-to-many") ## merging master data set with cutoff results

## Runner Movement

#### Filtering positions data to only include runners

runners = game_positions %>% 
  group_by(game_string_ppg) %>% 
  filter(player_id %in% c(10, 11, 12)) %>% 
  ungroup()

#### Adding runner speed for hitter, 1b runner (if present), and 2b runner (if present), distance from 2nd, 3rd, or home
hitter_speed = runners %>% 
  arrange(game_string_ppg, timestamp) %>% 
  group_by(game_string_ppg) %>% 
  filter(player_id == 10) %>% 
  mutate(
    distance_covered = sqrt((field_x - lag(field_x, default = first(field_x)))^2 + (field_y - lag(field_y, default = first(field_y)))^2),
    time_diff = timestamp - lag(timestamp, default = first(timestamp)),
    velo_ft_ms = distance_covered/time_diff,
    velo_mph = velo_ft_ms / 5280 * (1000*60*60),
    dist_from_2b = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
    dist_from_3b = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
    dist_from_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2),
    min_dist = pmin(dist_from_2b, dist_from_3b, dist_from_home)
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, field_x, field_y, velo_mph, dist_from_2b, dist_from_3b, dist_from_home, min_dist) %>% 
  rename(
    "hitter_x" = field_x,
    "hitter_y" = field_y,
    "hitter_moving_speed" = velo_mph,
    "hitter_dist_2b" = dist_from_2b,
    "hitter_dist_3b" = dist_from_3b,
    "hitter_dist_home" = dist_from_home,
    "hitter_dist_from_final" = min_dist
  )

  
runner_1b_speed = runners %>% 
  arrange(game_string_ppg, timestamp) %>% 
  group_by(game_string_ppg) %>% 
  filter(player_id == 11) %>% 
  mutate(
    distance_covered = sqrt((field_x - lag(field_x, default = first(field_x)))^2 + (field_y - lag(field_y, default = first(field_y)))^2),
    time_diff = timestamp - lag(timestamp, default = first(timestamp)),
    velo_ft_ms = distance_covered/time_diff,
    velo_mph = velo_ft_ms / 5280 * (1000*60*60),
    dist_from_2b = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
    dist_from_3b = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
    dist_from_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2),
    min_dist = pmin(dist_from_2b, dist_from_3b, dist_from_home)
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, field_x, field_y, velo_mph, dist_from_2b, dist_from_3b, dist_from_home, min_dist) %>% 
  rename(
    "runner_1b_x" = field_x,
    "runner_1b_y" = field_y,
    "runner_1b_moving_speed" = velo_mph,
    "runner_1b_dist_2b" = dist_from_2b,
    "runner_1b_dist_3b" = dist_from_3b,
    "runner_1b_dist_home" = dist_from_home,
    "runner_1b_dist_from_final" = min_dist
  )

runner_2b_speed = runners %>% 
  arrange(game_string_ppg, timestamp) %>% 
  group_by(game_string_ppg) %>% 
  filter(player_id == 12) %>% 
  mutate(
    distance_covered = sqrt((field_x - lag(field_x, default = first(field_x)))^2 + (field_y - lag(field_y, default = first(field_y)))^2),
    time_diff = timestamp - lag(timestamp, default = first(timestamp)),
    velo_ft_ms = distance_covered/time_diff,
    velo_mph = velo_ft_ms / 5280 * (1000*60*60),
    dist_from_2b = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
    dist_from_3b = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
    dist_from_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2),
    min_dist = pmin(dist_from_2b, dist_from_3b, dist_from_home)
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, field_x, field_y, velo_mph, dist_from_2b, dist_from_3b, dist_from_home, min_dist) %>% 
  rename(
    "runner_2b_x" = field_x,
    "runner_2b_y" = field_y,
    "runner_2b_moving_speed" = velo_mph,
    "runner_2b_dist_2b" = dist_from_2b,
    "runner_2b_dist_3b" = dist_from_3b,
    "runner_2b_dist_home" = dist_from_home,
    "runner_2b_dist_from_final" = min_dist
  )


## Joining runner data with relay data

relays = inner_join(relays, hitter_speed, by = c("game_string_ppg", "timestamp"))
relays = left_join(relays, runner_1b_speed, by = c("game_string_ppg", "timestamp"))
relays = left_join(relays, runner_2b_speed, by = c("game_string_ppg", "timestamp"))

relays = relays %>% 
  distinct()


## Grabbing end base for each runner

runner_ends = end_in_play %>% 
  group_by(game_string_ppg) %>% 
  filter(game_string_ppg %in% relays$game_string_ppg) %>% 
  ungroup()

runner_ends = inner_join(runner_ends, hitter_speed, by = c("game_string_ppg", "timestamp"))

runner_ends = left_join(runner_ends, runner_1b_speed, by = c("game_string_ppg", "timestamp"))

runner_ends = left_join(runner_ends, runner_2b_speed, by = c("game_string_ppg", "timestamp"))

#### Classifying the runner's final base

runner_ends = runner_ends %>% 
  group_by(game_string_ppg) %>% 
  mutate(hitter_final_stop = ifelse(hitter_dist_from_final > 60, NA, ## eliminates plays where the runner is at 1st or in between bases 
                                    ifelse(hitter_dist_from_final == hitter_dist_2b, "second",
                                            ifelse(hitter_dist_from_final == hitter_dist_3b, "third",
                                              ifelse(hitter_dist_from_final == hitter_dist_home, "home", NA)))),
         runner_1b_final_stop = ifelse(runner_1b_dist_from_final > 60, NA,
                                       ifelse(runner_1b_dist_from_final == runner_1b_dist_2b, "second", 
                                            ifelse(runner_1b_dist_from_final == runner_1b_dist_3b, "third",
                                              ifelse(runner_1b_dist_from_final == runner_1b_dist_home, "home", NA)))),
         runner_2b_final_stop = ifelse(runner_2b_dist_from_final > 60, NA, 
                                    ifelse(runner_2b_dist_from_final == runner_2b_dist_2b, "second", 
                                       ifelse(runner_2b_dist_from_final == runner_2b_dist_3b, "third", 
                                              ifelse(runner_2b_dist_from_final == runner_2b_dist_home, "home", NA))))
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, hitter_final_stop, runner_1b_final_stop, runner_2b_final_stop)

#### Merging the data set with new runner end base feature

relays = inner_join(relays, runner_ends, by = "game_string_ppg", relationship = "many-to-many")


## Filtering down to plays where there is a play to be made at the final base

valid_relays = relays %>% 
  group_by(game_string_ppg) %>% 
  filter(dist_from_end == 0) %>% ## Where the play ends
  mutate(final_runner = ifelse(hitter_final_stop == final_base, "hitter", 
                               ifelse(runner_1b_final_stop == final_base, "runner_1b",
                                      ifelse(runner_2b_final_stop == final_base, "runner_2b", NA)))) %>% 
  filter(!is.na(final_runner)) %>% 
  ungroup() %>% 
  select(game_string_ppg, final_runner)
  
relays = inner_join(relays, valid_relays, by = "game_string_ppg", relationship = "many-to-many")

#### Only grabbing the runner data from the runner of interest (play at 3rd, final runner is the runner on 1st = only including runner data from 1b runner)

relays = relays %>% 
  group_by(game_string_ppg) %>% 
  mutate(runner_x = ifelse(final_runner == "hitter", hitter_x, 
                           ifelse(final_runner == "runner_1b", runner_1b_x, 
                                  ifelse(final_runner == "runner_2b", runner_2b_x, NA))),
         runner_y = ifelse(final_runner == "hitter", hitter_y, 
                           ifelse(final_runner == "runner_1b", runner_1b_y,
                                  ifelse(final_runner == "runner_2b", runner_2b_y, NA))),
         runner_moving_speed = ifelse(final_runner == "hitter", hitter_moving_speed,
                                      ifelse(final_runner == "runner_1b", runner_1b_moving_speed, 
                                             ifelse(final_runner == "runner_2b", runner_2b_moving_speed, NA))),
         runner_dist_from_end = ifelse(final_runner == "hitter", hitter_dist_from_final,
                                       ifelse(final_runner == "runner_1b", runner_1b_dist_from_final,
                                              ifelse(final_runner == "runner_2b", runner_2b_dist_from_final, NA)))
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, player_id, ball_eventcode, ball_position_x, ball_position_y, 
         ball_position_z, final_base, dist_from_end, is_cutoff, runner_x, runner_y, runner_moving_speed, 
         runner_dist_from_end, final_runner) %>% 
  rename("ball_final_base" = final_base,
         "ball_dist_from_end" = dist_from_end
         )

## Ball velocity, transfer time, retrieval time, outcome features

#### Play Outcome

play_outcome = relays %>% 
  group_by(game_string_ppg) %>% 
  filter(ball_dist_from_end == 0) %>% 
  mutate(outcome = ifelse(ball_dist_from_end < runner_dist_from_end & runner_dist_from_end > 7, "out", "safe")) %>% ## 7 ft - compensates for slide + time to tag
  ungroup() %>% 
  select(game_string_ppg, outcome) %>% 
  distinct()

relays = inner_join(relays, play_outcome, by = "game_string_ppg", relationship = "many-to-many")

#### Time taken to acquire ball, time between acquisition and throw, and velocity on each throw

ball_transitions = relays %>% 
  group_by(game_string_ppg) %>% 
  filter(ball_eventcode == 3 | ball_eventcode == 2 | player_id == 10) %>% 
  mutate(retrieval_time = ifelse(ball_eventcode == 2 & lag(ball_eventcode) == 4, timestamp - lag(timestamp), NA), ## time taken for fielder to acquire the ball
         acquisition_throw_transition = ifelse(ball_eventcode == 3 & lag(ball_eventcode) == 2, timestamp - lag(timestamp), NA), ## time between acquisition and throw
         throw_velo_ms = ifelse(ball_eventcode == 2 & lag(ball_eventcode) == 3, (lag(ball_dist_from_end) - ball_dist_from_end)/(timestamp - lag(timestamp)), NA),
         throw_velo = throw_velo_ms / 5280 * (1000*60*60) ## velocity on each throw
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, timestamp, retrieval_time, acquisition_throw_transition, throw_velo)

relays = inner_join(relays, ball_transitions, by = c("game_string_ppg", "timestamp"), relationship = "many-to-many")

## Runner distance from the end from last throw made in the play sequence + ball distance from final base (cutoff/non-cuts)

relay_runner_dist_cutoff = relays %>% 
  filter(ball_eventcode == 3 & lag(ball_eventcode) == 2) %>% 
  filter(is_cutoff == "yes") %>% 
  filter(player_id %in% c(3, 4, 5, 6)) %>% 
  select(game_string_ppg, ball_dist_from_end, runner_dist_from_end) %>% 
  distinct() %>% 
  rename("final_throw_dist" = ball_dist_from_end,
         "runner_dist_at_final_throw" = runner_dist_from_end
         )
  
relay_runner_dist_no_cutoff = relays %>% 
  filter(ball_eventcode == 3 & lag(ball_eventcode) == 2) %>% 
  filter(is_cutoff == "no") %>% 
  select(game_string_ppg, ball_dist_from_end, runner_dist_from_end) %>% 
  distinct() %>% 
  rename("final_throw_dist" = ball_dist_from_end,
         "runner_dist_at_final_throw" = runner_dist_from_end
         )

relay_runner_ball_dist = rbind(relay_runner_dist_cutoff, relay_runner_dist_no_cutoff)

relays = inner_join(relays, relay_runner_ball_dist, by = "game_string_ppg", relationship = "many-to-many") %>% 
  distinct()


## Summarizing the output so each play has its own outcome/features

model_table = relays %>% 
  group_by(game_string_ppg) %>% 
  mutate(
    relay_dist = max(ball_dist_from_end),
    runner_speed = mean(runner_moving_speed, na.rm = TRUE),
    retrieval_time = mean(retrieval_time, na.rm = TRUE),
    throw_velo = mean(throw_velo, na.rm = TRUE),
    transition_time = mean(acquisition_throw_transition, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  select(game_string_ppg, ball_final_base, relay_dist, is_cutoff, runner_speed, retrieval_time, throw_velo, transition_time, final_throw_dist, 
         runner_dist_at_final_throw, outcome) %>% 
  distinct(game_string_ppg, .keep_all = TRUE)


## head(model_table)



## write_csv(model_table, "relays.csv") ## used for model


