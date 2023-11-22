install.packages("nflreadr")
install.packages("tidyverse")
install.packages("gt")
install.packages("gtExtras")
install.packages("nflfastR")
library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)

### load in data
pbp <- nflreadr::load_participation(2023, include_pbp = TRUE)

### check column names
names(pbp)

### make players their own row
pbp <- pbp |> 
  separate_rows(offense_players, sep = ";") |> 
  select(old_game_id, play_id, posteam, defteam, week, offense_players, offense_personnel, offense_formation,
         rush, pass, wp, down, ydstogo, yardline_100, score_differential, pass_oe)

### filter to just offensive plays
pbp <- pbp |> 
  filter(rush == 1 | pass == 1)

pbp <- pbp |> 
  mutate(
    `Down & Distance` = case_when(
      down == 1 & ydstogo == 10 ~ '1st & 10',
      down == 1 & ydstogo > 10 ~ '1st and 11+',
      down == 2 & ydstogo == 1 ~ '2nd & 1',
      down == 2 & ydstogo == 2 ~ '2nd & 2',
      down == 2 & ydstogo >= 3 & ydstogo <= 6 ~ '2nd & 3-6',
      down == 2 & ydstogo >= 7 & ydstogo <= 9 ~ '2nd & 7-9',
      down == 2 & ydstogo >= 10 ~ '2nd & 10+',
      down == 3 & ydstogo == 1 ~ '3rd & 1',
      down == 3 & ydstogo == 2 ~ '3rd & 2',
      down == 3 & ydstogo >= 3 & ydstogo <= 6 ~ '3rd & 3-6',
      down == 3 & ydstogo >= 7 & ydstogo <= 9 ~ '3rd & 7-9',
      down == 3 & ydstogo >= 10 ~ '3rd & 10+',
      TRUE ~ '4th Down'
    )
  )

pbp$`Down & Distance` <- factor(pbp$`Down & Distance`, levels = c('1st & 10', '1st & 11+',
                                                                  '2nd & 1', '2nd & 2',
                                                                  '2nd & 3-6', '2nd & 7-9', '2nd & 10+',
                                                                  '3rd & 1', '3rd & 2', '3rd & 3-6',
                                                                  '3rd & 7-9', '3rd & 10+',
                                                                  '4th Down'))

rosters <- load_rosters(2023)

### get names for every player row
pbp <- left_join(pbp, rosters |> select(gsis_id, full_name, position), by = c('offense_players' = 'gsis_id'))

pbp |> 
  filter(wp >= 0.025 & wp <= 0.975, position %in% c('HB', 'WR', 'TE', 'FB')) |> 
  group_by(full_name) |> 
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
  ) |> filter(passrate >= 0.8 | passrate <= 0.2, plays >= 50) |> 
  arrange(-passrate)

pbp |> 
  filter(wp >= 0.025 & wp <= 0.975, position %in% c('HB', 'WR', 'TE', 'FB')) |> 
  group_by(full_name, down) |> 
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
  ) |> filter(passrate >= 0.8 | passrate <= 0.2, plays >= 20, down %in% c(1, 2)) |> 
  arrange(-passrate)

pbp |> 
  filter(posteam == 'BUF', wp >= 0.025 & wp <= 0.975) |> 
  group_by(play_id) |> 
  mutate(
    Knoxin = any(full_name == 'Dawson Knox'),
    Kincaidin = any(full_name == 'Dalton Kincaid')
  ) |> slice(1) |> 
  group_by(Knoxin, Kincaidin) |> 
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
  )


pbp |> 
  filter(posteam == 'BUF', wp >= 0.025 & wp <= 0.975, down %in% c(1, 2),
         yardline_100 >= 14) |> 
  group_by(play_id) |> 
  mutate(
    Knoxin = any(full_name == 'Dawson Knox'),
    Kincaidin = any(full_name == 'Dalton Kincaid')
  ) |> slice(1) |> 
  group_by(Knoxin, Kincaidin) |> 
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
  )

BUF_runpass <- pbp |> 
  filter(posteam == 'BUF', wp >= 0.025 & wp <= 0.975, down %in% c(1, 2), 
         position %in% c('HB', 'WR', 'TE',' FB')) |> 
  group_by(full_name, position, posteam) |> 
  summarise(
    snapsplayed = n(),
    passrate = mean(pass, na.rm = T),
    rushrate = mean(rush, na.rm = T),
    PROE = mean(pass_oe, na.rm = T)/100
  ) |> 
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('posteam' = 'team_abbr'))

BUFtbl <- BUF_runpass |> 
  select(full_name, position, team_wordmark, snapsplayed, passrate, rushrate, PROE) |> 
  ungroup() |> 
  arrange(-snapsplayed) |> 
  gt() |> 
  gt_img_rows(team_wordmark) |> 
  fmt_percent(columns = c(passrate, rushrate, PROE), decimals = 2) |> 
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Arjun Menon | Data: nflreadr") |> 
  cols_label(
    full_name = "Player",
    team_wordmark = "Offense",
    snapsplayed = "Snaps",
    passrate = "Pass Rate",
    rushrate = "Rush Rate",
    PROE = "Pass Rate Over Expected"
  ) |> 
  opt_row_striping() |> 
  tab_header(title = "Bills run/pass rate by player in 2023") |> 
  gt_theme_538()
gtsave(BUFtbl, "BUFrunpasstbl.png")





