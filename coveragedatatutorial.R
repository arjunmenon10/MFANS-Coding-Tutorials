library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)
library(ggimage)
library(ggthemes)
library(ggtext)

pbp <- load_participation(2023, include_pbp = TRUE)

pbp <- pbp |> filter(pass == 1)

unique(pbp$defense_coverage_type)

pbp <- pbp |> 
  mutate(
    defense_coverage_type = case_when(
      defense_coverage_type == "COVER_0" ~ 'Cover 0',
      defense_coverage_type == "COVER_1" ~ 'Cover 1',
      defense_coverage_type == "COVER_2" ~ 'Cover 2',
      defense_coverage_type == "COVER_3" ~ 'Cover 3',
      defense_coverage_type == "COVER_4" ~ 'Cover 4',
      defense_coverage_type == "COVER_6" ~ 'Cover 6',
      defense_coverage_type == "2_MAN" ~ 'Cover 2 Man',
      TRUE ~ 'Other'
    )
  )

pbp |> filter(qb_scramble == 1) |> select(qb_scramble, defense_coverage_type)
pbp |> filter(sack == 1) |> select(sack, defense_coverage_type)

covrates <- pbp |> 
  group_by(defteam, defense_coverage_type) |> 
  summarise(
    plays = n(),
  ) |> 
  group_by(defteam) |> 
  filter(defense_coverage_type != "Other") |> 
  mutate(
    totplays = sum(plays),
    rates = plays/totplays
  ) |> 
  select(-plays, -totplays) |> 
  group_by(defteam) |> 
  pivot_wider(names_from = defense_coverage_type, values_from = rates)

covrates[is.na(covrates)] <- 0

nug = 10^(-3)

covrates <- covrates |> 
  mutate(
    shannon = -`Cover 0`*log(`Cover 0` + nug, 2) - `Cover 1`*log(`Cover 1` + nug, 2) - `Cover 2`*log(`Cover 2` + nug, 2) -
      `Cover 3`*log(`Cover 3` + nug, 2) - `Cover 4`*log(`Cover 4` + nug, 2) - `Cover 6`*log(`Cover 6` + nug, 2) - 
      `Cover 2 Man`*log(`Cover 2 Man` + nug, 2)
  )

defEPA <- pbp |> 
  group_by(defteam) |> 
  summarise(
    EPAp = mean(epa, na.rm = T)
  )

shannon_all <- left_join(covrates, defEPA, by = 'defteam') |> 
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

shannon_all |> 
  ggplot(aes(x = shannon, y = EPAp))+
  geom_image(image = shannon_all$team_logo_espn, asp = 16/9, size = 0.05)+
  geom_hline(yintercept = mean(shannon_all$EPAp), lty = 'dashed', color = 'red')+
  geom_vline(xintercept = mean(shannon_all$shannon), lty = 'dashed', color = 'red')+
  theme_fivethirtyeight()+
  labs(title = "Coverage predictability and defensive pass efficiency allowed for NFL defenses in 2023",
       caption = "By Arjun Menon | @arjunmenon100 | data: nflreadr",
       subtitle = "Predictability only looking at coverage schemes. Defensive fronts, formations, personnel usage not included here")+
  theme(axis.title = element_text(size = 18))+ xlab('Coverage scheme shannon entropy') + ylab("EPA/attempt allowed")+
  theme(panel.grid.minor = element_blank())+
  theme(axis.text = element_text(size = 17),
        plot.title = element_text(siz = 21, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 17, hjust = 0.5))+
  scale_y_reverse(breaks = scales::pretty_breaks(n=8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('coventropy.png', width = 14, height = 10, dpi = "retina")

covratesEPA <- pbp |> 
  group_by(defteam, defense_coverage_type) |> 
  summarise(
    plays = n(),
    EPAplay = mean(epa, na.rm = T),
    successrate = mean(success, na.rm = T),
    YPP = mean(yards_gained, na.rm = T)
  ) |> filter(defense_coverage_type != "Other") |> 
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('defteam' = 'team_abbr'))

KCtbl <- covratesEPA |> 
  ungroup() |> 
  filter(defteam == 'KC') |> 
  select(team_wordmark, defense_coverage_type, plays, EPAplay, successrate, YPP) |> 
  arrange(-plays) |> 
  gt() |> 
  gt_img_rows(columns = team_wordmark) |> 
  fmt_number(columns = c(EPAplay, YPP), decimals = 2) |> 
  fmt_percent(columns = successrate, decimals = 2) |> 
  opt_row_striping() |> 
  gt_theme_538() |> 
  tab_header(title = "Chiefs defensive coverage breakdown in the 2023 season") |> 
  cols_label(
    team_wordmark = "Defense",
    defense_coverage_type = "Coverage",
    EPAplay = "EPA/attempt",
    successrate = "Success Rate"
  )
gtsave(KCtbl, "KCtbl.png")



