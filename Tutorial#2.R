library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)
library(ggimage)
library(ggthemes)
library(ggtext)

### load in data
pbp <- nflreadr::load_participation(2023, include_pbp = TRUE)

pbp <- pbp |> filter(pass == 1)

### data exploration
unique(pbp$defense_coverage_type)

pbp <- pbp |> 
  mutate(
    defense_coverage_type = case_when(
      defense_coverage_type == "COVER_0" ~ 'Cover 0',
      defense_coverage_type == "COVER_1" ~ 'Cover 1',
      defense_coverage_type == "COVER_2" ~ 'Cover 2',
      defense_coverage_type == "COVER_3" ~ "Cover 3",
      defense_coverage_type == "COVER_4" ~ 'Cover 4',
      defense_coverage_type == "COVER_6" ~ 'Cover 6',
      defense_coverage_type == "2_MAN" ~ 'Cover 2 Man',
      TRUE ~ 'Other'
    )
  )

colSums(is.na(pbp))

pbp |> filter(qb_scramble == 1) |> select(qb_scramble, defense_coverage_type)
pbp |> filter(sack == 1) |> select(sack, defense_coverage_type)

### getting coverage rates for each team
covrates <- pbp |> 
  group_by(defteam, defense_coverage_type) |> 
  summarise(
    plays = n()
  ) |> 
  ungroup() |> 
  group_by(defteam) |> 
  filter(defense_coverage_type%in% c('Cover 0', 'Cover 1', 'Cover 2', 'Cover 3', 'Cover 4', 'Cover 6', 'Cover 2 Man')) |> 
  mutate(totplays = sum(plays),
         rates = plays/totplays) |> 
  select(-plays, -totplays) |> 
  group_by(defteam) |> 
  pivot_wider(names_from = defense_coverage_type, values_from = rates)

covrates[is.na(covrates)] <- 0

### entropy calculation
covrates <- covrates %>%
  mutate(shannon = 
           -`Cover 0`*log(`Cover 0`, 2) - `Cover 1`*log(`Cover 1`, 2) - `Cover 2`*log(`Cover 2`, 2) - 
           `Cover 3`*log(`Cover 3`, 2) -`Cover 4`*log(`Cover 4`, 2) - `Cover 6`*log(`Cover 6`, 2) - 
           `Cover 2 Man`*log(`Cover 2 Man`, 2)) %>% 
  arrange(-shannon) 

covrates <- covrates |> 
  select(defteam, shannon) |> 
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

def_EPA <- pbp |> 
  group_by(defteam, season) |> 
  summarise(
    EPAp = mean(epa, na.rm = T)
  )

shannon_all <- left_join(covrates, def_EPA, by = c('defteam'))

shannon_all |> 
  ggplot(aes(x = shannon, y = EPAp))+
  geom_image(image = shannon_all$team_logo_espn, asp = 16/9, size = 0.05)+
  geom_hline(yintercept = mean(shannon_all$EPAp), lty = 'dashed', color = 'red')+
  geom_vline(xintercept = mean(shannon_all$shannon), lty = 'dashed', color = 'red')+
  theme_fivethirtyeight()+
  labs(title = "Coverage predictability and efficiency for NFL defenses 2023",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("Predictability only looking at coverage schemes. Defensive fronts, formations, personnel usage not included here"))+
  theme(axis.title = element_text(size = 18)) + xlab('Coverage scheme shannon entropy') + ylab("EPA/dropback allowed")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 17, hjust = 0.5))+
  scale_y_reverse(breaks = scales::pretty_breaks(n=8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))

covratesEPA <- pbp |> 
  group_by(defteam, defense_coverage_type) |> 
  summarise(
    plays = n(),
    EPAplay = mean(epa, na.rm = T),
    successrate = mean(success, na.rm = T),
    YPP = mean(yards_gained, na.rm = T)
  ) |> filter(defense_coverage_type != "Other") 

covratesEPA |> 
  filter(defteam == 'KC') |> 
  arrange(-plays) |> 
  ungroup() |> 
  gt() |> 
  fmt_percent(columns = successrate, decimals = 2) |> 
  fmt_number(columns = c(EPAplay, YPP), decimals = 2) |> 
  opt_row_striping() |> 
  tab_header(title = "Chiefs defensive coverage rates in 2023") |> 
  gt_theme_538()


