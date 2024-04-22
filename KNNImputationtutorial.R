### load in libraries
library(tidyverse)
library(tidyselect)
library(nflreadr)
library(DMwR)
library(mltools)

### load in combine data
combine <- load_combine()

unique(combine$pos)

### change position groupings into buckets
combine <- combine |> 
  mutate(
    pos = case_when(
      pos == 'OG' ~ 'G',
      pos == 'OT' ~ 'T',
      pos %in% c('OLB', 'DE') ~ 'EDGE',
      pos == 'SAF' ~ 'S',
      TRUE ~ pos
    )
  )

str(combine)

### check number of NA values
colSums(is.na(combine))

### change height from character to numeric
combine <- combine |> 
  mutate(
    ht = case_when(
      ht == '5-4' ~ 64,
      ht == '5-5' ~ 65,
      ht == '5-6' ~ 66,
      ht == '5-7' ~ 67,
      ht == '5-8' ~ 68,
      ht == '5-9' ~ 69,
      ht == '5-10' ~ 70,
      ht == '5-11' ~ 71,
      ht == '6-0' ~ 72,
      ht == '6-1' ~ 73,
      ht == '6-2' ~ 74,
      ht == '6-3' ~ 75,
      ht == '6-4' ~ 76,
      ht == '6-5' ~ 77,
      ht == '6-6' ~ 78,
      ht == '6-7' ~ 79,
      ht == '6-8' ~ 80,
      ht == '6-9' ~ 81,
      ht == '6-10' ~ 82
    )
  )

### make position a factor
combine$pos <- as.factor(combine$pos)

combine |> filter(player_name == 'Quentin Johnston') |> as.data.frame()

### transpose position variable into individual columns
combine_all <- one_hot(as.data.table(combine))

combine_all <- combine_all |> 
  select(starts_with("pos"), ht:shuttle)

### KNN imputation function
measurables <- knnImputation(combine_all, k = 10)

colSums(is.na(measurables))

combine_names <- combine |> 
  select(season:player_name)

### combine imputed values with original names
combine_test <- cbind(combine_names, measurables)

colSums(is.na(combine_test))

### test on if it worked
combine |> filter(player_name == 'Quentin Johnston') |> as.data.frame()
combine_test|> filter(player_name == 'Quentin Johnston') |> as.data.frame()

combine_40 <- combine_all |> 
  select(ht, wt, forty, starts_with("pos"))

measurables40 <- knnImputation(combine_40, k = 10)




