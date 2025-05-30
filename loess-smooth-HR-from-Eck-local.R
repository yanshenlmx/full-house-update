
library(fullhouse)
library(tidyverse)

data("batters_adjusted")
names = colnames(batters_adjusted)


foo = batters_adjusted %>% 
  mutate(HRpAB = ifelse(AB == 0, 0, HR/AB)) %>% 
  group_by(playerID) %>% 
  mutate(is_dead_ball = as.numeric(any(year < 1920))) %>% 
  mutate(n_obs = n(), 
         HR_smooth = ifelse(is_dead_ball == 1 & n_obs > 2, predict(loess(HRpAB ~ year, span = 0.5)), HRpAB) * AB) %>% 
  mutate(HR_smooth = ifelse(HR_smooth < 0, 0, HR_smooth)) %>% 
  ungroup()

bar = foo %>% 
  mutate(HR_smooth = ifelse(is.na(HR_smooth), HR, HR_smooth)) %>% 
  mutate(HR_final = sum(HR)/sum(HR_smooth) * HR_smooth)

baz = bar %>% 
  mutate(HR = round(HR_final)) %>% 
  select(-HRpAB, -n_obs, -HR_smooth, -HR_final, -is_dead_ball) %>% 
  select(name, playerID, age, year, PA, AB, H, HR, BB, BA, OBP, HBP, SF, ebWAR, efWAR)

colnames(baz)

write_csv(baz, file = "~/website/era-adjustment-website/src/data/batters_adjusted.csv")
save(baz, file = "~/research/fullhouse/data/batters_adjusted.rda")

