rm(list=ls())
## data preparation
## scrape data from baseball reference and fangraphs
library(reticulate)
reticulate::py_config()
## copy the python directory and paste into here
use_python("/Users/yanshen/Library/r-miniconda/envs/r-reticulate/bin/python", required = TRUE)

py_install("bs4")
py_install("requests")
py_install("selenium")
py_install("Send2Trash")
py_install("pandas")

## fix local path to scraping.py on your computer
source_python('scraping.py')

## new season
new_season <- 2024L

## you may run the following codes two or three times in case 
## the scrapping process is interrupted by the ads on the web. 
bbref_bat <- raw_bbref_bat(new_season)
bbref_pit <- raw_bbref_pit(new_season)

## load previous datasets
load('previous_data.RData')

## batters
## clean the dataset from baseball-reference
bbref_bat <- bbref_bat %>% mutate(name = gsub('[^[:alnum:] ]',' ',Name)) %>%
  mutate(name = ifelse(str_sub(name, -1) == ' ', gsub('.{1}$', '',name), name))
bbref_bat <- cbind(bbref_bat[-nrow(bbref_bat),], yearID = new_season)
bbref_bat <- bbref_bat %>% filter(Lg %in% c('NL', 'AL')) %>% 
  filter(Tm != "TOT")

batting_stats <- rbind(previous_bat_bbref, bbref_bat)

## park factor adjustment for Runs, Hits and Home Runs
## park factor adjustment from 1901 - 2022

## load required functions
source('functions.R')
## detect the number of CPU cores
ncores <- detectCores() - 1

years <- c(1901:new_season)
team_log <- do.call(rbind, lapply(years, function(x) 
  cbind(read_csv(paste("https://raw.githubusercontent.com/chadwickbureau/retrosplits/master/daybyday/teams-", x, ".csv", sep = "")), yearID = x)))

team_log <- team_log %>% select(c(team.alignment, team.key, opponent.key, 
                                  B_R, B_H, B_HR, P_R, P_H, P_HR, yearID))
park_factor <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  park_factor_year(xx)
}))

## add baseball reference teamID to chadwick teamID
library(Lahman)
teams_chadwick_1901 <- Teams %>% filter(yearID >= 1901) %>% 
  filter(lgID %in% c('NL', 'AL')) %>% 
  select(yearID, teamID, teamIDBR)

## teamID in 2022 and 2023 has not been updated on Lahman
## manually add 2022 and 2023teamID
teamID2021 <- teams_chadwick_1901 %>% filter(yearID == 2021)
teamID2022 <- teamID2021 %>% mutate(yearID = 2022)
teamID2023 <- teamID2021 %>% mutate(yearID = 2023)
teams_chadwick_1901 <- rbind(teams_chadwick_1901, rbind(teamID2022, teamID2023))

m <- park_factor
m$teamID <- as.character(m$teamID)
m$teamID[m$yearID >= 2005] <- gsub('ANA', 'LAA', m$teamID[m$yearID >= 2005])
m$teamID[m$yearID >= 1953 & m$yearID <= 1965] <- gsub('MLN', 'ML1', m$teamID[m$yearID >= 1953 & m$yearID <= 1965])
m$teamID[m$yearID >= 1970 & m$yearID <= 1997] <- gsub('MIL', 'ML4', m$teamID[m$yearID >= 1970 & m$yearID <= 1997])
park_factor <- m


## merge park factor from Chadwick with teamID from baseball reference
park_factor_1901 <- merge(park_factor, teams_chadwick_1901, 
                          by = c('yearID', 'teamID'))

## moving window for park factor. 

park_factor_averaged <- do.call(rbind, mclapply(unique(park_factor_1901$teamID), mc.cores = ncores, FUN = function(xx){
  if (xx == "LAA") {
    m <- park_factor_1901 %>% filter(teamID == xx)
    m$park_factor_R[1:4] <- moving_window_mean(m$park_factor_R[1:4])
    m$park_factor_H[1:4] <- moving_window_mean(m$park_factor_H[1:4])
    m$park_factor_HR[1:4] <- moving_window_mean(m$park_factor_HR[1:4])
    m$park_factor_R[5:21] <- moving_window_mean(m$park_factor_R[5:21])
    m$park_factor_H[5:21] <- moving_window_mean(m$park_factor_H[5:21])
    m$park_factor_HR[5:21] <- moving_window_mean(m$park_factor_HR[5:21])
    m
  } else{
    m <- park_factor_1901 %>% filter(teamID == xx)
    m$park_factor_R <- moving_window_mean(m$park_factor_R)
    m$park_factor_H <- moving_window_mean(m$park_factor_H)
    m$park_factor_HR <- moving_window_mean(m$park_factor_HR)
  }
  m
}))

batters <- batting_stats[,-c(1,2,30)]
colnames(batters)[c(2,28)] <- c('teamID', 'playerID')

park_factor_br <- park_factor_averaged[,-2]
colnames(park_factor_br)[5] <- 'teamID'

batters_park_factor <- merge(batters %>% filter(yearID >= 1901), park_factor_br, 
                             by = c("teamID", "yearID"))

## No data for the teams before 1901
## No park factor for them

pre01_batter <- batters %>% filter(yearID <= 1900) 
batters_adj <- rbind(cbind(pre01_batter, park_factor_R = 1, park_factor_H = 1, 
                           park_factor_HR = 1), batters_park_factor)

batters_adj <- batters_adj %>% 
  mutate(H = as.numeric(H), 
         HR = as.numeric(HR), 
         R = as.numeric(R)) %>%
  mutate(H_PK =  H / park_factor_H) %>% 
  mutate(HR_PK =  HR / park_factor_HR) %>% 
  mutate(R_PK =  R / park_factor_R)

## merge with population
## years to consider
years <- 1871:new_season

MLB_pop <- read.csv('datMLBeligible_0.5_favorite_sport.csv') 
MLB_pop <- rbind(MLB_pop, data.frame(year = 2021:new_season, 
                                     pop = (2:(new_season-2019))*14866270 - (1:(new_season-2020))*15041126, 
                                     ALpop = (2:(new_season-2019))*14866270 - (1:(new_season-2020))*15041126))

## add weighted populations to data
pops_data <- rbind(
  data.frame(pops = MLB_pop$pop, lgID = "NL", yearID = 1870 + 1:(15*10 + (new_season - 2020))),
  data.frame(pops = MLB_pop$ALpop, lgID = "AL", yearID = 1870 + 1:(15*10 + (new_season - 2020))))

colnames(batters_adj)[3] <- "lgID"
pop_batters <- merge(batters_adj, pops_data, by = c("yearID", "lgID"))

pop_batters$BA[is.na(pop_batters$BA)] <- 0
pop_batters$OBP[is.na(pop_batters$OBP)] <- 0
pop_batters$SH[is.na(pop_batters$SH)] <- 0
pop_batters$SF[is.na(pop_batters$SF)] <- 0
pop_batters$HBP[is.na(pop_batters$HBP)] <- 0
pop_batters$G <- as.numeric(pop_batters$G)
pop_batters$PA <- as.numeric(pop_batters$PA)
pop_batters$AB <- as.numeric(pop_batters$AB)
pop_batters$BB <- as.numeric(pop_batters$BB)
pop_batters$G <- as.numeric(pop_batters$G)
pop_batters$H <- as.numeric(pop_batters$H)
pop_batters$HR <- as.numeric(pop_batters$HR)
pop_batters$HBP <- as.numeric(pop_batters$HBP)
pop_batters$SH <- as.numeric(pop_batters$SH)
pop_batters$SF <- as.numeric(pop_batters$SF)
pop_batters$H_PK <- as.numeric(pop_batters$H_PK)
pop_batters$HR_PK <- as.numeric(pop_batters$HR_PK)

batters_stats <- pop_batters %>% group_by(playerID, yearID) %>% 
  summarise(name = unique(name), 
            age = unique(Age),
            G = sum(G), 
            lgID = lgID[which.max(PA)], 
            PA = sum(PA), 
            AB = sum(AB), 
            BB = sum(BB), 
            obs_hits = sum(H), 
            obs_HR = sum(HR), 
            HBP = sum(HBP),
            SH = sum(SH), 
            SF = sum(SF), 
            H = sum(H_PK), 
            HR = sum(HR_PK), 
            pops = pops[which.max(PA)])

## batting bwar from baseball reference
batting_bwar = readr::read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt", na = "NULL")
batting_bwar <- batting_bwar[!is.na(batting_bwar$WAR),]
batters_bWAR <- batting_bwar %>% filter(lg_ID %in% c('NL', 'AL', 'NA')) %>% 
  select(player_ID, year_ID, WAR) %>% group_by(player_ID, year_ID) %>% 
  summarise(bWAR = sum(WAR))
colnames(batters_bWAR)[c(1,2)] <- c('playerID', 'yearID')

## all batting stats except fWAR are merged together
batters_stats_bWAR <- merge(batters_stats, batters_bWAR, 
                            by = c('playerID', 'yearID'))


## datasets from fangraphs need membership to download. 
## I am working on the python code to deal with this issue. 
## Use the following link to download dataset
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=nl&qual=0&type=c%2c7%2c13%2c59&season=2024&month=0&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=al&qual=0&type=c%2C7%2C13%2C59&season=2024&month=0&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=al&qual=0&type=8&season=2024&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&month=0
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=nl&qual=0&type=8&season=2024&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&month=0
## then click the 'Export Data'. 

batters_fWAR_2024_AL <- read_csv("batters_fWAR_2024_AL.csv")
batters_fWAR_2024_NL <- read_csv("batters_fWAR_2024_NL.csv")
batters_fWAR_2024 <- rbind(cbind(batters_fWAR_2024_AL, yearID = new_season, lgID = 'AL'), 
                           cbind(batters_fWAR_2024_NL, yearID = new_season, lgID = 'NL'))
batters_fWAR_group <- batters_fWAR_2024 %>% group_by(PlayerId, yearID) %>% summarise(
  name = unique(Name), fWAR = sum(WAR)
)
colnames(batters_fWAR_group)[1] <- c('playerid')
batters_fWAR <- rbind(previous_bat_fangraphs, batters_fWAR_group)

## merge fangraph ID with baseball reference ID
people <- do.call(rbind, lapply(c(0:9, 'a', 'b', 'c', 'd', 'e', 'f'), function(x) {
  m <- read_csv(paste("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-", x, ".csv", sep = ""))
  m
})) %>% select(key_bbref, key_fangraphs)

bbref_fangraphs_ID <- people[!is.na(people$key_fangraphs),]
colnames(bbref_fangraphs_ID)[2] <- 'playerid'

batters_fWAR_bWAR <- merge(batters_fWAR, bbref_fangraphs_ID, by = 'playerid')

## combine fWAR with batting stats and bWAR
colnames(batters_fWAR_bWAR)[5] <- 'playerID'
batters <- merge(batters_stats_bWAR, 
                 batters_fWAR_bWAR %>% select(playerID, yearID, fWAR, playerid), 
                 by = c('playerID', 'yearID'))

colnames(batters)[20] <- 'key_fangraphs'
colnames(batters)[1] <- 'key_bbref'
batters <- batters %>% mutate(playerID = key_bbref)

## pitchers
## clean the dataset from baseball-reference
bbref_pit  <- bbref_pit  %>% mutate(name = gsub('[^[:alnum:] ]',' ',Name)) %>%
  mutate(name = ifelse(str_sub(name, -1) == ' ', gsub('.{1}$', '',name), name))
bbref_pit <- cbind(bbref_pit[-nrow(bbref_pit),], yearID = new_season)
bbref_pit <- bbref_pit %>% filter(Lg %in% c('NL', 'AL')) %>% 
  filter(Tm != "TOT")

pitching_stats <- rbind(previous_pit_bbref, bbref_pit)

## park factor adjustment for Runs, Hits and Home Runs
## park factor adjustment from 1901 - 2022

years <- c(1901:new_season)
team_log <- do.call(rbind, lapply(years, function(x) 
  cbind(read_csv(paste("https://raw.githubusercontent.com/chadwickbureau/retrosplits/master/daybyday/teams-", x, ".csv", sep = "")), yearID = x)))

team_log <- team_log %>% select(c(team.alignment, team.key, opponent.key, 
                                  B_R, B_H, B_HR, P_R, P_H, P_HR, yearID))
park_factor <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  park_factor_year(xx)
}))

## add baseball reference teamID to chadwick teamID
library(Lahman)
teams_chadwick_1901 <- Teams %>% filter(yearID >= 1901) %>% 
  filter(lgID %in% c('NL', 'AL')) %>% 
  select(yearID, teamID, teamIDBR)

## teamID in 2022 and 2023 has not been updated on Lahman
## manually add 2022 and 2023teamID
teamID2021 <- teams_chadwick_1901 %>% filter(yearID == 2021)
teamID2022 <- teamID2021 %>% mutate(yearID = 2022)
teamID2023 <- teamID2021 %>% mutate(yearID = 2023)
teams_chadwick_1901 <- rbind(teams_chadwick_1901, rbind(teamID2022, teamID2023))

m <- park_factor
m$teamID <- as.character(m$teamID)
m$teamID[m$yearID >= 2005] <- gsub('ANA', 'LAA', m$teamID[m$yearID >= 2005])
m$teamID[m$yearID >= 1953 & m$yearID <= 1965] <- gsub('MLN', 'ML1', m$teamID[m$yearID >= 1953 & m$yearID <= 1965])
m$teamID[m$yearID >= 1970 & m$yearID <= 1997] <- gsub('MIL', 'ML4', m$teamID[m$yearID >= 1970 & m$yearID <= 1997])
park_factor <- m

## merge park factor from Chadwick with teamID from baseball reference
park_factor_1901 <- merge(park_factor, teams_chadwick_1901, 
                          by = c('yearID', 'teamID'))

## moving window for park factor. 
park_factor_averaged <- do.call(rbind, mclapply(unique(park_factor_1901$teamID), mc.cores = ncores, FUN = function(xx){
  if (xx == "LAA") {
    m <- park_factor_1901 %>% filter(teamID == xx)
    m$park_factor_R[1:4] <- moving_window_mean(m$park_factor_R[1:4])
    m$park_factor_H[1:4] <- moving_window_mean(m$park_factor_H[1:4])
    m$park_factor_HR[1:4] <- moving_window_mean(m$park_factor_HR[1:4])
    m$park_factor_R[5:21] <- moving_window_mean(m$park_factor_R[5:21])
    m$park_factor_H[5:21] <- moving_window_mean(m$park_factor_H[5:21])
    m$park_factor_HR[5:21] <- moving_window_mean(m$park_factor_HR[5:21])
    m
  } else{
    m <- park_factor_1901 %>% filter(teamID == xx)
    m$park_factor_R <- moving_window_mean(m$park_factor_R)
    m$park_factor_H <- moving_window_mean(m$park_factor_H)
    m$park_factor_HR <- moving_window_mean(m$park_factor_HR)
  }
  m
}))

pitchers <- pitching_stats[,-c(1,2)]
colnames(pitchers)[c(2,34)] <- c('teamID', 'playerID')

park_factor_br <- park_factor_averaged[,-2]
colnames(park_factor_br)[5] <- 'teamID'

pitchers_park_factor <- merge(pitchers %>% filter(yearID >= 1901), park_factor_br, 
                              by = c("teamID", "yearID"))

## No data for the teams before 1901
## No park factor for them

pre01_pitchers <- pitchers %>% filter(yearID <= 1900) 
pitchers_adj <- rbind(cbind(pre01_pitchers, park_factor_R = 1, park_factor_H = 1, 
                            park_factor_HR = 1), pitchers_park_factor)

pitchers_adj$H <- as.numeric(pitchers_adj$H)
pitchers_adj$HR <- as.numeric(pitchers_adj$HR)
pitchers_adj$ER <- as.numeric(pitchers_adj$ER)

pitchers_adj <- pitchers_adj %>% mutate(H_PK =  H / park_factor_H) %>% 
  mutate(HR_PK =  HR / park_factor_HR) %>% 
  mutate(ER_PK =  ER / park_factor_R)

## merge with population
## years to consider
years <- 1871:new_season
MLB_pop <- read.csv('datMLBeligible_0.5_favorite_sport.csv') 
MLB_pop <- rbind(MLB_pop, data.frame(year = 2021:new_season, 
                                     pop = (2:(new_season-2019))*14866270 - (1:(new_season-2020))*15041126, 
                                     ALpop = (2:(new_season-2019))*14866270 - (1:(new_season-2020))*15041126))

## add weighted populations to data
pops_data <- rbind(
  data.frame(pops = MLB_pop$pop, lgID = "NL", yearID = 1870 + 1:(15*10 + (new_season - 2020))),
  data.frame(pops = MLB_pop$ALpop, lgID = "AL", yearID = 1870 + 1:(15*10 + (new_season - 2020))))

colnames(pitchers_adj)[3] <- "lgID"
pop_pitchers <- merge(pitchers_adj, pops_data, by = c("yearID", "lgID"))

pop_pitchers$HBP[is.na(pop_pitchers$HBP)] <- 0

pop_pitchers$G <- as.numeric(pop_pitchers$G)
pop_pitchers$IP <- as.numeric(pop_pitchers$IP)
pop_pitchers$ER <- as.numeric(pop_pitchers$ER)
pop_pitchers$HR <- as.numeric(pop_pitchers$HR)
pop_pitchers$H <- as.numeric(pop_pitchers$H)
pop_pitchers$SO <- as.numeric(pop_pitchers$SO)
pop_pitchers$BB <- as.numeric(pop_pitchers$BB)
pop_pitchers$HBP <- as.numeric(pop_pitchers$HBP)

pitchers_stats <- pop_pitchers %>% group_by(playerID, yearID) %>% 
  summarise(name = unique(name), 
            age = unique(Age),
            G = sum(G), 
            lgID = lgID[which.max(IP)], 
            teamID = teamID[which.max(IP)],
            IP = sum(IP), 
            obs_ER = sum(ER), 
            obs_HR = sum(HR), 
            obs_H = sum(H), 
            SO = sum(SO), 
            ER = sum(ER_PK), 
            HR_PF = sum(HR_PK), 
            H_PF = sum(H_PK), 
            BB = sum(BB), 
            HBP = sum(HBP),
            pops = pops[which.max(IP)])


## pitching bwar from baseball reference
pitching_bwar = readr::read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt", na = "NULL")
pitching_bwar <- pitching_bwar[!is.na(pitching_bwar$WAR),]
pitching_bWAR <- pitching_bwar %>% filter(lg_ID %in% c('NL', 'AL', 'NA')) %>% 
  select(player_ID, year_ID, WAR) %>% group_by(player_ID, year_ID) %>% 
  summarise(bWAR = sum(WAR))
colnames(pitching_bWAR)[c(1,2)] <- c('playerID', 'yearID')

## all pitching stats except fWAR are merged together
pitching_stats_bWAR <- merge(pitchers_stats, pitching_bWAR, 
                             by = c('playerID', 'yearID'))

## datasets from fangraphs need membership to download. 
## I am working on the python code to deal with this issue. 
## Use the following link to download dataset
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=nl&qual=0&type=c%2c7%2c13%2c59&season=2024&month=0&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=pit&lg=al&qual=0&type=c%2C7%2C13%2C59&season=2024&month=0&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=al&qual=0&type=8&season=2024&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&month=0
## https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=nl&qual=0&type=8&season=2024&season1=2024&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&month=0
## then click the 'Export Data'. 

pitchers_fWAR_2024_AL <- read_csv("pitchers_fWAR_2024_AL.csv")
pitchers_fWAR_2024_NL <- read_csv("pitchers_fWAR_2024_NL.csv")
pitchers_fWAR_2024 <- rbind(cbind(pitchers_fWAR_2024_AL, yearID = new_season, lgID = 'AL'), 
                           cbind(pitchers_fWAR_2024_NL, yearID = new_season, lgID = 'NL'))
pitchers_fWAR_group <- pitchers_fWAR_2024 %>% group_by(PlayerId, yearID) %>% summarise(
  name = unique(Name), fWAR = sum(WAR)
)
colnames(pitchers_fWAR_group)[1] <- c('playerid')
pitchers_fWAR <- rbind(previous_pit_fangraphs, pitchers_fWAR_group)

## merge fangraph ID with baseball reference ID
people <- do.call(rbind, lapply(c(0:9, 'a', 'b', 'c', 'd', 'e', 'f'), function(x) {
  m <- read_csv(paste("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-", x, ".csv", sep = ""))
  m
})) %>% select(key_bbref, key_fangraphs)

bbref_fangraphs_ID <- people[!is.na(people$key_fangraphs),]
colnames(bbref_fangraphs_ID)[2] <- 'playerid'

pitchers_fWAR_bWAR <- merge(pitchers_fWAR, bbref_fangraphs_ID, by = 'playerid')

## combine fWAR with batting stats and bWAR

colnames(pitchers_fWAR_bWAR)[5] <- 'playerID'

pitchers <- merge(pitching_stats_bWAR, 
                  pitchers_fWAR_bWAR %>% select(playerID, yearID, fWAR, playerid), 
                  by = c('playerID', 'yearID'))

colnames(pitchers)[21] <- 'key_fangraphs'
colnames(pitchers)[1] <- 'key_bbref'
pitchers <- pitchers %>% mutate(playerID = key_bbref)

## computing the era-adjusted statistics
bat_dat <- batters
pit_dat <- pitchers

library(tidyverse)
library(orderstats)
library(Pareto)
library(doParallel)
library(splines)
library(retrosheet)
library(kableExtra)
library(Lahman)
## fix local path to functions.R on your computer
source('functions.R')

## detect the number of CPU cores
ncores <- detectCores() - 1

## specify season span
#years <- 1871:2023
years <- 1871:new_season

###### 
## bWAR for batters

## create labels for full-time batters
## set bWAR per game as component
batters <- bat_dat %>% select(yearID, playerID, lgID, name, age, PA, G, bWAR, pops)
cutoff <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  m <- batters %>% filter(yearID == xx) %>% filter(PA >= 75)
  data.frame(thres = median(m$PA), yearID = xx)
}))
batters <- merge(batters, cutoff, by = 'yearID')
batters <- batters %>% mutate(comp =  bWAR / G, full_time = ifelse(PA >= thres, 'Y', 'N'))

## apply shrinkage adjustement
batters_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- batters %>% filter(yearID == xx)
  lg_avg <- sum(int$bWAR)/sum(int$G)
  int %>% mutate(comp = (bWAR + lg_avg * 7)/(G + 7))
}))

## mapping statistics
mapped_quan_b_raw <- do.call(rbind, mclapply(years, function(xx){
  batters_full <- batters %>% filter(yearID == xx) %>% 
    filter(full_time == 'Y') %>% arrange(-G)
  batters_less <- batters %>% filter(yearID == xx) %>% 
    filter(full_time == 'N') %>% arrange(-G)
  
  n1 <- nrow(batters_full)
  n2 <- nrow(batters_less)
  
  mapped_G_full <- c()
  mapped_G_less <- c()
  for (yy in c(1977:1980, 1982:1989)) {
    batters_ref_full <- batters %>% 
      filter(yearID == yy, full_time == 'Y') %>% arrange(-G)
    batters_ref_less <- batters %>% 
      filter(yearID == yy, full_time == 'N') %>% arrange(-G)
    n1r <- nrow(batters_ref_full)
    n2r <- nrow(batters_ref_less)
    
    mapped_G_full <- cbind(mapped_G_full, approx(x = seq((n1r-1),0)/(n1r-1), 
                                                 y = batters_ref_full$G, 
                                                 xout = seq((n1-1),0)/(n1-1))$y)
    mapped_G_less <- cbind(mapped_G_less, approx(x = seq((n2r-1),0)/(n2r-1), 
                                                 y = batters_ref_less$G, 
                                                 xout = seq((n2-1),0)/(n2-1))$y)
  }
  
  batters_full$mapped_G <- rowMeans(mapped_G_full)
  batters_less$mapped_G <- rowMeans(mapped_G_less)
  
  
  batters_full <- batters_full %>% arrange(-PA)
  batters_less <- batters_less %>% arrange(-PA)
  mapped_PA_full <- c()
  mapped_PA_less <- c()
  for (yy in c(1977:1980, 1982:1989)) {
    batters_ref_full <- batters %>% 
      filter(yearID == yy, full_time == 'Y') %>% arrange(-PA)
    batters_ref_less <- batters %>% 
      filter(yearID == yy, full_time == 'N') %>% arrange(-PA)
    n1r <- nrow(batters_ref_full)
    n2r <- nrow(batters_ref_less)
    
    mapped_PA_full <- cbind(mapped_PA_full, approx(x = seq((n1r-1),0)/(n1r-1), 
                                                   y = batters_ref_full$PA, 
                                                   xout = seq((n1-1),0)/(n1-1))$y)
    mapped_PA_less <- cbind(mapped_PA_less, approx(x = seq((n2r-1),0)/(n2r-1), 
                                                   y = batters_ref_less$PA, 
                                                   xout = seq((n2-1),0)/(n2-1))$y)
  }
  
  batters_full$mapped_PA <- rowMeans(mapped_PA_full)
  batters_less$mapped_PA <- rowMeans(mapped_PA_less)
  
  m <- rbind(batters_full, batters_less)
  data.frame(playerID = m$playerID, yearID = m$yearID,
             mapped_G_raw = round(m$mapped_G), mapped_PA_raw = round(m$mapped_PA))
  
}, mc.cores = ncores))

res <- era_adjusted_comp(component_name = 'bWAR', dataset = batters_schell)

mapped_batters_1 <- merge(res$era_adjusted, 
                          mapped_quan_b_raw, by = c('playerID', 'yearID'))

## minimum bWAR in the league
min_refbWAR <- -2

## get era-adjusted statistic using mapped statistics 
## trim the era-adjusted statistic below the minimum bWAR
mapped_batters_bWAR <-mapped_batters_1 %>% 
  mutate(adj_bWAR = adj_comp * mapped_G_raw) %>% 
  mutate(adj_bWAR = ifelse(adj_bWAR < min_refbWAR, min_refbWAR, adj_bWAR)) %>%
  mutate(mapped_G_bWAR = round(adj_bWAR / adj_comp))

######

###### 
## fWAR for batters

## create labels for full-time batters
## set fWAR per game as component
batters <- bat_dat %>% select(yearID, playerID, lgID, name, age, PA, G, bWAR,fWAR,pops)
batters <- merge(batters, cutoff, by = 'yearID')
batters <- batters %>% mutate(comp =  fWAR / G, full_time = ifelse(PA >= thres, 'Y', 'N'))

## apply shrinkage adjustement
batters_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- batters %>% filter(yearID == xx)
  lg_avg <- sum(int$fWAR)/sum(int$G)
  int %>% mutate(comp = (fWAR + lg_avg * 7)/(G + 7))
}))

## get era-adjusted statistic
res <- era_adjusted_comp(component_name = 'fWAR', dataset = batters_schell)

mapped_batters_1 <- merge(res$era_adjusted, 
                          mapped_quan_b_raw, by = c('playerID', 'yearID'))

## minimum fWAR in the league
min_reffWAR <- -2

## get era-adjusted statistic using mapped statistics 
## trim the era-adjusted statistic below the minimum fWAR

mapped_batters_fWAR <- mapped_batters_1 %>% 
  mutate(adj_fWAR = adj_comp * mapped_G_raw) %>% 
  mutate(adj_fWAR = ifelse(adj_fWAR < min_reffWAR, min_reffWAR, adj_fWAR)) %>%
  mutate(mapped_G_fWAR = round(adj_fWAR / adj_comp))
######

######
## HR for batters
batters_HR <- bat_dat %>% 
  select(yearID, playerID, lgID, name, PA, AB, HR, BB, HBP, SH, SF, bWAR, pops)

batters <- merge(batters_HR, cutoff, by = 'yearID')

batters <- batters %>% mutate(comp =  ifelse(AB != 0, HR / AB, 0), full_time = ifelse(PA >= thres, 'Y', 'N'))

batters_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- batters %>% filter(yearID == xx)
  lg_avg <- sum(int$HR)/sum(int$AB)
  int %>% mutate(comp = (HR + lg_avg * 25)/(AB + 25))
}))

## get era-adjusted statistic
res <- era_adjusted_comp(component_name = 'HR', dataset = batters_schell)
career_kAB <- res$era_adjusted

######

######
## BB for batters
batters_BB <- bat_dat %>% select(yearID, playerID, lgID, name, G, PA, BB, pops)
batters <- merge(batters_BB, cutoff, by = 'yearID')
batters <- batters %>% mutate(comp = ifelse(PA > 0, BB / PA, 0), full_time = ifelse(PA >= thres, 'Y', 'N'))

batters_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- batters %>% filter(yearID == xx)
  lg_avg <- sum(int$BB)/sum(int$PA)
  int %>% mutate(comp = (BB + lg_avg * 28)/(PA + 28))
}))

res <- era_adjusted_comp(component_name = 'BB', dataset = batters_schell)

career_kAB_BB <- res$era_adjusted

colnames(career_kAB)[colnames(career_kAB) == 'adj_comp'] <- "adj_HR_AB"

career_combined <- merge(career_kAB %>% select(-comp), career_kAB_BB %>% 
                           select(yearID, playerID, comp, adj_comp), by = c('yearID', 'playerID'))

mapped_batters_1 <- merge(career_combined, mapped_quan_b_raw, 
                          by = c('playerID', 'yearID'))

## find lower bound for BB and HR
IDs7789 = Batting %>% 
  filter(yearID >= 1977, yearID <= 1989, yearID != 1981) %>% 
  pull(playerID)

IDs10 = Batting %>% 
  filter(playerID %in% IDs7789) %>% 
  group_by(playerID) %>% 
  summarise(n = n()) %>% 
  filter(n >= 10) %>% 
  pull(playerID)

lowerb <- Batting %>% 
  filter(playerID %in% IDs10) %>% 
  filter(yearID >= 1977, yearID <= 1989, yearID != 1981) %>% 
  mutate(PA = AB + BB + HBP + SH + SF) %>% 
  filter(PA >= 400) %>% 
  mutate(BBrate = BB/PA, HRrate = HR/AB) %>% 
  select(playerID, yearID, BBrate, HRrate) %>% 
  summarise(Q03BB = quantile(BBrate, probs = 0.03), 
            Q03HR = quantile(HRrate, probs = 0.03))

min_refBBrate <- lowerb$Q03BB
min_refHRrate <- lowerb$Q03HR

mapped_batters_1$adj_comp[mapped_batters_1$full_time == 'Y' & mapped_batters_1$adj_comp < min_refBBrate] <- min_refBBrate
mapped_batters_1$adj_HR_AB[mapped_batters_1$full_time == 'Y' & mapped_batters_1$adj_HR_AB < min_refHRrate] <- min_refHRrate

mapped_batters_HR <- mapped_batters_1 %>%
  mutate(adj_BB = ceiling(adj_comp * ceiling(mapped_PA_raw))) %>% 
  mutate(mapped_PA = mapped_PA_raw) %>%
  mutate(adj_AB = ceiling(mapped_PA) - adj_BB - HBP - SH - SF) %>% 
  mutate(adj_AB = ifelse(adj_AB < 0, 0, adj_AB)) %>% 
  mutate(adj_HR = round(adj_HR_AB * adj_AB))
######

######
## BA for batters
batters <- bat_dat
batters <- merge(batters, cutoff, by = 'yearID')
batters <- batters %>% select(playerID, yearID, lgID, name, age, lgID, AB, 
                              PA, obs_hits, H, thres, bWAR, pops) %>%
  mutate(comp = ifelse(AB != 0, H / AB, 0), full_time = ifelse(PA >= thres, 'Y', 'N'))

batters_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- batters %>% filter(yearID == xx)
  lg_avg <- sum(int$H)/sum(int$AB)
  int %>% mutate(comp = (H + lg_avg * 25)/(AB + 25))
}))

res <- era_adjusted_comp(component_name = 'AVG', dataset = batters_schell)

career_kAB <- res$era_adjusted

min_refAVG <- res$minimum

mapped_batters_AVG_nonpara <- career_kAB %>% 
  mutate(adj_AVG = ifelse(adj_comp < min_refAVG, min_refAVG, adj_comp))

######

######
## combine statistics together

AVG_part <- mapped_batters_AVG_nonpara %>%
  mutate(adj_AVG = round(adj_AVG, 3)) %>% 
  select(yearID, playerID, name, adj_AVG) 
HR_part <- mapped_batters_HR %>% 
  mutate(mapped_PA = round(mapped_PA)) %>% 
  mutate(adj_HR = round(adj_HR)) %>%
  mutate(adj_AB = round(adj_AB)) %>%
  select(yearID, playerID, adj_HR, adj_AB, HBP, SF, mapped_PA) 
bWAR_part <- mapped_batters_bWAR %>%
  mutate(adj_bWAR = round(adj_bWAR, 2)) %>%
  select(yearID, playerID, adj_bWAR, mapped_G_bWAR) 
fWAR_part <- mapped_batters_fWAR %>%
  mutate(adj_fWAR = round(adj_fWAR, 2)) %>%
  select(yearID, playerID, age, adj_fWAR, mapped_G_fWAR) 
BB_part <- mapped_batters_HR %>% 
  mutate(adj_BB = round(adj_BB)) %>%
  select(yearID, playerID, BB, adj_BB)
master_batters <- merge(BB_part, merge(AVG_part, 
                                       merge(HR_part, merge(bWAR_part, fWAR_part, 
                                                            by = c('yearID', 'playerID')), 
                                             by = c('yearID', 'playerID')), 
                                       by = c('yearID', 'playerID')), 
                        by = c('yearID', 'playerID'))

master_batters <- master_batters %>% 
  mutate(adj_OBP = round((adj_AVG * adj_AB + adj_BB + HBP) / (adj_AB + adj_BB + HBP + SF), 3))
master_batters$adj_OBP[is.na(master_batters$adj_OBP)] <- 0

master_batters_nonpara <- master_batters %>% mutate(adj_BB = ifelse(adj_BB < 0, 0, adj_BB)) %>% 
  mutate(adj_AB = ifelse(mapped_PA < adj_AB, mapped_PA, adj_AB))
master_batters_nonpara$mapped_G <- apply(master_batters_nonpara[,c(13,16)], 1, min)

## Batters
batters <- master_batters_nonpara

## extract and remove bad players 
foo <- batters %>% 
  arrange(desc(adj_AVG)) %>% 
  filter(adj_AB >= 300) %>% 
  dplyr::select(name, playerID, yearID, adj_AB, adj_AVG, adj_OBP, adj_HR, adj_fWAR, adj_bWAR) %>% 
  mutate(adj_HR_AB = round(adj_HR/adj_AB,4))
bar <- split(foo, as.factor(foo$playerID))
baz <- do.call(rbind, lapply(bar, function(m){
  m[which.max(m$adj_fWAR), ]
}))
baz <- baz %>% arrange(adj_fWAR)
bad_players_fWAR <- baz %>% filter(adj_fWAR < 0) %>% pull(playerID)

baz <- do.call(rbind, lapply(bar, function(m){
  m[which.max(m$adj_bWAR), ]
}))
baz <- baz %>% arrange(adj_bWAR)
bad_players_bWAR <- baz %>% filter(adj_bWAR < 0) %>% pull(playerID)
bad_players <- union(bad_players_bWAR, bad_players_fWAR)
batters <- batters[!batters$playerID %in% bad_players, ]

## investigate anomalies
## more on base events than PAs

# check average for minimal at bats and correct issues
batters[batters$adj_AVG > 0 & batters$adj_AB == 0, ]$adj_AVG <- 0 
batters <- batters %>% mutate(adj_hits = round(adj_AVG * adj_AB))
batters[batters$adj_AB > 0, ]$adj_AVG <- 
  round(batters[batters$adj_AB > 0, ]$adj_hits / batters[batters$adj_AB > 0, ]$adj_AB, 3)

## build adjusted data set
batters_adjusted <- batters %>% 
  dplyr::select(name, playerID, age, yearID, mapped_PA, adj_AB, adj_hits, adj_HR, adj_BB, 
                adj_AVG, adj_OBP, HBP, SF, adj_bWAR, adj_fWAR)
colnames(batters_adjusted) <- c("name", "playerID", "age", "year", "mapped_PA", "adj_AB", "adj_H", 
                                "adj_HR", "adj_BB", "adj_AVG", "adj_OBP", "HBP", "SF", "adj_bWAR", "adj_fWAR")
batters_adjusted$playerID <- droplevels(as.factor(batters_adjusted$playerID))


## trim out bad players
# first round
foo <- split(batters_adjusted, f = batters_adjusted$playerID)
bar <- lapply(foo, function(m){
  ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0)
})
checker <- data.frame(pid = levels(batters_adjusted$playerID), 
                      m_bad = unlist(lapply(bar, mean)), 
                      len = unlist(lapply(bar, length)))
batters_adjusted <- batters_adjusted %>% 
  filter(!batters_adjusted$playerID %in% rownames(checker)[checker$m_bad == 2])
batters_adjusted$playerID <- droplevels(batters_adjusted$playerID)

# second round
foo <- split(batters_adjusted, f = batters_adjusted$playerID)
bar <- lapply(foo, function(m){
  ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0)
})
checker <- data.frame(pid = levels(batters_adjusted$playerID), 
                      m_bad = unlist(lapply(bar, mean)), 
                      len = unlist(lapply(bar, length)))
batters_adjusted <- batters_adjusted %>% 
  filter(!batters_adjusted$playerID %in% rownames(checker)[checker$m_bad >= 1 & checker$len <= 2])
batters_adjusted$playerID <- droplevels(batters_adjusted$playerID)

# third round
foo <- split(batters_adjusted, f = batters_adjusted$playerID)
bar <- lapply(foo, function(m){
  min(ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0))
})
checker <- data.frame(pid = levels(batters_adjusted$playerID), 
                      m_bad = unlist(lapply(bar, mean)), 
                      len = unlist(lapply(bar, length)))
batters_adjusted <- batters_adjusted %>% 
  filter(!batters_adjusted$playerID %in% rownames(checker)[checker$m_bad >= 1])
batters_adjusted$playerID <- droplevels(batters_adjusted$playerID)

# forth round
foo <- split(batters_adjusted, f = batters_adjusted$playerID)
bar <- lapply(foo, function(m){
  ifelse(m$adj_bWAR == -2, 1, 0) + ifelse(m$adj_fWAR == -2, 1, 0)
})
checker <- data.frame(pid = levels(batters_adjusted$playerID), 
                      m_bad = unlist(lapply(bar, mean)), 
                      len = unlist(lapply(bar, length)))
batters_adjusted <- batters_adjusted %>% 
  filter(!batters_adjusted$playerID %in% rownames(checker)[checker$m_bad >= 1])
batters_adjusted$playerID <- droplevels(batters_adjusted$playerID)


## remove tails 
foo <- split(batters_adjusted, f = batters_adjusted$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_bWAR <= 0.2, 1, 0) + ifelse(m$adj_fWAR <= 0.2, 1, 0)
  bad_tail <- sum(c(ifelse(sum(tail(bad, 2)) >= 3,1,0),
                    ifelse(sum(tail(bad, 3)) >= 5,1,0),
                    ifelse(sum(tail(bad, 4)) >= 7,1,0),
                    ifelse(sum(tail(bad, 5)) >= 9,1,0),
                    ifelse(sum(tail(bad, 6)) >= 11,1,0)))
  1:(length(bad)-bad_tail)
})
batters_adjusted_1 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

foo <- split(batters_adjusted_1, f = batters_adjusted_1$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_fWAR <= -1.5, 1, 0) 
  bad_tail <- sum(c(ifelse(sum(tail(bad, 2)) >= 2,1,0),
                    ifelse(sum(tail(bad, 3)) >= 3,1,0),
                    ifelse(sum(tail(bad, 4)) >= 4,1,0),
                    ifelse(sum(tail(bad, 5)) >= 5,1,0),
                    ifelse(sum(tail(bad, 6)) >= 6,1,0)))
  1:(length(bad)-bad_tail)
})
batters_adjusted_2 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

foo <- split(batters_adjusted_2, f = batters_adjusted_2$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_bWAR <= -1.5, 1, 0) 
  bad_tail <- sum(c(ifelse(sum(tail(bad, 2)) >= 2,1,0),
                    ifelse(sum(tail(bad, 3)) >= 3,1,0),
                    ifelse(sum(tail(bad, 4)) >= 4,1,0),
                    ifelse(sum(tail(bad, 5)) >= 5,1,0),
                    ifelse(sum(tail(bad, 6)) >= 6,1,0)))
  1:(length(bad)-bad_tail)
})
batters_adjusted_3 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

## remove starts
foo <- split(batters_adjusted_3, f = batters_adjusted_3$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0)
  bad_head <- sum(c(ifelse(sum(head(bad, 1)) == 2,1,0),
                    ifelse(sum(head(bad, 2)) >= 3,1,0),
                    ifelse(sum(head(bad, 3)) >= 5,1,0),
                    ifelse(sum(head(bad, 4)) >= 7,1,0),
                    ifelse(sum(head(bad, 5)) >= 9,1,0),
                    ifelse(sum(head(bad, 6)) >= 11,1,0)))
  if (bad_head < length(bad)) {
    (bad_head + 1):length(bad)
  }
})
batters_adjusted_4 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

batters_adjusted_4$playerID <- droplevels(batters_adjusted_4$playerID)

# taper down average WAR for players with small PAs
batters_adjusted_4[batters_adjusted_4$mapped_PA <= 20, ]$adj_fWAR <- 
  round(batters_adjusted_4[batters_adjusted_4$mapped_PA <= 20, ]$adj_fWAR/9,2)
batters_adjusted_4[batters_adjusted_4$mapped_PA <= 20, ]$adj_bWAR <- 
  round(batters_adjusted_4[batters_adjusted_4$mapped_PA <= 20, ]$adj_bWAR/9,2)	

batters_adjusted <- do.call(rbind, mclapply(
  split(batters_adjusted_4, f = droplevels(as.factor(batters_adjusted_4$playerID))), 
  mc.cores = ncores, FUN = function(xx){
    ## natural cubic spline
    #ns_AVG = lm(adj_AVG ~ ns(year, df=6), data=xx)
    #nn_AVG <- predict(ns_AVG, data.frame("year"= xx$year))
    #ns_HR = lm(adj_HR ~ ns(year, df=6), data=xx)
    #nn_HR <- predict(ns_HR, data.frame("year"= xx$year))
    #ns_BB = lm(adj_BB ~ ns(year, df=6), data=xx)
    #nn_BB <- predict(ns_BB, data.frame("year"= xx$year))
    #ns_bWAR = lm(adj_bWAR ~ ns(year, df=6), data=xx)
    #nn_bWAR <- predict(ns_bWAR, data.frame("year"= xx$year))
    #ns_fWAR = lm(adj_fWAR ~ ns(year, df=6), data=xx)
    #nn_fWAR <- predict(ns_fWAR, data.frame("year"= xx$year))
    
    xx %>% mutate(AVG = round(adj_AVG, 3)) %>% 
      mutate(HR = round(adj_HR)) %>%
      mutate(BB = round(adj_BB)) %>%
      mutate(ebWAR = round(adj_bWAR, 2)) %>%
      mutate(efWAR = round(adj_fWAR, 2))
  })) 

bat_season <- batters_adjusted %>% 
  mutate(PA = adj_AB + BB + HBP + SF)
bat_season[bat_season$HR >= bat_season$adj_AB, ]$HR <- 
  bat_season[bat_season$HR >= bat_season$adj_AB, ]$adj_AB

bat_season <- bat_season %>% 
  dplyr::select(-c("adj_H", "adj_HR", "adj_BB", "adj_AVG", "adj_OBP", "adj_bWAR", "adj_fWAR", 'mapped_PA'))
bat_season <- bat_season %>% mutate(OBP = round((AVG * adj_AB + BB + HBP)/(adj_AB + BB + HBP + SF), 3 )) %>%
  mutate(AVG = ifelse(AVG > 0, AVG, 0)) %>%
  mutate(HR = ifelse(HR > 0, HR, 0)) %>%
  mutate(BB = ifelse(BB > 0, BB, 0)) %>%
  mutate(OBP = ifelse(OBP > 0, OBP, 0))

colnames(bat_season)[5] <- 'AB'
colnames(bat_season)[8] <- 'BA'
bat_season_nonpara <- bat_season %>% mutate(H = ceiling(AB * BA)) %>%
  mutate(BA = round(H / AB, 3)) %>%
  mutate(BA = ifelse(AB == 0, 0, BA)) %>%
  mutate(OBP = round((H+BB+HBP)/(AB+BB+HBP+SF), 3)) %>%
  mutate(OBP = ifelse(AB+BB+HBP+SF == 0, 0, OBP))

bat_career_nonpara <- bat_season_nonpara %>% group_by(playerID) %>% 
  summarise(name = unique(name), 
            playerID = unique(playerID), 
            PA = sum(round(PA)), 
            AB = sum(AB), 
            H = sum(H), 
            HR = sum(round(HR)), 
            BB = sum(round(BB)), 
            BA = round(H/AB, 3), 
            HBP = sum(HBP), 
            SF = sum(SF), 
            OBP = round((H + BB + HBP)/(AB + BB + HBP + SF), 3), 
            ebWAR = sum(ebWAR), 
            efWAR = sum(efWAR)) %>% ungroup() %>% 
  arrange(desc(ebWAR))

bat_career_nonpara <- bat_career_nonpara %>% mutate(BA = ifelse(AB == 0, 0, BA))
bat_career_nonpara <- bat_career_nonpara %>% mutate(OBP = ifelse(AB + BB + HBP + SF == 0, 0, OBP))

######

######
## bWAR for pitchers

foo <- getRetrosheet(type = "game", year = 2023)
rotation_bound <- as.data.frame(cbind(years, unlist(mclapply(years, mc.cores = ncores, function(yr){
  foo <- getRetrosheet(type = "game", year = yr)
  Tms <- unique(foo$HmTm)
  
  starter <- lapply(Tms, function(tm){
    bar <- foo %>% filter(VisTm == tm | HmTm == tm) %>% 
      select(VisTm, HmTm, VisStPchID, HmStPchID, VisTmGNum, HmTmGNum)
    t(apply(bar, 1, function(xx){
      y <- NULL
      if(xx[2] == tm){
        y <- xx[4]
      }
      else{
        y <- xx[3]
      }
    })) 
  })
  
  y <- unlist(lapply(starter, function(xx){
    unlist(lapply(1:length(xx), function(j){
      flag <- TRUE
      k <- 1
      if(j > 1){
        while(flag){
          flag <- length(xx[(j-k):j]) == length(unique(xx[(j-k):j]))
          if(!flag){
            break
          }
          else{
            k <- k + 1
          }
          if(k == j){
            break
          }
        }
      }
      k
    }))
  }))
  mean(y)
  
}))))

pitchers <- pit_dat %>% 
  select(playerID, yearID, name, age, lgID, teamID, IP, pops, bWAR)
pitchers <- pitchers %>% mutate(comp = bWAR / IP)
pitchers$comp[is.na(pitchers$comp)] = 0

rotation_bound <- as.data.frame(cbind(years, unlist(mclapply(years, mc.cores = ncores, function(yr){
  foo <- getRetrosheet(type = "game", year = yr)
  Tms <- unique(foo$HmTm)
  
  starter <- lapply(Tms, function(tm){
    bar <- foo %>% filter(VisTm == tm | HmTm == tm) %>% 
      select(VisTm, HmTm, VisStPchID, HmStPchID, VisTmGNum, HmTmGNum)
    t(apply(bar, 1, function(xx){
      y <- NULL
      if(xx[2] == tm){
        y <- xx[4]
      }
      else{
        y <- xx[3]
      }
    })) 
  })
  
  y <- unlist(lapply(starter, function(xx){
    unlist(lapply(1:length(xx), function(j){
      flag <- TRUE
      k <- 1
      if(j > 1){
        while(flag){
          flag <- length(xx[(j-k):j]) == length(unique(xx[(j-k):j]))
          if(!flag){
            break
          }
          else{
            k <- k + 1
          }
          if(k == j){
            break
          }
        }
      }
      k
    }))
  }))
  mean(y)
  
}))))

unique_team <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  c(xx, nrow(unique(pitchers %>% filter(yearID == xx) %>% select(teamID))))
}))
rotation_player <- data.frame(yearID = years, rotation = ceiling(rotation_bound[,2] * unique_team[,2]))
cutoff <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  m <- pitchers %>% filter(yearID == xx) %>% arrange(-IP)
  index <- rotation_player$rotation[rotation_player$yearID == xx]
  data.frame(thres = (m$IP[index]), yearID = xx)
}))
pitchers <- merge(pitchers, cutoff, by = 'yearID')
pitchers <- pitchers %>% mutate(full_time = ifelse(IP >= thres, 'Y', 'N'))

pitchers_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- pitchers %>% filter(yearID == xx)
  lg_avg <- sum(int$bWAR)/sum(int$IP)
  int %>% mutate(comp = (bWAR + lg_avg * 14)/(IP + 14))
}))
mapped_quan_p <- do.call(rbind, mclapply(years, function(xx){
  pitchers_full <- pitchers %>% filter(yearID == xx) %>% 
    filter(full_time == 'Y') %>% arrange(-IP)
  pitchers_less <- pitchers %>% filter(yearID == xx) %>% 
    filter(full_time == 'N') %>% arrange(-IP)
  
  n1 <- nrow(pitchers_full)
  n2 <- nrow(pitchers_less)
  
  mapped_IP_full <- c()
  mapped_IP_less <- c()
  for (yy in c(1977:1980, 1982:1989)) {
    pitchers_ref_full <- pitchers %>% 
      filter(yearID == yy, full_time == 'Y') %>% arrange(-IP)
    pitchers_ref_less <- pitchers %>% 
      filter(yearID == yy, full_time == 'N') %>% arrange(-IP)
    n1r <- nrow(pitchers_ref_full)
    n2r <- nrow(pitchers_ref_less)
    
    mapped_IP_full <- cbind(mapped_IP_full, approx(x = seq((n1r-1),0)/(n1r-1), 
                                                   y = pitchers_ref_full$IP,
                                                   xout = seq((n1-1),0)/(n1-1))$y)
    mapped_IP_less <- cbind(mapped_IP_less, approx(x = seq((n2r-1),0)/(n2r-1), 
                                                   y = pitchers_ref_less$IP, 
                                                   xout = seq((n2-1),0)/(n2-1))$y)
  }
  
  pitchers_full$mapped_IP <- rowMeans(mapped_IP_full)
  pitchers_less$mapped_IP <- rowMeans(mapped_IP_less)
  
  m <- rbind(pitchers_full, pitchers_less)
  data.frame(playerID = m$playerID, yearID = m$yearID, 
             mapped_IP_raw = round(m$mapped_IP))
  
}, mc.cores = ncores))

res <- era_adjusted_comp(component_name = 'bWAR_p', dataset = pitchers_schell)

mapped_pitchers_1 <- merge(res$era_adjusted, 
                           mapped_quan_p, by = c('playerID', 'yearID'))

mapped_pitchers_bWAR <- mapped_pitchers_1 %>% 
  mutate(adj_bWAR = adj_comp * mapped_IP_raw) %>%
  mutate(adj_bWAR = ifelse(adj_bWAR < min_refbWAR, min_refbWAR, adj_bWAR)) %>%
  mutate(mapped_IP_bWAR = round(adj_bWAR / adj_comp))
######

######
## fWAR for pitchers
pitchers <- pit_dat %>% 
  select(playerID, yearID, name, age, lgID, teamID, IP, pops, fWAR)
pitchers <- pitchers %>% mutate(comp = fWAR / IP)
pitchers$comp[is.na(pitchers$comp)] = 0
pitchers <- merge(pitchers, cutoff, by = 'yearID')
pitchers <- pitchers %>% mutate(full_time = ifelse(IP >= thres, 'Y', 'N'))

pitchers_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- pitchers %>% filter(yearID == xx)
  lg_avg <- sum(int$fWAR)/sum(int$IP)
  int %>% mutate(comp = (fWAR + lg_avg * 14)/(IP + 14))
}))

res <- era_adjusted_comp(component_name = 'fWAR_p', dataset = pitchers_schell)

mapped_pitchers_1 <- merge(res$era_adjusted, 
                           mapped_quan_p, by = c('playerID', 'yearID'))

mapped_pitchers_fWAR <- mapped_pitchers_1 %>% 
  mutate(adj_fWAR = adj_comp * mapped_IP_raw) %>%
  mutate(adj_fWAR = ifelse(adj_fWAR < min_reffWAR, min_reffWAR, adj_fWAR)) %>%
  mutate(mapped_IP_fWAR = round(adj_fWAR / adj_comp))

mapped_quan_p_merge <- merge(mapped_pitchers_bWAR %>% select(yearID, playerID, mapped_IP_bWAR), 
                             mapped_pitchers_fWAR %>% select(yearID, playerID, mapped_IP_fWAR), 
                             by = c('yearID', 'playerID')) 
mapped_quan_p_merge$mapped_IP <- apply(mapped_quan_p_merge[,c(3,4)], 1, min)
mapped_quan_p <- mapped_quan_p_merge %>% select(yearID, playerID, mapped_IP)
######

######
## SO for pitchers
pitchers <- pit_dat %>% 
  select(playerID, yearID, name, age, lgID, teamID, IP, pops, SO)
pitchers <- pitchers %>% mutate(comp = SO / IP *9)
pitchers$comp[is.na(pitchers$comp)] = 0
pitchers <- merge(pitchers, cutoff, by = 'yearID')
pitchers <- pitchers %>% mutate(full_time = ifelse(IP >= thres, 'Y', 'N'))

pitchers_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- pitchers %>% filter(yearID == xx)
  lg_avg <- sum(int$SO)/sum(int$IP)
  int %>% mutate(comp = (SO + lg_avg * 14)/(IP + 14) * 9)
}))

res <- era_adjusted_comp(component_name = 'SO', dataset = pitchers_schell)

mapped_pitchers_1 <- merge(res$era_adjusted, 
                           mapped_quan_p, by = c('playerID', 'yearID'))

## get minimum threshold for SO
min_refSO <- res$minimum

mapped_pitchers_SO <- mapped_pitchers_1 %>% 
  mutate(adj_SO = adj_comp * mapped_IP / 9) %>%
  mutate(adj_SO = ifelse(adj_SO < min_refSO, min_refSO, adj_SO))

######

######
## ERA for pitchers
pitchers <- pit_dat %>% 
  select(playerID, yearID, name, age, lgID, teamID, IP, pops, ER)
pitchers <- pitchers %>% mutate(comp = -9*ER / IP)
pitchers$comp[is.na(pitchers$comp)] = 0
pitchers <- merge(pitchers, cutoff, by = 'yearID')
pitchers <- pitchers %>% mutate(full_time = ifelse(IP >= thres, 'Y', 'N'))

pitchers_schell <- do.call(rbind, mclapply(years, mc.cores = ncores, FUN = function(xx){
  int<- pitchers %>% filter(yearID == xx)
  lg_avg <- sum(int$ER)/sum(int$IP)
  int %>% mutate(comp = -9*(ER + lg_avg * 14)/(IP + 14) )
}))

res <- era_adjusted_comp(component_name = 'ERA', dataset = pitchers_schell)

mapped_pitchers_1 <- merge(res$era_adjusted, 
                           mapped_quan_p, by = c('playerID', 'yearID'))

min_refERA <- res$minimum

mapped_pitchers_ERA <- mapped_pitchers_1 %>% 
  mutate(adj_ERA = ifelse(adj_comp > min_refERA, min_refERA, adj_comp))

######

######
## combine statistics together

ERA_part <- mapped_pitchers_ERA %>% 
  mutate(mapped_IP = round(mapped_IP, 1)) %>%
  mutate(adj_ERA = round(-adj_comp, 3)) %>%
  select(yearID, playerID, name, IP, mapped_IP, adj_ERA)
SO_part <- mapped_pitchers_SO %>% 
  mutate(adj_SO = round(adj_SO))%>%
  select(yearID, playerID, SO, adj_SO)
bWAR_part <- mapped_pitchers_bWAR %>% 
  mutate(adj_bWAR = round(adj_bWAR, 2)) %>% 
  select(yearID, playerID, adj_bWAR, full_time)
fWAR_part <- mapped_pitchers_fWAR %>%
  mutate(adj_fWAR = round(adj_fWAR, 2)) %>% 
  select(yearID, age, playerID, adj_fWAR) 

master_pitchers <- merge(ERA_part, merge(SO_part, merge(bWAR_part, fWAR_part, by = c('yearID', 'playerID')), 
                                         by = c('yearID', 'playerID')), by = c('yearID', 'playerID'))
## Pitchers
pitchers <- master_pitchers %>% 
  mutate(adj_K9 = round(adj_SO/mapped_IP*9,1), 
         K9 = round(SO/IP*9,1), 
         adj_fWAR = round(adj_fWAR, 2))
pitchers <- as.data.frame(pitchers)
#pitchers$avg_ERA[pitchers$avg_ERA > 6.20] <- 6.20
#pitchers <- pitchers[pitchers$adj_bWAR != 0, ]

## extract and remove bad pitchers 
foo <- pitchers %>% 
  arrange(adj_ERA) %>% 
  filter(mapped_IP >= 100) %>% 
  dplyr::select(name, playerID, yearID, mapped_IP, adj_ERA, adj_SO, adj_fWAR, adj_bWAR)
bar <- split(foo, as.factor(foo$playerID))
baz <- do.call(rbind, lapply(bar, function(m){
  m[which.max(m$adj_fWAR), ]
}))
baz <- baz %>% arrange(adj_fWAR)
bad_players_fWAR <- baz %>% filter(adj_fWAR < 0) %>% pull(playerID)

baz <- do.call(rbind, lapply(bar, function(m){
  m[which.max(m$adj_bWAR), ]
}))
baz <- baz %>% arrange(adj_bWAR)
bad_players_bWAR <- baz %>% filter(adj_bWAR < 0) %>% pull(playerID)
bad_players <- union(bad_players_bWAR, bad_players_fWAR)
pitchers <- pitchers[!pitchers$playerID %in% bad_players, ]


## build adjusted data set
pitchers_adjusted <- pitchers %>% dplyr::select(name, playerID, age, yearID, 
                                                mapped_IP, adj_ERA, adj_SO, adj_bWAR, adj_fWAR, full_time)
colnames(pitchers_adjusted) <- c("name", "playerID", "age", "year", "IP", 
                                 "adj_ERA", "adj_SO", "adj_bWAR", "adj_fWAR", "full_time")
pitchers_adjusted$playerID <- droplevels(as.factor(pitchers_adjusted$playerID))

## trim out bad players
# first round
foo <- split(pitchers_adjusted, f = pitchers_adjusted$playerID)
bar <- lapply(foo, function(m){
  ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0)
})
checker <- data.frame(pid = levels(pitchers_adjusted$playerID),
                      m_bad = unlist(lapply(bar, mean)),
                      len = unlist(lapply(bar, length)))
pitchers_adjusted <- pitchers_adjusted %>%
  filter(!pitchers_adjusted$playerID %in% rownames(checker)[checker$m_bad == 2])
pitchers_adjusted$playerID <- droplevels(pitchers_adjusted$playerID)

# second round
foo <- split(pitchers_adjusted, f = pitchers_adjusted$playerID)
bar <- lapply(foo, function(m){
  ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0)
})
checker <- data.frame(pid = levels(pitchers_adjusted$playerID),
                      m_bad = unlist(lapply(bar, mean)),
                      len = unlist(lapply(bar, length)))
pitchers_adjusted <- pitchers_adjusted %>%
  filter(!pitchers_adjusted$playerID %in% rownames(checker)[checker$m_bad >= 1 & checker$len <= 2])
pitchers_adjusted$playerID <- droplevels(pitchers_adjusted$playerID)

# third round
foo <- split(pitchers_adjusted, f = pitchers_adjusted$playerID)
bar <- lapply(foo, function(m){
  min(ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0))
})
checker <- data.frame(pid = levels(pitchers_adjusted$playerID),
                      m_bad = unlist(lapply(bar, mean)),
                      len = unlist(lapply(bar, length)))
pitchers_adjusted <- pitchers_adjusted %>%
  filter(!pitchers_adjusted$playerID %in% rownames(checker)[checker$m_bad >= 1])
pitchers_adjusted$playerID <- droplevels(pitchers_adjusted$playerID)

# forth round
foo <- split(pitchers_adjusted, f = pitchers_adjusted$playerID)
bar <- lapply(foo, function(m){
  ifelse(m$adj_bWAR == -2, 1, 0) + ifelse(m$adj_fWAR == -2, 1, 0)
})
checker <- data.frame(pid = levels(pitchers_adjusted$playerID),
                      m_bad = unlist(lapply(bar, mean)),
                      len = unlist(lapply(bar, length)))
pitchers_adjusted <- pitchers_adjusted %>%
  filter(!pitchers_adjusted$playerID %in% rownames(checker)[checker$m_bad == 2])
pitchers_adjusted$playerID <- droplevels(pitchers_adjusted$playerID)


## remove tails 
foo <- split(pitchers_adjusted, f = pitchers_adjusted$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_bWAR <= 0.41, 1, 0) + ifelse(m$adj_fWAR <= 0.4, 1, 0)
  bad_tail <- sum(c(ifelse(sum(tail(bad, 2)) >= 3,1,0),
                    ifelse(sum(tail(bad, 3)) >= 5,1,0),
                    ifelse(sum(tail(bad, 4)) >= 7,1,0),
                    ifelse(sum(tail(bad, 5)) >= 9,1,0),
                    ifelse(sum(tail(bad, 6)) >= 11,1,0)))
  1:(length(bad)-bad_tail)
})
pitchers_adjusted_1 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

foo <- split(pitchers_adjusted_1, f = pitchers_adjusted_1$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_fWAR <= -1.5, 1, 0) 
  bad_tail <- sum(c(ifelse(sum(tail(bad, 2)) >= 2,1,0),
                    ifelse(sum(tail(bad, 3)) >= 3,1,0),
                    ifelse(sum(tail(bad, 4)) >= 4,1,0),
                    ifelse(sum(tail(bad, 5)) >= 5,1,0),
                    ifelse(sum(tail(bad, 6)) >= 6,1,0)))
  1:(length(bad)-bad_tail)
})
pitchers_adjusted_2 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

foo <- split(pitchers_adjusted_2, f = pitchers_adjusted_2$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_bWAR <= -1.5, 1, 0) 
  bad_tail <- sum(c(ifelse(sum(tail(bad, 2)) >= 2,1,0),
                    ifelse(sum(tail(bad, 3)) >= 3,1,0),
                    ifelse(sum(tail(bad, 4)) >= 4,1,0),
                    ifelse(sum(tail(bad, 5)) >= 5,1,0),
                    ifelse(sum(tail(bad, 6)) >= 6,1,0)))
  1:(length(bad)-bad_tail)
})
pitchers_adjusted_3 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

## remove starts
foo <- split(pitchers_adjusted_3, f = pitchers_adjusted_3$playerID)
bar <- lapply(foo, function(m){
  bad <- ifelse(m$adj_bWAR <= 0, 1, 0) + ifelse(m$adj_fWAR <= 0, 1, 0)
  bad_head <- sum(c(ifelse(sum(head(bad, 1)) == 2,1,0),
                    ifelse(sum(head(bad, 2)) >= 3,1,0),
                    ifelse(sum(head(bad, 3)) >= 5,1,0),
                    ifelse(sum(head(bad, 4)) >= 7,1,0),
                    ifelse(sum(head(bad, 5)) >= 9,1,0),
                    ifelse(sum(head(bad, 6)) >= 11,1,0)))
  if (bad_head < length(bad)) {
    (bad_head + 1):length(bad)
  }
})
pitchers_adjusted_4 <- do.call(rbind, lapply(1:length(bar), function(j){
  foo[[j]][bar[[j]], ]
})) %>% arrange(year)

pitchers_adjusted_4$playerID <- droplevels(pitchers_adjusted_4$playerID)

# taper down average WAR for players with small innings
pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 20, ]$adj_fWAR <- 
  round(pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 20, ]$adj_fWAR/9,2)
pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 20, ]$adj_bWAR <- 
  round(pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 20, ]$adj_bWAR/9,2)	
pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 40 & 
                      pitchers_adjusted_4$mapped_IP > 20 & pitchers_adjusted_4$adj_ERA > 4, ]$adj_fWAR <- 
  round(pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 40 & 
                              pitchers_adjusted_4$mapped_IP > 20 & pitchers_adjusted_4$adj_ERA > 4, ]$adj_fWAR/9, 2)
pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 40 & 
                      pitchers_adjusted_4$mapped_IP > 20 & pitchers_adjusted_4$adj_ERA > 4, ]$adj_bWAR <- 
  round(pitchers_adjusted_4[pitchers_adjusted_4$mapped_IP <= 40 & 
                              pitchers_adjusted_4$mapped_IP > 20 & pitchers_adjusted_4$adj_ERA > 4, ]$adj_bWAR/9, 2)


pitchers_adjusted <- do.call(rbind, mclapply(
  split(pitchers_adjusted_4, f = droplevels(as.factor(pitchers_adjusted_4$playerID))), 
  mc.cores = ncores, FUN = function(xx){
    ## natural cubic spline
    #ns_ERA = lm(adj_ERA ~ ns(year, df=6), data=xx)
    #nn_ERA <- predict(ns_ERA, data.frame("year"= xx$year))
    #ns_SO = lm(adj_SO ~ ns(year, df=6), data=xx)
    #nn_SO <- predict(ns_SO, data.frame("year"= xx$year))
    #ns_bWAR = lm(adj_bWAR ~ ns(year, df=6), data=xx)
    #nn_bWAR <- predict(ns_bWAR, data.frame("year"= xx$year))
    #ns_fWAR = lm(adj_fWAR ~ ns(year, df=6), data=xx)
    #nn_fWAR <- predict(ns_fWAR, data.frame("year"= xx$year))
    
    xx %>% mutate(ERA = round(adj_ERA, 3)) %>% 
      mutate(SO = round(adj_SO)) %>%
      mutate(ebWAR = round(adj_bWAR, 2)) %>%
      mutate(efWAR = round(adj_fWAR, 2))
  })) 

pitchers_adjusted$playerID <- droplevels(pitchers_adjusted$playerID)
pitchers_adjusted <- pitchers_adjusted %>% 
  mutate(ER = round(ERA * IP / 9)) %>%
  mutate(ERA = round(ER / IP *9, 2)) %>% 
  mutate(ERA = ifelse(is.na(ERA), 0, ERA))

rownames(pitchers_adjusted) <- c()

pit_season <- pitchers_adjusted %>% 
  mutate(IP = ceiling(IP)) %>% 
  mutate(ERA = round(ER/IP*9, 2))
colnames(pit_season)[12] <- 'K'
pit_season[which(pit_season$K >= 1.5*pit_season$IP), ]$K <- 
  round(1.5*pit_season[which(pit_season$K >= 1.5*pit_season$IP), ]$IP)

pit_season <- pit_season %>% dplyr::select(-c(adj_bWAR, adj_fWAR, adj_ERA, adj_SO, full_time))

pit_career <- pit_season %>% group_by(playerID) %>% 
  summarise(name = unique(name), 
            playerID = unique(playerID), 
            IP = sum(IP), 
            ER = sum(ER), 
            ERA = round(ER/IP*9,2), 
            K = sum(K), 
            ebWAR = sum(ebWAR), 
            efWAR = sum(efWAR)) %>% 
  arrange(desc(ebWAR))

######







