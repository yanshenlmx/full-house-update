rm(list=ls())
library(tidyverse)
library(orderstats)
library(Pareto)
library(doParallel)
library(splines)
library(retrosheet)
library(kableExtra)
ncores <- detectCores() - 1
new_season <- 2024L
years <- 1871:new_season

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

source_python('scraping.py')

## new season
new_season <- 2024L

## you may run the following codes two or three times in case 
## the scrapping process is interrupted by the ads on the web. 
bbref_bat <- raw_bbref_bat(new_season)
bbref_pit <- raw_bbref_pit(new_season)
colnames(bbref_bat)[31] <- 'playerID'
colnames(bbref_pit)[36] <- 'playerID'

## get player ID from new season
bat_ID_new <- bbref_bat[-nrow(bbref_bat),]$playerID
pit_ID_new <- bbref_pit[-nrow(bbref_pit),]$playerID
ID_new <- union(bat_ID_new, pit_ID_new)

## get playerID from previous seasons
load('previous_data.RData')
ID_previous <- union(previous_bat_bbref$`Name-additional`, 
                     previous_pit_bbref$`Name-additional`)

ID_diff <- setdiff(ID_new, ID_previous)

## get the players' information
## you may run the following codes two or three times in case 
## the scrapping process is interrupted by the ads on the web. 
position <- c()
for (xx in ID_diff) {
  m <- raw_bioinfo(xx)
  position <- c(position, list(m))
}


## clean the datasets
info_all <- do.call(rbind, mclapply(1:length(position), mc.cores = ncores, FUN = function(xx){
  m <- position[[xx]]
  index <- which(grepl('Full Name: ', unlist(m), fixed = TRUE))
  data.frame(playerID = ID_diff[xx], 
             player_name = m[1,1],
             Full_name = gsub("^.{0,11}", "", m[index, 1]),
             position = gsub("^.{0,4}", "", m[3,1]), 
             Bats = gsub("^.{0,6}", "", m[5,1]), 
             Throws = gsub("^.{0,9}", "", m[7,1]))
}))

MLB_info <- rbind(previous_MLB_info, info_all %>% select(-player_name))

MLB_info_longer <- rbind(previous_MLB_info_longer, info_all %>% 
  pivot_longer(!c(playerID, player_name), names_to = "bio_header", values_to = "bio_details"))

MLB_info_longer <- MLB_info_longer %>% mutate(bio_header = paste(bio_header, ':', sep = ''))





