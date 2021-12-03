#' Importing Libraries 

library(tidyverse)
library(worldfootballR)

#' Scraping data. The data source is Understat (Opta event data). 
#' The leagues are the Big 5 European Leagues from 2014 to 2021. 
#' Data compiled by Shusruta Nandy. worldfootballR package used for datasets not included in already compiled data. 

setwd("C:/Users/harsh_1mwi2o4/Downloads") 
data1 <- read.csv("fullshots.csv")

data1 <- data1 %>%
filter(!year == 2020)

data1 <- data1[, c("X", "Y", "player", "shotType", "situation", "result")]

# worldfootballR Scraping + Cleaning

df1 <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2020)
df2 <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2021)

df3 <- understat_league_season_shots(league = "EPL", season_start_year = 2020)
df4 <- understat_league_season_shots(league = "EPL", season_start_year = 2021)

df5 <- understat_league_season_shots(league = "La liga", season_start_year = 2020)
df6 <- understat_league_season_shots(league = "La liga", season_start_year = 2021)

df7 <- understat_league_season_shots(league = "Bundesliga", season_start_year = 2020)
df8 <- understat_league_season_shots(league = "Bundesliga", season_start_year = 2021)

df9 <- understat_league_season_shots(league = "Serie A", season_start_year = 2020)
df10 <- understat_league_season_shots(league = "Serie A", season_start_year = 2021)

data2 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)
data2 <- data2[, c("X", "Y", "player", "shotType", "situation", "result")]

#' Joining two datasets

data <- rbind(data1, data2)
data <- data %>%
mutate(NewX = X * 120) %>%
mutate(NewY = Y * 80) %>%
filter(!situation == "Penalty") %>%
filter(!situation == "BlockedShot") %>%
filter(!situation == "OwnGoal")

data <- data[, c("NewX", "NewY", "player", "shotType", "result")]

#' Saving dataset as .csv

write.csv(data, "shotdata.csv", row.names = FALSE)
