#' Importing Libraries

library(tidymodels)
library(tidyverse)
library(ggsoccer)
library(understatr)
library(ggpubr)

#' Function

xGModel <- function(df) {

setwd("C:/Users/harsh_1mwi2o4/Downloads")
data <- read.csv("shotdata.csv")

custom_distance <- function(NewX, NewY) {
    x_dist <- (120 - NewX)
    y_dist <- (40 - NewY)

    dist <- sqrt(x_dist*x_dist + y_dist*y_dist)
}

custom_angle <- function(NewX, NewY) {
    x_dist <- (120 - NewX)
    y_dist <- (40 - NewY)
    slope <- y_dist/x_dist
  
  angle <- atan((8*x_dist)/(x_dist*x_dist + y_dist*y_dist - (8/2)*(8/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

data$dist <- custom_distance(data$NewX, data$NewY)

data1 <- data %>%
filter(shotType == "Head")
data2 <- data %>%
filter(!shotType == "Head")

head <- mean(data1$dist)
foot <- mean(data2$dist)
ratio <- foot/head

data <- data %>%
mutate(distance = custom_distance(NewX, NewY)) %>%
mutate(angle = custom_angle(NewX, NewY)) %>%
mutate(type = ifelse(shotType == "Head", distance, distance + ratio))

data$is_goal <- ifelse(data$result == "Goal", 1,0)
data$is_goal <- factor(data$is_goal, levels = c("1", "0"))

set.seed(1234) 

train_test_split <- initial_split(data = data, prop = 0.80) 
train_data <- train_test_split %>% 
training() 
test_data  <- train_test_split %>% 
testing()

xg_recipe <- 
  recipe(is_goal ~ distance + angle + type + NewX + NewY, data = train_data) %>% 
  update_role(NewX, NewY, new_role = "ID") 

model <- logistic_reg() %>% 
  set_engine("glm")

xg_wflow <- 
  workflow() %>% 
  add_model(model) %>% 
  add_recipe(xg_recipe)

xg_fit <- 
  xg_wflow %>% 
  fit(data = train_data)

df <- df %>%
mutate(NewX = X * 120) %>%
mutate(NewY = Y * 80) %>%
mutate(distance = custom_distance(NewX, NewY)) %>%
mutate(angle = custom_angle(NewX, NewY)) %>%
mutate(type = ifelse(shotType == "Head", distance, distance + ratio))

data_pred <- predict(xg_fit, df, type = "prob") %>% 
  bind_cols(df) %>%
  rename("xGoals" = ".pred_1")
}

df <- get_player_shots(553)
dataframe <- xGModel(df)