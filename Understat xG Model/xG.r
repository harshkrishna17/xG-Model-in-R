#' Importing Libraries

library(tidymodels)
library(tidyverse)
library(ggsoccer)
library(understatr)
library(ggpubr)

#' Loading in Dataset + Minor Changes

setwd("C:/Users/harsh_1mwi2o4/Downloads")
data <- read.csv("shotdata.csv")

#' Calculations

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

data$foot <- data$shotType
data$foot[!data$foot == "Head"] <- 1
data$foot[data$foot == "Head"] <- 0

data$shotType[!data$shotType == "Head"] <- 0
data$shotType[data$shotType == "Head"] <- 1

data <- data %>%
rename(head = shotType) %>%
mutate(distance = custom_distance(NewX, NewY)) %>%
mutate(angle = custom_angle(NewX, NewY))

data$is_goal <- ifelse(data$result == "Goal", 1,0)
data$is_goal <- factor(data$is_goal, levels = c("1", "0"))

#' Modelling 

set.seed(1234) 

train_test_split <- initial_split(data = data, prop = 0.80) 
train_data <- train_test_split %>% 
training() 
test_data  <- train_test_split %>% 
testing()

xg_recipe <- 
  recipe(is_goal ~ distance + angle + foot + head + NewX + NewY, data = train_data) %>% 
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

#' Testing 

data_pred <- predict(xg_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) %>%
  rename("xGoals" = ".pred_1")

xg1 <- data_pred$xGoals
xg <- data_pred$xG

rsq <- function (x, y) {cor(x, y) ^ 2}
rsq(xg, xg1)