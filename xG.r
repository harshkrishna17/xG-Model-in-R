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
  
  angle <- atan(slope)
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

#' Modelling 

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

# Testing on Understat dataset

df <- get_player_shots(556)

df <- df %>%
mutate(NewX = X * 120) %>%
mutate(NewY = Y * 80) %>%
mutate(distance = custom_distance(NewX, NewY)) %>%
mutate(angle = custom_angle(NewX, NewY)) %>%
mutate(type = ifelse(shotType == "Head", distance, distance + ratio))

data_pred <- predict(xg_fit, df, type = "prob") %>% 
  bind_cols(df) %>%
  rename("xGoals" = ".pred_1")

xg1 <- data_pred$xGoals
xg <- data_pred$xG

rsq <- function (x, y) {cor(x, y) ^ 2}
rsq(xg, xg1)

# Plotting side-by-side

data_pred <- data_pred %>%
filter(year == 2020)

p1 <- ggplot() +
annotate_pitch(dimensions = pitch_statsbomb, fill = "black", colour = "white") +
theme_pitch() +
geom_point(data = data_pred, aes(NewX, NewY, colour = result, size = xG)) +
labs(title = "Understat Model") +
theme(plot.title = element_text(hjust = 0.5, size = 15)) +
coord_flip() 

p2 <- ggplot() +
annotate_pitch(dimensions = pitch_statsbomb, fill = "black", colour = "white") +
theme_pitch() +
geom_point(data = data_pred, aes(NewX, NewY, colour = result, size = xGoals)) +
labs(title = "Harsh's Model") +
theme(plot.title = element_text(hjust = 0.5, size = 15)) +
coord_flip() 

fig <- ggarrange(p1, p2,
ncol = 2, nrow = 1) +
labs(title = "Marcus Rashford") +
theme(plot.title = element_text(colour = "black", size = 20))

fig