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

df$foot <- df$shotType
df$foot[!df$foot == "Head"] <- 1
df$foot[df$foot == "Head"] <- 0

df$shotType[!df$shotType == "Head"] <- 0
df$shotType[df$shotType == "Head"] <- 1

df <- df %>%
rename(head = shotType) %>%
mutate(NewX = X * 120) %>%
mutate(NewY = Y * 80) %>%
mutate(distance = custom_distance(NewX, NewY)) %>%
mutate(angle = custom_angle(NewX, NewY))

data_pred <- predict(xg_fit, df, type = "prob") %>% 
  bind_cols(df) %>%
  rename("xGoals" = ".pred_1")
}

data <- get_player_shots(556)
dataframe <- xGModel(df = data)
dataframe <- as.data.frame(dataframe)

#' Plotting side-by-side

dataframe <- dataframe %>%
filter(year == 2020)

p1 <- ggplot() +
annotate_pitch(dimensions = pitch_statsbomb, fill = "black", colour = "white") +
theme_pitch() +
geom_point(data = dataframe, aes(NewX, NewY, colour = result, size = xG)) +
labs(title = "Understat Model") +
theme(plot.title = element_text(hjust = 0.5, size = 15)) +
coord_flip() 

p2 <- ggplot() +
annotate_pitch(dimensions = pitch_statsbomb, fill = "black", colour = "white") +
theme_pitch() +
geom_point(data = dataframe, aes(NewX, NewY, colour = result, size = xGoals)) +
labs(title = "Harsh's Model") +
theme(plot.title = element_text(hjust = 0.5, size = 15)) +
coord_flip() 

fig <- ggarrange(p1, p2,
ncol = 2, nrow = 1) +
labs(title = "Marcus Rashford") +
theme(plot.title = element_text(colour = "black", size = 20))

fig