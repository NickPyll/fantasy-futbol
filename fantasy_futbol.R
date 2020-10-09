#### Set up environment ----

# load packages
# library(devtools)
# install_github("wiscostret/fplscrapR")
# install_github("jogall/fantasypl")
library(fantasypl)
library(tidyverse)
library(lpSolve)

#### Load data ----

# pull data in from fantasypl API
data_2020 <- 
  player_points() %>%
  select(first_name, second_name, now_cost, points_per_game, pos, team) %>%
  mutate(now_cost = now_cost / 10)

`%notin%` = Negate(`%in%`)

df <- 
  data_2020 #%>%
  # filter(second_name %notin% c('Salah'))

#### Scenario 1 ----
# Select best team of 15 given cost and team constraints.

# # objective
# obj = df$points_per_game
# 
# # constraints
# con <- rbind(t(model.matrix(~ pos + 0, df)),        # position
#              t(model.matrix(~ team + 0, df)),       # team
#              rep(1, nrow(df)),                      # total players
#              df$now_cost)                           # total cost
# 
# # direction
# dir <- c("==","==","==","==",                       # 2 gk, 5 def, 5 mid, 3 fwd
#         rep("<=", 20),                              # no more than 3 from same team
#         "==",                                       # must pick 15 players
#         "<=")                                       # total cost le 100
# 
# # right hand side
# rhs <- c(2, 5, 5, 3,                                # 2 gk, 5 def, 5 mid, 3 fwd
#         rep(3, 20),                                 # no more than 3 from same team
#         15,                                         # must pick 15 players
#         100)                                        # total cost le 100
# 
# # select team
# result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)
# 
# # view solution
# solution <- cbind(df, selected = result$solution) %>%
#   filter(selected == 1)

#### Scenario 2 ----
# Select best team of 11 given cost and team constraints. 
# Essentially purchase 4 bench players whose stats we don't care about.

# # objective
# obj_mod = df$points_per_game
# 
# # constraints
# con_mod <- rbind(t(model.matrix(~ pos + 0, df)),        # position
#              t(model.matrix(~ team + 0, df)),       # team
#              rep(1, nrow(df)),                      # total players
#              df$now_cost)                           # total cost
#
# # direction
# dir_mod <- c("==","==","==","==",                       # 2 gk, 5 def, 5 mid, 3 fwd
#         rep("<=", 20),                              # no more than 3 from same team
#         "==",                                       # must pick 15 players
#         "<=")                                       # total cost le 100
#
# # modify right side to change position limits
# rhs_mod <- c(1, 3, 5, 2, 
#         rep(3, 20),
#         11, 
#         84)
# 
# # select team
# result_mod <- lp("max", obj_mod, con_mod, dir_mod, rhs_mod, all.bin = TRUE)
# 
# # view solution
# solution_mod <- cbind(df, selected = result_mod$solution) %>%
#   filter(selected == 1)

#### Scenario 3 ----
# Select best team of 15 players, no cost or team constraints.

# objective
obj_draft = df$points_per_game

# constraints
con_draft <- rbind(t(model.matrix(~ pos + 0, df)),  # position
             rep(1, nrow(df)))                      # total players

# direction
dir_draft <- c("==","==","==","==",                 # 2 gk, 5 def, 5 mid, 3 fwd
        "==")                                       # must pick 15 players

# modify right side to change position limits
rhs_draft <- c(2, 5, 5, 3,
        15)

# select team
result_draft <- lp("max", obj_draft, con_draft, dir_draft, rhs_draft, all.bin = TRUE)

# view solution
solution_draft <- cbind(df, selected = result_draft$solution) %>%
  filter(selected == 1) %>%
  arrange(desc(points_per_game))
