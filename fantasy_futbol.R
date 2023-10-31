#### Set up environment ----

# load packages
# library(devtools)
# install_github("wiscostret/fplscrapR") # dependency for fantasypl 
# install_github("jogall/fantasypl")
library(fantasypl)
library(tidyverse)
library(lpSolve)

#### Load data ----

# pull data in from fantasypl API
df <- 
  player_points() %>%
  select(first_name, second_name, now_cost, points_per_game, pos, team) %>%
  mutate(now_cost = now_cost / 10)

`%notin%` = Negate(`%in%`)

#### Scenario 1 ----
# Select best team of 15 given cost and team constraints.

# objective
obj = df$points_per_game

# constraints
con <- rbind(t(model.matrix(~ pos + 0, df)),        # position
             t(model.matrix(~ team + 0, df)),       # team
             rep(1, nrow(df)),                      # total players
             df$now_cost)                           # total cost

# direction
dir <- c("==","==","==","==",                       # 2 gk, 5 def, 5 mid, 3 fwd
         rep("<=", 20),                             # no more than 3 from same team
         "==",                                      # must pick 15 players
         "<=")                                      # total cost le 100

# right hand side
rhs <- c(2, 5, 5, 3,                                # 2 gk, 5 def, 5 mid, 3 fwd
         rep(3, 20),                                # no more than 3 from same team
         15,                                        # must pick 15 players
         100)                                       # total cost le 100

# select team
result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)

# view solution
solution_scenario1 <- cbind(df, selected = result$solution) %>%
  filter(selected == 1)

#### Scenario 2 ----
# Select best team of 11 given cost and team constraints.
# Essentially purchase 4 bench players whose stats we don't care about.

# objective
obj = df$points_per_game

# constraints
con <- rbind(t(model.matrix(~ pos + 0, df)),        # position
             t(model.matrix(~ team + 0, df)),       # team
             rep(1, nrow(df)),                      # total players
             df$now_cost)                           # total cost

# direction
dir <- c("==","==","==","==",                       # 2 gk, 5 def, 5 mid, 3 fwd
         rep("<=", 20),                             # no more than 3 from same team
         "==",                                      # must pick 15 players
         "<=")                                      # total cost le 100

# modify right side to change position limits
rhs <- c(1, 3, 5, 2,
         rep(3, 20),
         11,
         84)

# select team
result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)

# view solution
solution_scenario2 <- cbind(df, selected = result$solution) %>%
  filter(selected == 1)

#### Scenario 3 ----
# Select best team of 15 players, no cost or team constraints.

# objective
obj = df$points_per_game

# constraints
con <- rbind(t(model.matrix(~ pos + 0, df)),  # position
             rep(1, nrow(df)))                # total players

# direction
dir <- c("==","==","==","==",                 # 2 gk, 5 def, 5 mid, 3 fwd
         "==")                                # must pick 15 players

# modify right side to change position limits
rhs <- c(2, 5, 5, 3,
         15)

# select team
result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)

# view solution
solution_scenario3 <- cbind(df, selected = result$solution) %>%
  filter(selected == 1) %>%
  arrange(desc(points_per_game))
