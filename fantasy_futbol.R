# load packages
library(fantasypl)
library(tidyverse)
library(lpSolve)

# pull data in from fantasypl API
df <- player_points() %>%
  select(first_name, second_name, now_cost, points_per_game, pos, team) %>%
  mutate(now_cost = now_cost / 10)

# objective
obj = df$points_per_game

# constraints
con <- rbind(t(model.matrix(~ pos + 0, df)),        # position
             t(model.matrix(~ team + 0, df)),       # team
             rep(1, nrow(df)),                      # total players
             df$now_cost)                           # total cost

# direction
dir <- c("==","==","==","==",                       # 2 gk, 5 def, 5 mid, 3 fwd
        rep("<=", 20),                              # no more than 3 from same team
        "==",                                       # must pick 15 players
        "<=")                                       # total cost le 100

# right hand side
rhs <- c(2, 5, 5, 3,                                # 2 gk, 5 def, 5 mid, 3 fwd
        rep(3, 20),                                 # no more than 3 from same team
        15,                                         # must pick 15 players
        100)                                        # total cost le 100

# select team
result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)

# view solution
solution <- cbind(df, selected = result$solution) %>%
  filter(selected == 1)

# but only 11 players can play at a time...
# so one strategy might be to buy the four cheapest players 
# and then select the strongest 11 i can...hmm...
con_mod <- rbind(t(model.matrix(~ pos + 0, df)), 
             t(model.matrix(~ team + 0, df)), 
             rep(1, nrow(df)), 
             df$now_cost)

dir_mod = c("==","==","==","==", 
        rep("<=", 20),
        "==",
        "<=")

rhs_mod = c(1, 3, 5, 2, 
        rep(3, 20),
        11, 
        84)

result_mod <- lp("max", obj, con_mod, dir_mod, rhs_mod, all.bin = TRUE)

solution_mod <- cbind(df, selected = result_mod$solution) %>%
  filter(selected == 1)