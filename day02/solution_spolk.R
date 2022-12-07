# --- Day 2: Rock Paper Scissors ---

library(tidyverse)
setwd("/Users/spolk/Desktop/AdventOfCode2022/day02")

dat <- read.csv("data.txt", header = F)
dat <- dat %>% 
  separate(V1, c("opp", "me"), " ")

outcome <- data.frame(X = c(3, 0, 6),
                      Y = c(6, 3, 0),
                      Z = c(0, 6, 3))

dat %>% 
  mutate(opp = case_when(opp == "A" ~ 1,
                         opp == "B" ~ 2,
                         opp == "C" ~ 3),
         me = case_when(me == "X" ~ 1,
                        me == "Y" ~ 2,
                        me == "Z" ~ 3)) %>% 
  rowwise() %>% 
  mutate(pointsOutcome = outcome[opp, me],
         totalPoints = me + pointsOutcome) %>% 
  ungroup() %>% 
  summarise(totalPoints = sum(totalPoints))

# --- Part Two ---

dat %>% 
  mutate(opp = case_when(opp == "A" ~ 1,
                         opp == "B" ~ 2,
                         opp == "C" ~ 3),
         me = case_when(me == "X" ~ 0,
                        me == "Y" ~ 3,
                        me == "Z" ~ 6),
         chosen = case_when(me == 0 & opp != 1 ~ opp - 1,
                            me == 0 & opp == 1 ~ 3,
                            me == 3 ~ opp,
                            me == 6 & opp != 3 ~ opp + 1,
                            me == 6 & opp == 3 ~ 1)) %>% 
  mutate(totalPoints = me + chosen) %>% 
  summarise(totalPoints = sum(totalPoints))
 