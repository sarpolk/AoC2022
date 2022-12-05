# --- Day 3: Rucksack Reorganization ---

setwd("/Volumes/FB-LIP/Plasticity/STUDIES/Sarah/Misc/AdventOfCode2022/day03")

dat <- readLines("data.txt")
priorities <- c(letters, toupper(letters))

getPriorities <- function(x){
  y <- vector(mode = "list", length = 2)
  y[[1]] <- unlist(strsplit(substr(x, 1, (nchar(x) / 2)), ""))
  y[[2]] <- unlist(strsplit(substr(x, (nchar(x) / 2 + 1), nchar(x)), ""))
  
  matches <- y[[1]][which(y[[1]] %in% y[[2]])]
  return(which(priorities %in% matches))
}

sum(unlist(lapply(dat, getPriorities)))

# --- Part Two ---
library(tidyverse)

dat <- data.frame(contents = dat)
dat <- dat %>% 
  mutate(group = sort(rep(seq(1, nrow(dat) / 3, 1), 3)))

dat <- split(dat$contents, dat$group)

getBadges <- function(x){
  y <- vector(mode = "list", length = 3)
  y[[1]] <- unlist(strsplit(x[1], ""))
  y[[2]] <- unlist(strsplit(x[2], ""))
  y[[3]] <- unlist(strsplit(x[3], ""))
  
  matches <- y[[3]][which(y[[3]] %in% y[[1]][which(y[[1]] %in% y[[2]])])]
  return(which(priorities %in% matches))
}

sum(unlist(lapply(dat, getBadges)))

       