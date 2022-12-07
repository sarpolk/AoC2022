# --- Day 1: Calorie Counting ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day01")

dat <- readLines("data.txt")
blanks <- which(dat == "")

elves <- vector(mode = "list", length = length(blanks))
start <- 0
for (i in 1:length(blanks)){
  elves[[i]] <- sum(as.numeric(dat[(start+1):(blanks[i]-1)]))
  start <- blanks[i]
}

max(unlist(elves))

# --- Part Two ---

sum(sort(unlist(elves))[length(elves):(length(elves) - 2)])

