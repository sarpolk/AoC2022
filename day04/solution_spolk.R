# --- Day 4: Camp Cleanup ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day04")

dat <- data.frame(assignments = readLines("data.txt"))
dat %>% 
  separate(assignments, c("min1", "max1", "min2", "max2"), "[,-]") %>% 
  mutate_at(vars(min1, max1, min2, max2), as.numeric) %>% 
  mutate(fullOverlap = case_when(min1 <= min2 & max1 >= max2 ~ 1,
                                 min2 <= min1 & max2 >= max1 ~ 1,
                                 T ~ 0)) %>% 
  summarise(ans = sum(fullOverlap))

# --- Part Two ---

dat <- dat %>% 
  separate(assignments, c("min1", "max1", "min2", "max2"), "[,-]")
datList <- split(dat, row.names(dat))

getOverlap <- function(x) {
  seq1 <- seq(as.numeric(x[,"min1"]), as.numeric(x[,"max1"]), 1)
  seq2 <- seq(as.numeric(x[,"min2"]), as.numeric(x[,"max2"]), 1)
  
  return(as.numeric(length(which(seq1 %in% seq2)) + length(which(seq2 %in% seq1)) != 0))
}

sum(unlist(lapply(datList, getOverlap)))
