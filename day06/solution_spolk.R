# --- Day 6: Tuning Trouble ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day06")

dat <- data.frame(x = readLines("data.txt"))

for (i in 1:nchar(dat)) {
  curChunk <- unique(strsplit(substr(dat, i, i+3), "")[[1]])
  if (length(curChunk) == 4){
    print(i+3)
    break
  }
}

# --- Part Two ---

for (i in 1:nchar(dat)) {
  curChunk <- unique(strsplit(substr(dat, i, i+13), "")[[1]])
  if (length(curChunk) == 14){
    print(i+13)
    break
  }
}
