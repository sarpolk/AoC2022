# --- Day 9: Rope Bridge ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day09")

# get data
dat <- read.csv("data.txt", header = F) %>% separate(V1, c("dir", "num"), " ")

# create function
moveKnots <- function(x, maxKnot) {
  headPos <<- headPos
  tailPos <<- tailPos
  tailHistory <<- tailHistory
  
  directions <<- directions
  rowCol <- as.numeric(directions[[as.character(x["dir"])]][1])
  addSub <- as.numeric(directions[[as.character(x["dir"])]][2])
  
  i <- as.numeric(x["num"])
  while (i != 0) {
    # move head
    headPos[rowCol] <<- headPos[rowCol] + addSub
    
    frontKnot <- headPos
    for (knot in 1:maxKnot) {

      # move knot diagonally
      if (frontKnot[1] != tailPos[knot,][1] & frontKnot[2] != tailPos[knot,][2] & sum(abs(frontKnot - tailPos[knot,])) > 2) {
        if (tailPos[knot,][1] < frontKnot[1]){tailPos[knot,][1] <<- tailPos[knot,][1] + 1}
        if (tailPos[knot,][1] > frontKnot[1]){tailPos[knot,][1] <<- tailPos[knot,][1] - 1}
        if (tailPos[knot,][2] < frontKnot[2]){tailPos[knot,][2] <<- tailPos[knot,][2] + 1}
        if (tailPos[knot,][2] > frontKnot[2]){tailPos[knot,][2] <<- tailPos[knot,][2] - 1}
        
      # move knot sideways
      } else if (abs(frontKnot[1] - tailPos[knot,][1]) > 1 | abs(frontKnot[2] - tailPos[knot,][2]) > 1) {
        if (tailPos[knot,][1] < frontKnot[1]){tailPos[knot,][1] <<- tailPos[knot,][1] + 1}
        if (tailPos[knot,][1] > frontKnot[1]){tailPos[knot,][1] <<- tailPos[knot,][1] - 1}
        if (tailPos[knot,][2] < frontKnot[2]){tailPos[knot,][2] <<- tailPos[knot,][2] + 1}
        if (tailPos[knot,][2] > frontKnot[2]){tailPos[knot,][2] <<- tailPos[knot,][2] - 1}
      }
      
      if (knot == maxKnot) {
        # update tail history 
        curValue <- tailHistory[as.character(tailPos[knot,][1]), as.character(tailPos[knot,][2])]
        curValue <- ifelse(class(curValue) != "NULL", ifelse(!is.na(curValue), curValue, 0), 0)
        tailHistory[as.character(tailPos[knot,][1]), as.character(tailPos[knot,][2])] <<- curValue + 1
      }
      
      frontKnot <- tailPos[knot,]
    }
    
    i <- i - 1
  }
}

# set directions
directions <- list(U = c(1, 1), D = c(1, -1), R = c(2, 1), L = c(2, -1))

# specify number of tail knots
knots <- 1

# position of head and tail, row x col
headPos <- c(1, 1)
tailPos <- matrix(rep(1, 2*knots), nrow = knots)

# initialize tail histories as data.frame
tailHistory <- data.frame(1)
row.names(tailHistory)[1] <- "1"
names(tailHistory)[1] <- "1"

# run function 
invisible(apply(dat, 1, moveKnots, maxKnot = knots))

# get answer
sum(!is.na(tailHistory))

# --- Part Two ---

# specify number of tail knots
knots <- 9

# position of head and tail, row x col
headPos <- c(1, 1)
tailPos <- matrix(rep(1, 2*knots), nrow = knots)

# initialize tail histories as data.frame
tailHistory <- data.frame(1)
row.names(tailHistory)[1] <- "1"
names(tailHistory)[1] <- "1"

# run function 
invisible(apply(dat, 1, moveKnots, maxKnot = knots))

# get answer
sum(!is.na(tailHistory))

