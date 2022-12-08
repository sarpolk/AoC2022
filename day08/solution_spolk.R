# --- Day 8: Treetop Tree House ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day08")

dat <- readLines("data.txt")
dat <- t(matrix(as.numeric(unlist(str_split(dat, ""))), nrow = length(dat), ncol = nchar(dat[1])))

getRows <- function(curRow) {
  sapply(curRow, getTree, curRow = curRow)
  i <<- i + 1
  iTree <<- 1
  return(trees)
}

getTree <- function(curTree, curRow) {
  tallerTrees <- curTree <= curRow 
  
  treesR <- if (iTree != 1) which(1:nrow(dat) < iTree) else 0
  treesL <- if (iTree != nrow(dat)) which(1:nrow(dat) > iTree) else 0
  
  if (sum(treesR) != 0) {if (sum(tallerTrees[treesR]) > 0) trees[i, iTree] <<- trees[i, iTree] - 1}
  if (sum(treesL) != 0) {if (sum(tallerTrees[treesL]) > 0) trees[i, iTree] <<- trees[i, iTree] - 1}
  
  iTree <<- iTree + 1
  
}

#create tree matrix fully visible
trees <- matrix(rep(4, nrow(dat)*ncol(dat)), nrow = nrow(dat))

#initialize indices and get rows across rows
i <- 1; iTree <- 1; invisible(apply(dat, 1, getRows))

#transpose tree matrix, reinitialize indices, get rows across columns
trees <- t(trees)
i <- 1; iTree <- 1; invisible(apply(t(dat), 1, getRows))

#get answer
sum(trees != 0)

# --- Part Two ---