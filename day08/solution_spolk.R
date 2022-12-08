# --- Day 8: Treetop Tree House ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day08")

dat <- readLines("data.txt")
dat <- t(matrix(as.numeric(unlist(str_split(dat, ""))), nrow = length(dat), ncol = nchar(dat[1])))

getVisibility <- function(curRow) {
  #run getTree function across rows
  sapply(curRow, function(curTree, curRow) {
    #find which trees are blocking my view
    tallerTrees <- curTree <= curRow 
    
    #find trees to the right and left of the current tree
    treesR <- if (iTree != 1) which(1:nrow(dat) < iTree) else 0
    treesL <- if (iTree != nrow(dat)) which(1:nrow(dat) > iTree) else 0
    
    #subtract from visibility score if there's a tree blocking from the right and/or the left
    if (sum(treesR) != 0) {if (sum(tallerTrees[treesR]) > 0) trees[i, iTree] <<- trees[i, iTree] - 1}
    if (sum(treesL) != 0) {if (sum(tallerTrees[treesL]) > 0) trees[i, iTree] <<- trees[i, iTree] - 1}
    
    #update tree index
    iTree <<- iTree + 1
  }, 
  curRow = curRow)
  
  #update indices
  i <<- i + 1
  iTree <<- 1
  
  #get output
  return(trees)
}

#create tree matrix fully visible
trees <- matrix(rep(4, nrow(dat)*ncol(dat)), nrow = nrow(dat))

#initialize indices and get rows across rows
i <- 1; iTree <- 1; invisible(apply(dat, 1, getVisibility))

#transpose tree matrix, reinitialize indices, get rows across columns
trees <- t(trees)
i <- 1; iTree <- 1; invisible(apply(t(dat), 1, getVisibility))

#get answer
sum(trees != 0)

# --- Part Two ---

getVisibilityScore <- function(curRow) {
  #run getTreeScore function across rows
  sapply(curRow, function(curTree, curRow) {
    #find which trees are blocking my view
    tallerTrees <- curTree <= curRow 
    
    #find trees to the right and left of the current tree
    treesR <- if (iTree != 1) which(1:nrow(dat) < iTree) else 0
    treesL <- if (iTree != nrow(dat)) which(1:nrow(dat) > iTree) else 0
    
    #find blocking tree to the right and left, distance to current tree, update score
    if (sum(treesR) != 0) {
      anyTallerTrees <- treesR[which(tallerTrees[treesR] == T)]
      numTrees <- ifelse(length(anyTallerTrees) > 0, abs(iTree - rev(anyTallerTrees)[1]), length(tallerTrees[treesR]))
      trees[i, iTree] <<- trees[i, iTree] * numTrees
    } else {
      #edge trees = 0
      trees[i, iTree] <<- 0
    }
    if (sum(treesL) != 0) {
      anyTallerTrees <- treesL[which(tallerTrees[treesL] == T)]
      numTrees <- ifelse(length(anyTallerTrees) > 0, abs(iTree - anyTallerTrees[1]), length(tallerTrees[treesL]))
      trees[i, iTree] <<- trees[i, iTree] * numTrees
    } else {
      #edge trees = 0
      trees[i, iTree] <<- 0
    }
    
    #update tree index
    iTree <<- iTree + 1
  }, 
  curRow = curRow)
  
  #update indices
  i <<- i + 1
  iTree <<- 1
  
  #get output
  return(trees)
}

#create tree matrix, scores = 1
trees <- matrix(rep(1, nrow(dat)*ncol(dat)), nrow = nrow(dat))

#initialize indices and get rows across rows
i <- 1; iTree <- 1; invisible(apply(dat, 1, getVisibilityScore))

#transpose tree matrix, reinitialize indices, get rows across columns
trees <- t(trees)
i <- 1; iTree <- 1; invisible(apply(t(dat), 1, getVisibilityScore))

#get answer
max(trees)
