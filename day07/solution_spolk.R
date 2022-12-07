# --- Day 7: No Space Left On Device ---

library(tidyverse)

setwd("/Users/spolk/Desktop/AdventOfCode2022/day07")

dat <- readLines("data.txt")

# initialize list
dirList <- vector(mode = "list", length = 1)
dirList[[1]][c("path", "size")] <- c("//", "0")

# initialize current directory
curDir <- vector(mode = "list", length = 1)
curDir[[1]][c("path", "size")] <- c("", "0")
  
for (x in dat[1:length(dat)]) {
  cmd <- substr(x, 1, 4)
  
  if (cmd == "$ cd"){
    dirName <- substr(x, 6, nchar(x))
    if (dirName == "..") {
      # go up
      curPath <- sub("/[^/]+$", "", curDir[[1]]["path"])
      curDir[[1]] <- dirList[[which(rapply(dirList, function(x) head(x, 1)) == curPath)]]
      
    } else {
      # go down
      curPath <- paste0(curDir[[1]]["path"], "/", dirName)
      curDir[[1]] <- dirList[[which(rapply(dirList, function(x) head(x, 1)) == curPath)]]
      
    }
    
  } else if (cmd == "$ ls") {
    # don't do anything
    
  } else if (cmd == "dir ") { 
    # assuming i never need to check again if a specific dir exists
    dirName <- substr(x, 5, nchar(x))
    dirList <- append(dirList, vector(mode = "list", length = 1))
    curLen <- length(dirList)
    dirList[[curLen]]["path"] <- paste0(curDir[[1]]["path"], "/", dirName)
    dirList[[curLen]]["size"] <- 0
    
  } else {
    txtSz <- gsub( " .*$", "", x)
    curDirIdx <- match(curDir, dirList)
    curSz <- as.numeric(dirList[[curDirIdx]]["size"])
    dirList[[curDirIdx]]["size"] <- curSz + as.numeric(txtSz)
    
    path <- dirList[[curDirIdx]]["path"]
    while (path[[1]] != "//") {
      tmpDir <- sub("/[^/]+$", "", path)
      tmpDirIdx <- which(rapply(dirList, function(x) head(x, 1)) == tmpDir)
      dirList[[tmpDirIdx]]["size"] <- as.numeric(dirList[[tmpDirIdx]]["size"]) + as.numeric(txtSz)
      path <- tmpDir
    }
    curDir[[1]] <- dirList[[curDirIdx]]
  }
}

df <- data.frame(size = as.numeric(unlist(dirList)[which(names(unlist(dirList)) == "size")]))
df %>% 
  filter(size <= 100000) %>% 
  summarise(sum = sum(size))

# --- Part Two ---

df <- data.frame(size = as.numeric(unlist(dirList)[which(names(unlist(dirList)) == "size")]))
totalUsed <- df %>% 
  summarise(total = max(size))
free <- 70000000 - totalUsed
need <- 30000000 - free

df %>% 
  filter(size > as.numeric(need)) %>% 
  filter(size == min(size))
