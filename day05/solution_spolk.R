# --- Day 5: Supply Stacks ---

setwd("/Users/spolk/Desktop/AdventOfCode2022/day05")

dat <- data.frame(x = readLines("data.txt"))
breakLine <- which(dat$x == "")

moveInstr <- data.frame(x = dat[(breakLine + 1):nrow(dat),])
moveInstr <- moveInstr %>% 
  filter(grepl("move", x)) %>% 
  separate(x, c(NA, "move", NA, "from", NA, "to"), sep = " ")

configuration <- dat[1:(breakLine - 2),]
configuration <- as.list(configuration)

getConfig <- function(x){
  splitConfig <- sapply(seq(from=1, to=nchar(x), by=4), function(i) substr(x, i, i+3))
  substr(splitConfig, 1, 3)
}

config <- lapply(configuration, getConfig)
crateSlots <- length(config[[1]])
cratePos <- length(configuration)
config <- data.frame(crate = unlist(config),
                     slot = 1:crateSlots,
                     pos = rev(sort(rep(1:crateSlots, cratePos))))
config <- config %>% 
  filter(crate != "   ") %>%
  arrange(slot)
config2 <- config

reconfig <- function() {
  maxPosFrom <- max((config[which(config$slot == from),])$pos)
  if (nrow(config %>% filter(slot == to))  == 0){
    maxPosTo <- 0
  } else{
    maxPosTo <- max((config[which(config$slot == to),])$pos)
  }
  curCrate <- config[which(config$slot == from & config$pos == maxPosFrom),][,1]
  
  config[which(config$slot == from & config$pos == maxPosFrom),] <- data.frame(curCrate, as.numeric(to), as.numeric(maxPosTo) + 1)
}

for (curInstr in 1:nrow(moveInstr)) {
  n <- as.numeric(moveInstr[curInstr, 1])
  from <- as.numeric(moveInstr[curInstr, 2])
  to <- as.numeric(moveInstr[curInstr, 3])
  
  x <- 1
  while (x <= n) {
    reconfig
    x <- x + 1
  }
}

ans <- config %>% 
  group_by(slot) %>% 
  filter(pos == max(pos)) %>% 
  arrange(slot)

paste0(gsub("\\[|\\]", "", ans$crate), collapse = "")

# --- Part Two ---

for (i in 1:nrow(moveInstr)) {
  curInstr <- moveInstr[i,]
  n <- as.numeric(curInstr[, 1])
  from <- as.numeric(curInstr[, 2])
  to <- as.numeric(curInstr[, 3])
  
  nseq <- rev(1:n)
  if (nrow(config2 %>% filter(slot == to)) == 0){
    maxPosTo <- 0
  } else{
    maxPosTo <- max((config2[which(config2$slot == to),])$pos)
  }
  for (x in 1:n) {
    maxPosFrom <- max((config2[which(config2$slot == from),])$pos)
    curCrate <- config2[which(config2$slot == from & config2$pos == maxPosFrom),][,1]
    
    config2[which(config2$slot == from & config2$pos == maxPosFrom),] <- data.frame(curCrate, 
                                                                                 as.numeric(to), 
                                                                                 as.numeric(maxPosTo) + nseq[x])
  }
}

ans <- config2 %>% 
  group_by(slot) %>% 
  filter(pos == max(pos)) %>% 
  arrange(slot)

paste0(gsub("\\[|\\]", "", ans$crate), collapse = "")
