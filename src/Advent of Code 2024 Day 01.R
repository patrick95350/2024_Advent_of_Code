### Advent of Code - 2024
#     Day  1
#
#     By Patrick Rogers, California Research Bureau
#       December 2024

# Load Packages
library(here)

# Inits
setwd(here::here())

# Part 1 ####

# Read in Data
data <-  read.fwf(file = file.path("data", "day_01_01.txt"), widths = c(5,3,5))
data <- data[,-2]

# Sort Columns Independently
data[,1] <- sort(data[,1])
data[,2] <- sort(data[,2])

# Get Distances
data$distance <- abs(data[,2] - data[,1])

# Sum Distances
sum(data$distance)

# Part 2 ####

# sim_score function
sim_score <- function(value){
  count <- sum(data[,2] %in% value)
  score <- value * count
  return(score)
}

# Calculate sim score for each item in List 1
data$sim_score <- sapply(as.list(data[,1]), sim_score)

# Sum sim scores
sum(data$sim_score)

# EOF ####